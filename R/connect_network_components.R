library(sf)
library(dplyr)
library(igraph)
library(units)
library(lwgeom)
library(rmapshaper)

connect_network_components <- function(lines_sf,
                                       connection_type = "artificial",
                                       new_id_column = "line_id",
                                       clean_network = TRUE,
                                       precision_digits = NULL,
                                       track_memory = TRUE) {
                                       track_memory = FALSE) {

  track_mem <- function(step) {
    if(track_memory) {
      message(sprintf("Memory at %s:", step))
    }
    gc(verbose = track_memory)
  }

  track_mem("start")

  # Basic validation
  if(!"component" %in% names(lines_sf))
    stop("Column 'component' not found in lines_sf")
  if(!"highway" %in% names(lines_sf))
    stop("lines_sf must have a 'highway' column")

  # Determine appropriate precision based on CRS
  if(is.null(precision_digits)) {
    precision_digits <- if(sf::st_is_longlat(lines_sf)) 3 else 2
  }

  # Calculate simplification tolerance based on precision_digits
  hull_tolerance <- if(sf::st_is_longlat(lines_sf)) {
    10^-precision_digits  # For geographic CRS
  } else {
    10^abs(precision_digits)  # For projected CRS
  }

  # Add new ID column if needed
  if(!new_id_column %in% names(lines_sf)) {
    lines_sf[[new_id_column]] <- seq_len(nrow(lines_sf))
  }

  components <- unique(lines_sf$component)
  n_components <- length(components)

  if(n_components <= 1) {
    return(list(
      complete_network = lines_sf,
      artificial_edges = NULL
    ))
  }

  current_crs <- st_crs(lines_sf)

  # Create convex hulls for each component index
  track_mem("before hulls")
  component_hulls <- vector("list", n_components)
  for(i in seq_len(n_components)) {
    comp <- components[i]
    coords <- st_coordinates(lines_sf[lines_sf$component == comp,])[,c("X","Y")]
    hull <- st_convex_hull(st_sfc(st_multipoint(coords), crs = current_crs)) %>%
      st_simplify(dTolerance = hull_tolerance)
    component_hulls[[i]] <- hull
    rm(hull)
    rm(coords)
    track_mem(glue::glue("After calculating convex hull {i}."))
  }
  track_mem("after hulls")

  # Calculate distances and find connections
  distances <- matrix(Inf, n_components, n_components)
  connections <- list()

  track_mem("before distance calculations")
  for(i in 1:(n_components-1)) {
    for(j in (i+1):n_components) {
      nearest_points <- st_nearest_points(component_hulls[[i]], component_hulls[[j]])

      if(st_intersects(component_hulls[[i]], component_hulls[[j]], sparse=FALSE)[1,1]) {
        # Hulls intersect - use single point and zero distance
        connection_line <- st_sfc(st_linestring(rbind(
          st_coordinates(nearest_points)[1,],
          st_coordinates(nearest_points)[1,]
        )), crs = current_crs)

        connections[[paste(i,j)]] <- list(
          from_component = components[i],
          to_component = components[j],
          connection_line = connection_line,
          distance = 0
        )

        distances[i,j] <- distances[j,i] <- 0
      } else {
        # Hulls don't intersect - use actual distance and points
        hull_dist <- as.numeric(st_length(nearest_points))

        connections[[paste(i,j)]] <- list(
          from_component = components[i],
          to_component = components[j],
          connection_line = st_sfc(nearest_points, crs = current_crs),
          distance = hull_dist
        )

        distances[i,j] <- distances[j,i] <- hull_dist
      }
    }
    if(track_memory && i %% 100 == 0) {
      track_mem(sprintf("after %d distance rows", i))
    }
  }
  track_mem("after distances")

  rm(component_hulls)
  gc()
  track_mem("after hull cleanup")

  # Find minimum spanning tree
  track_mem("before MST")
  g <- graph_from_adjacency_matrix(distances, weighted = TRUE, mode = "undirected")
  mst <- mst(g)
  edge_list <- as_edgelist(mst)
  track_mem("after MST")

  rm(distances, g, mst)
  gc()

  # Process the connections
  track_mem("before processing connections")
  processed_lines <- list()
  next_id <- max(lines_sf[[new_id_column]]) + 1
  lines_to_remove <- integer()

  for(i in seq_len(nrow(edge_list))) {
    from_idx <- edge_list[i,1]
    to_idx <- edge_list[i,2]

    conn_key <- paste(min(from_idx, to_idx), max(from_idx, to_idx))
    conn <- connections[[conn_key]]

    # Find nearest lines to connection points in each component
    coords <- st_coordinates(conn$connection_line)
    from_point <- st_sfc(st_point(coords[1,]), crs = current_crs)
    to_point <- st_sfc(st_point(coords[2,]), crs = current_crs)

    comp1_lines <- lines_sf %>% filter(component == conn$from_component)
    comp2_lines <- lines_sf %>% filter(component == conn$to_component)

    from_line_idx <- which.min(st_distance(comp1_lines, from_point))
    to_line_idx <- which.min(st_distance(comp2_lines, to_point))

    # Get actual line indices in original data
    from_line_idx_orig <- which(lines_sf$component == conn$from_component)[from_line_idx]
    to_line_idx_orig <- which(lines_sf$component == conn$to_component)[to_line_idx]

    # Split lines at connection points
    line1 <- lines_sf[from_line_idx_orig,]
    split1 <- st_split(st_geometry(line1), from_point)
    segments1 <- st_collection_extract(split1, "LINESTRING")

    line2 <- lines_sf[to_line_idx_orig,]
    split2 <- st_split(st_geometry(line2), to_point)
    segments2 <- st_collection_extract(split2, "LINESTRING")

    # Process split segments
    if(length(segments1) > 0) {
      for(seg in segments1) {
        new_line <- line1
        st_geometry(new_line) <- st_sfc(seg, crs = current_crs)
        new_line[[new_id_column]] <- next_id
        next_id <- next_id + 1
        processed_lines[[length(processed_lines) + 1]] <- new_line
      }
      lines_to_remove <- c(lines_to_remove, from_line_idx_orig)
    }

    if(length(segments2) > 0) {
      for(seg in segments2) {
        new_line <- line2
        st_geometry(new_line) <- st_sfc(seg, crs = current_crs)
        new_line[[new_id_column]] <- next_id
        next_id <- next_id + 1
        processed_lines[[length(processed_lines) + 1]] <- new_line
      }
      lines_to_remove <- c(lines_to_remove, to_line_idx_orig)
    }

    # Create connection
    connection <- line1
    st_geometry(connection) <- st_sfc(st_geometry(conn$connection_line),
                                      crs = current_crs)
    connection[[new_id_column]] <- next_id
    connection$highway <- connection_type
    next_id <- next_id + 1
    processed_lines[[length(processed_lines) + 1]] <- connection

    if(track_memory && i %% 100 == 0) {
      track_mem(sprintf("after processing %d connections", i))
    }
  }
  track_mem("after processing connections")

  if(length(processed_lines) > 0) {
    track_mem("before final combining")
    new_lines_sf <- do.call(rbind, processed_lines)
    artificial_edges_sf <- new_lines_sf[new_lines_sf$highway == connection_type,]

    complete_network <- rbind(
      lines_sf[!seq_len(nrow(lines_sf)) %in% lines_to_remove,],
      new_lines_sf
    )

    if(clean_network) {
      complete_network <- ms_simplify(complete_network, keep = 0.99,
                                      keep_shapes = TRUE)
      complete_network <- st_simplify(complete_network, dTolerance = hull_tolerance)
    }
    track_mem("after final combining")
  } else {
    warning("No lines were successfully processed")
    complete_network <- lines_sf
    artificial_edges_sf <- NULL
  }

  track_mem("end")

  return(list(
    complete_network = complete_network,
    artificial_edges = artificial_edges_sf
  ))
}
