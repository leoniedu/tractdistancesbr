library(sf)
library(dplyr)
library(TSP)
library(units)
library(lwgeom)
library(rmapshaper)

connect_network_components <- function(lines_sf,
                                       connection_type = "artificial",
                                       new_id_column = "line_id",
                                       clean_network = TRUE) {
  # Basic validation
  if(!"component" %in% names(lines_sf))
    stop("Column 'component' not found in lines_sf")
  if(!"highway" %in% names(lines_sf))
    stop("lines_sf must have a 'highway' column")

  # Add new ID column if needed
  if(!new_id_column %in% names(lines_sf)) {
    lines_sf[[new_id_column]] <- seq_len(nrow(lines_sf))
  }

  # Create mapping between component values and indices
  components <- unique(lines_sf$component)
  n_components <- length(components)

  if(n_components <= 1) {
    return(list(
      complete_network = lines_sf,
      artificial_edges = NULL
    ))
  }

  # Create lookup tables
  comp_to_idx <- setNames(seq_along(components), as.character(components))
  idx_to_comp <- setNames(components, seq_along(components))

  # Create convex hulls for each component index
  component_hulls <- vector("list", n_components)
  for(i in seq_len(n_components)) {
    comp <- idx_to_comp[as.character(i)]
    coords <- st_coordinates(lines_sf[lines_sf$component == comp,])
    unique_coords <- unique(coords[,c("X","Y")])
    points_geom <- st_multipoint(as.matrix(unique_coords))
    points_sfc <- st_sfc(points_geom, crs = st_crs(lines_sf))
    component_hulls[[i]] <- st_convex_hull(points_sfc)
  }

  # Calculate distances and find connections
  distances <- matrix(Inf, n_components, n_components)
  connections <- list()

  for(i in 1:(n_components-1)) {
    for(j in (i+1):n_components) {
      # Try to find intersection
      intersection <- st_intersection(component_hulls[[i]], component_hulls[[j]])

      if(length(intersection) > 0) {
        # Hulls intersect - use centroid of intersection
        connection_point <- st_centroid(intersection)
        connection_line <- st_linestring(rbind(
          st_coordinates(connection_point),
          st_coordinates(connection_point)
        ))

        connections[[paste(i,j)]] <- list(
          from_component = idx_to_comp[as.character(i)],
          to_component = idx_to_comp[as.character(j)],
          connection_line = connection_line,
          distance = 0
        )

        distances[i,j] <- distances[j,i] <- 0

      } else {
        # Hulls don't intersect - use hull distance
        hull_dist <- as.numeric(st_distance(component_hulls[[i]],
                                            component_hulls[[j]]))
        nearest_hull_points <- st_nearest_points(component_hulls[[i]],
                                                 component_hulls[[j]])

        connections[[paste(i,j)]] <- list(
          from_component = idx_to_comp[as.character(i)],
          to_component = idx_to_comp[as.character(j)],
          connection_line = nearest_hull_points,
          distance = hull_dist
        )

        distances[i,j] <- distances[j,i] <- hull_dist
      }
    }
  }

  # Set diagonal to 0 to avoid NA issues
  diag(distances) <- 0

  # Use TSP solver to find minimum spanning tree
  tsp <- TSP(distances)
  tour <- solve_TSP(tsp, method = "nearest_insertion")
  tour_vector <- as.integer(tour)

  # Process the selected connections
  processed_lines <- list()
  next_id <- max(lines_sf[[new_id_column]]) + 1
  lines_to_remove <- integer()

  for(i in 1:(length(tour_vector)-1)) {
    from_idx <- tour_vector[i]
    to_idx <- tour_vector[i+1]

    conn_key <- paste(min(from_idx, to_idx), max(from_idx, to_idx))
    conn <- connections[[conn_key]]

    # Find nearest lines to connection points in each component
    from_point <- st_point(st_coordinates(conn$connection_line)[1,])
    to_point <- st_point(st_coordinates(conn$connection_line)[2,])

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
        st_geometry(new_line) <- seg
        new_line[[new_id_column]] <- next_id
        next_id <- next_id + 1
        processed_lines[[length(processed_lines) + 1]] <- new_line
      }
      lines_to_remove <- c(lines_to_remove, from_line_idx_orig)
    }

    if(length(segments2) > 0) {
      for(seg in segments2) {
        new_line <- line2
        st_geometry(new_line) <- seg
        new_line[[new_id_column]] <- next_id
        next_id <- next_id + 1
        processed_lines[[length(processed_lines) + 1]] <- new_line
      }
      lines_to_remove <- c(lines_to_remove, to_line_idx_orig)
    }

    # Create connection
    connection <- line1
    st_geometry(connection) <- conn$connection_line
    connection[[new_id_column]] <- next_id
    connection$highway <- connection_type
    next_id <- next_id + 1
    processed_lines[[length(processed_lines) + 1]] <- connection
  }

  if(length(processed_lines) > 0) {
    new_lines_sf <- do.call(rbind, processed_lines)
    artificial_edges_sf <- new_lines_sf[new_lines_sf$highway == connection_type,]

    complete_network <- rbind(
      lines_sf[!seq_len(nrow(lines_sf)) %in% lines_to_remove,],
      new_lines_sf
    )

    if(clean_network) {
      complete_network <- ms_simplify(complete_network, keep = 0.99,
                                      keep_shapes = TRUE)
      complete_network <- st_simplify(complete_network, dTolerance = 1)
    }
  } else {
    warning("No lines were successfully processed")
    complete_network <- lines_sf
    artificial_edges_sf <- NULL
  }

  return(list(
    complete_network = complete_network,
    artificial_edges = artificial_edges_sf
  ))
}
