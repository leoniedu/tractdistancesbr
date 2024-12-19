#' Connect Network Components Using dodgr Network Information
#'
#' This function connects disconnected components of a street network using information
#' from both the original sf linestrings and a pre-processed dodgr network graph.
#' It uses the dodgr network's component and vertex information to efficiently find
#' connection points, then creates new edges to connect the components using a
#' minimum spanning tree approach.
#'
#' @param lines_sf sf object containing the street network linestrings. Must have
#'   a way_id column matching the way_id in dodgr_net and a highway column.
#' @param dodgr_net A dodgr network graph (from weight_streetnet). Must contain:
#'   - way_id: matching the way_ids in lines_sf (typically OSM IDs)
#'   - component: component identifiers
#'   - from_id/to_id: vertex identifiers
#' @param connection_type Character string specifying the highway type for new connections
#' @param way_id_column Name of the column in lines_sf containing original way IDs (typically OSM IDs)
#' @param track_memory Logical; whether to track memory usage
#'
#' @details
#' The function preserves original way IDs (typically OSM IDs) while adding a new internal
#' edge_id column for managing the network connections. The edge_id is a simple numeric
#' identifier starting from 1, while the original way_ids remain unchanged. New artificial
#' connections will have:
#' - A new unique edge_id
#' - NA in the original way_id column
#' - The specified connection_type in the highway column
#'
#' Only edges present in the dodgr network will be considered from the input lines_sf.
#'
#' @return A list containing:
#'   - complete_network: sf object with original and new connecting edges
#'   - artificial_edges: sf object with only the new connecting edges
#'
library(sf)
library(dplyr)
library(igraph)
library(dodgr)
library(lwgeom)


connect_network_components <- function(lines_sf,
                                       dodgr_net,
                                       connection_type = "artificial",
                                       way_id_column = "way_id",
                                       track_memory = TRUE) {

  track_mem <- function(step) {
    if(track_memory) {
      cli::cli_inform("Memory at {step}")
    }
    gc(verbose = track_memory)
  }

  track_mem("start")

  # Basic validation
  if(!"highway" %in% names(lines_sf))
    stop("lines_sf must have a 'highway' column")
  if(!way_id_column %in% names(lines_sf)) {
    stop(paste("lines_sf must have a", way_id_column, "column matching way_id in dodgr_net"))
  }
  if(!"way_id" %in% names(dodgr_net))
    stop("dodgr_net must have a way_id column")
  if(!"component" %in% names(dodgr_net))
    stop("dodgr_net must have a component column")

  # Filter lines_sf to only include ways present in dodgr_net
  lines_sf <- lines_sf %>%
    semi_join(data.frame(way_id = unique(dodgr_net$way_id)),
              by = setNames("way_id", way_id_column))

  if(nrow(lines_sf) == 0)
    stop("No matching ways found between lines_sf and dodgr_net")

  # Add internal edge_id after filtering
  lines_sf$edge_id <- seq_len(nrow(lines_sf))

  # Get components from dodgr network
  components <- unique(dodgr_net$component)
  n_components <- length(components)

  if(n_components <= 1) {
    return(list(
      complete_network = lines_sf,
      artificial_edges = NULL
    ))
  }

  current_crs <- st_crs(lines_sf)

  # Get vertices from dodgr network
  vertices <- dodgr_vertices(dodgr_net)
  if(nrow(vertices) == 0)
    stop("No vertices found in dodgr network")

  # Calculate distances and find connections
  distances <- matrix(Inf, n_components, n_components)
  connections <- list()

  track_mem("before distance calculations")
  for(i in 1:(n_components-1)) {
    for(j in (i+1):n_components) {
      comp_i <- components[i]
      comp_j <- components[j]

      # Get vertices for each component from dodgr network
      comp_i_edges <- dodgr_net$component == comp_i
      comp_j_edges <- dodgr_net$component == comp_j

      comp_i_vert_ids <- unique(c(
        dodgr_net$from_id[comp_i_edges],
        dodgr_net$to_id[comp_i_edges]
      ))
      comp_j_vert_ids <- unique(c(
        dodgr_net$from_id[comp_j_edges],
        dodgr_net$to_id[comp_j_edges]
      ))

      # Get vertex coordinates
      comp_i_vertices <- vertices[vertices$id %in% comp_i_vert_ids,]
      comp_j_vertices <- vertices[vertices$id %in% comp_j_vert_ids,]

      # Convert to sf points for distance calculation
      comp_i_points <- st_as_sf(comp_i_vertices, coords = c("x", "y"),
                                crs = current_crs)
      comp_j_points <- st_as_sf(comp_j_vertices, coords = c("x", "y"),
                                crs = current_crs)

      # Find nearest vertices
      nearest_i_idx <- st_nearest_feature(comp_j_points, comp_i_points)
      nearest_i_vertex_id <- comp_i_vertices$id[nearest_i_idx[1]]

      # Find edges containing these vertices in dodgr network
      edges_with_i <- which(dodgr_net$from_id == nearest_i_vertex_id |
                              dodgr_net$to_id == nearest_i_vertex_id)

      # Get corresponding sf line using way_id mapping
      way_id_i <- dodgr_net$way_id[edges_with_i[1]]
      nearest_i_line_idx <- which(lines_sf[[way_id_column]] == way_id_i)

      # Find lines from component j
      comp_j_way_ids <- unique(dodgr_net$way_id[comp_j_edges])
      comp_j_lines <- lines_sf[lines_sf[[way_id_column]] %in% comp_j_way_ids,]

      # Find nearest point and line in component j
      nearest_point <- st_sfc(st_point(
        c(comp_i_vertices$x[nearest_i_idx[1]],
          comp_i_vertices$y[nearest_i_idx[1]])),
        crs = current_crs)

      nearest_j_line_idx <- st_nearest_feature(nearest_point, comp_j_lines)
      nearest_j_way_id <- comp_j_lines[[way_id_column]][nearest_j_line_idx]

      # Get actual nearest points between the lines
      nearest_points <- st_nearest_points(
        lines_sf[nearest_i_line_idx,],
        comp_j_lines[nearest_j_line_idx,]
      )

      # Create connection
      nearest_endpoints <- st_cast(nearest_points, "POINT")
      connection_line <- st_sfc(st_linestring(rbind(
        st_coordinates(nearest_endpoints[1])[1,],
        st_coordinates(nearest_endpoints[2])[1,]
      )), crs = current_crs)

      connections[[paste(i,j)]] <- list(
        from_component = components[i],
        to_component = components[j],
        connection_line = connection_line,
        distance = st_length(connection_line),
        from_way_id = way_id_i,
        to_way_id = nearest_j_way_id
      )
      distances[i,j] <- distances[j,i] <- st_length(connection_line)
    }
    if(track_memory && i %% 100 == 0) {
      track_mem(sprintf("after %d distance rows", i))
    }
  }
  track_mem("after distances")

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
  next_edge_id <- max(lines_sf$edge_id) + 1
  lines_to_remove <- integer()

  for(i in seq_len(nrow(edge_list))) {
    track_mem(glue::glue("cleanup before connecting edge {i}"))
    from_idx <- edge_list[i,1]
    to_idx <- edge_list[i,2]

    conn_key <- paste(min(from_idx, to_idx), max(from_idx, to_idx))
    conn <- connections[[conn_key]]

    # Get lines using way IDs
    line1_idx <- which(lines_sf[[way_id_column]] == conn$from_way_id)
    line2_idx <- which(lines_sf[[way_id_column]] == conn$to_way_id)

    line1 <- lines_sf[line1_idx,]
    line2 <- lines_sf[line2_idx,]

    # Get connection endpoints
    coords <- st_coordinates(conn$connection_line)
    from_point <- st_sfc(st_point(coords[1,]), crs = current_crs)
    to_point <- st_sfc(st_point(coords[2,]), crs = current_crs)

    # Split lines at connection points
    split1 <- st_split(st_geometry(line1), from_point)
    segments1 <- st_collection_extract(split1, "LINESTRING")

    split2 <- st_split(st_geometry(line2), to_point)
    segments2 <- st_collection_extract(split2, "LINESTRING")

    # Process split segments
    if(length(segments1) > 0) {
      for(seg in segments1) {
        new_line <- line1
        st_geometry(new_line) <- st_sfc(seg, crs = current_crs)
        new_line$edge_id <- next_edge_id
        next_edge_id <- next_edge_id + 1
        processed_lines[[length(processed_lines) + 1]] <- new_line
      }
      lines_to_remove <- c(lines_to_remove, line1_idx)
    }

    if(length(segments2) > 0) {
      for(seg in segments2) {
        new_line <- line2
        st_geometry(new_line) <- st_sfc(seg, crs = current_crs)
        new_line$edge_id <- next_edge_id
        next_edge_id <- next_edge_id + 1
        processed_lines[[length(processed_lines) + 1]] <- new_line
      }
      lines_to_remove <- c(lines_to_remove, line2_idx)
    }

    # Create connection
    connection <- line1
    st_geometry(connection) <- st_sfc(st_geometry(conn$connection_line),
                                      crs = current_crs)
    connection$edge_id <- next_edge_id
    connection[[way_id_column]] <- NA  # No OSM way_id for artificial connections
    connection$highway <- connection_type
    next_edge_id <- next_edge_id + 1
    processed_lines[[length(processed_lines) + 1]] <- connection

    if(track_memory && i %% 100 == 0) {
      track_mem(sprintf("after processing %d connections", i))
    }
  }
  track_mem("after processing connections")

  if(length(processed_lines) > 0) {
    track_mem("before final combining")
    new_lines_sf <- bind_rows(processed_lines)
    artificial_edges_sf <- new_lines_sf[new_lines_sf$highway == connection_type,]
    complete_network <- bind_rows(
      lines_sf[!seq_len(nrow(lines_sf)) %in% lines_to_remove,],
      new_lines_sf
    )
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
