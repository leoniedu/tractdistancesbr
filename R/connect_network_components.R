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

      # Get vertices for each component
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

      # Convert to sf points for connecting
      comp_i_point <- st_as_sf(
        comp_i_vertices[1,],
        coords = c("x", "y"),
        crs = current_crs
      )
      comp_j_point <- st_as_sf(
        comp_j_vertices[1,],
        coords = c("x", "y"),
        crs = current_crs
      )

      # Get lines for each component
      comp_i_way_ids <- unique(dodgr_net$way_id[comp_i_edges])
      comp_j_way_ids <- unique(dodgr_net$way_id[comp_j_edges])

      comp_i_line <- lines_sf[lines_sf[[way_id_column]] %in% comp_i_way_ids,][1,]
      comp_j_line <- lines_sf[lines_sf[[way_id_column]] %in% comp_j_way_ids,][1,]

      # Use split_and_connect_lines to create connection
      connection_result <- split_and_connect_lines(
        line1_sf = comp_i_line,
        point1_sf = comp_i_point,
        point2_sf = comp_j_point,
        line2_sf = comp_j_line,
        connection_id = paste0("CONN_", i, "_", j),
        highway = connection_type
      )

      # Store connection info
      connection_geom <- connection_result[connection_result$artificial,]
      connections[[paste(i,j)]] <- list(
        from_component = components[i],
        to_component = components[j],
        connection_line = st_geometry(connection_geom),
        distance = st_length(connection_geom),
        from_way_id = comp_i_line[[way_id_column]],
        to_way_id = comp_j_line[[way_id_column]],
        connection_result = connection_result
      )

      distances[i,j] <- distances[j,i] <- st_length(connection_geom)
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
  all_segments <- list()
  artificial_segments <- list()

  for(i in seq_len(nrow(edge_list))) {
    from_idx <- edge_list[i,1]
    to_idx <- edge_list[i,2]

    conn_key <- paste(min(from_idx, to_idx), max(from_idx, to_idx))
    conn <- connections[[conn_key]]

    # Add segments from connection result
    all_segments[[i]] <- conn$connection_result
    artificial_segments[[i]] <- conn$connection_result[conn$connection_result$artificial,]

    if(track_memory && i %% 100 == 0) {
      track_mem(sprintf("after processing %d connections", i))
    }
  }
  track_mem("after processing connections")

  if(length(all_segments) > 0) {
    track_mem("before final combining")

    # Combine all segments
    complete_network <- do.call(rbind, all_segments)
    artificial_edges <- do.call(rbind, artificial_segments)

    # Add remaining unmodified lines (being careful to avoid duplicates)
    modified_way_ids <- unique(complete_network$original_way_id)
    unmodified_lines <- lines_sf[!lines_sf[[way_id_column]] %in% modified_way_ids,]
    complete_network <- bind_rows(complete_network, unmodified_lines)

    track_mem("after final combining")
  } else {
    warning("No lines were successfully processed")
    complete_network <- lines_sf
    artificial_edges <- NULL
  }

  track_mem("end")

  return(list(
    complete_network = complete_network,
    artificial_edges = artificial_edges
  ))
}
