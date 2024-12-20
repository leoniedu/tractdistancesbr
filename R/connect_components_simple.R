#' Connect Network Components Using Closest Vertices
#'
#' A simplified function that connects disconnected components of a street network
#' by finding and connecting the closest vertices between components.
#'
#' @param lines_sf sf object containing the street network linestrings
#' @param dodgr_net A dodgr network graph with component and vertex information
#' @param connection_type Character string specifying the type for new connections
#' @param lines_way_id Name of the column in lines_sf containing original way IDs
#' @param dodgr_way_id Name of the column in dodgr_net containing way IDs
#' @param track_memory Logical; whether to track memory usage
#'
#' @return A list containing the complete network and artificial edges
#'
#' @import sf dplyr igraph dodgr
#' @export
#'
connect_components_simple <- function(lines_sf,
                                      dodgr_net,
                                      connection_type = "artificial",
                                      lines_way_id = "osm_id",
                                      dodgr_way_id = "way_id",
                                      new_way_id= "new_id",
                                      track_memory = TRUE) {

  track_mem <- function(step) {
    if(track_memory) {
      cli::cli_inform("Memory at {step}")
    }
    gc(verbose = track_memory)
  }

  track_mem("start")

  # Basic validation
  if(!lines_way_id %in% names(lines_sf)) {
    stop(paste("lines_sf must have a", lines_way_id, "column"))
  }
  if(!dodgr_way_id %in% names(dodgr_net)) {
    stop(paste("dodgr_net must have a", dodgr_way_id, "column"))
  }
  if(!"component" %in% names(dodgr_net))
    stop("dodgr_net must have a component column")

  # Filter lines_sf to only include ways present in dodgr_net
  lines_sf <- lines_sf %>%
    semi_join(
      data.frame(way_id = unique(dodgr_net[[dodgr_way_id]])),
      by = setNames("way_id", lines_way_id)
    )

  if(nrow(lines_sf) == 0)
    stop("No matching ways found between lines_sf and dodgr_net")

  # Get unique components
  components <- unique(dodgr_net$component)
  n_components <- length(components)

  if(n_components <= 1) {
    return(list(
      complete_network = lines_sf,
      artificial_edges = NULL
    ))
  }

  track_mem("before vertex processing")

  # Get vertices from dodgr network
  vertices <- dodgr_vertices(dodgr_net)
  current_crs <- st_crs(lines_sf)

  # Initialize lists for storing connections
  artificial_lines <- list()
  distances <- matrix(Inf, n_components, n_components)
  conn_counter <- 1

  track_mem("before component connections")

  # Calculate distances between components
  for(i in 1:(n_components-1)) {
    for(j in (i+1):n_components) {
      # Get vertices for each component
      ## FIX: Cant just get from the component column?
      comp_i_verts <- vertices[vertices$component==components[i],]
      comp_j_verts <- vertices[vertices$component==components[j],]
      # Convert to sf points
      comp_i_points <- st_as_sf(comp_i_verts, coords = c("x", "y"),
                                crs = current_crs)
      comp_j_points <- st_as_sf(comp_j_verts, coords = c("x", "y"),
                                crs = current_crs)

      # Find nearest points using st_nearest_feature
      nearest_i_idx <- st_nearest_feature(comp_j_points, comp_i_points)
      nearest_j_idx <- st_nearest_feature(comp_i_points[nearest_i_idx[1],],
                                          comp_j_points)

      # Create connecting line
      connection <- st_sfc(st_linestring(rbind(
        st_coordinates(comp_i_points[nearest_i_idx[1],]),
        st_coordinates(comp_j_points[nearest_j_idx[1],])
      )), crs = current_crs)

      # Store distance for MST
      distances[i,j] <- distances[j,i] <- st_length(connection)

      # Create artificial edge with the dodgr way ID format
      artificial_lines[[conn_counter]] <- st_sf(
        geometry = connection,
        highway = connection_type,
        artificial = TRUE
      )
      # Add way ID column with the same name as dodgr_net uses
      artificial_lines[[conn_counter]][[new_way_id]] <-
        paste0("CONN_", i, "_", j)

      conn_counter <- conn_counter + 1

      if(track_memory && i %% 100 == 0) {
        track_mem(sprintf("after processing %d component pairs", i))
      }
    }
  }

  track_mem("before MST")

  # Find minimum spanning tree
  browser()
  g <- igraph::graph_from_adjacency_matrix(distances, weighted = TRUE,
                                   mode = "undirected")
  mst <- igraph::mst(g)
  edge_list <- igraph::as_edgelist(mst)


  # Clean up large objects
  rm(distances, g, mst)

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

  }
  track_mem("after processing connections")

  if(length(all_segments) > 0) {
    track_mem("before final combining")

    # Combine all segments
    complete_network <- do.call(rbind, all_segments)
    artificial_edges <- do.call(rbind, artificial_segments)

    # Add remaining unmodified lines
    modified_way_ids <- unique(complete_network$way_id)
    unmodified_lines <- lines_sf[!lines_sf[[way_id_column]] %in% modified_way_ids,]
    complete_network <- bind_rows(complete_network, unmodified_lines)

    track_mem("after final combining")
  } else {
    warning("No lines were successfully processed")
    complete_network <- lines_sf
    artificial_edges <- NULL
  }

  track_mem("end")
  complete_network[[new_way_id]] <- coalesce(complete_network[[new_way_id]], complete_network[[lines_way_id]])

  return(list(
    complete_network = complete_network,
    artificial_edges = artificial_edges
  ))
}

