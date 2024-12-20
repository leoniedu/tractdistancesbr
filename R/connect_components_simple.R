#' Connect Network Components Using Closest Vertices
#' @param lines_sf sf object containing the street network linestrings
#' @param dodgr_net A dodgr network graph with component and vertex information
#' @param connection_type Character string specifying the type for new connections
#' @param lines_way_id Name of the column in lines_sf containing original way IDs
#' @param dodgr_way_id Name of the column in dodgr_net containing way IDs
#' @param track_memory Logical; whether to track memory usage
#'
connect_components_simple <- function(lines_sf,
                                      dodgr_net,
                                      connection_type = "artificial",
                                      lines_way_id = "osm_id",
                                      dodgr_way_id = "way_id",
                                      new_way_id = "new_id",
                                      track_memory = TRUE) {

  # Helper function to track memory usage if requested
  track_mem <- function(step) {
    if(track_memory) {
      cli::cli_inform("Memory at {step}")
    }
    gc(verbose = track_memory)
  }

  track_mem("start")

  # Basic validation of required columns
  if(!lines_way_id %in% names(lines_sf)) {
    stop(paste("lines_sf must have a", lines_way_id, "column"))
  }
  if(!dodgr_way_id %in% names(dodgr_net)) {
    stop(paste("dodgr_net must have a", dodgr_way_id, "column"))
  }
  if(!"component" %in% names(dodgr_net))
    stop("dodgr_net must have a component column")

  # Filter lines_sf to only include ways that exist in dodgr_net
  lines_sf <- lines_sf %>%
    semi_join(
      data.frame(way_id = unique(dodgr_net[[dodgr_way_id]])),
      by = setNames("way_id", lines_way_id)
    )

  if(nrow(lines_sf) == 0)
    stop("No matching ways found between lines_sf and dodgr_net")

  # Get list of unique components from the network
  components <- unique(dodgr_net$component)
  n_components <- length(components)

  # If only one component, return original network unchanged
  if(n_components <= 1) {
    return(list(
      complete_network = lines_sf,
      artificial_edges = NULL
    ))
  }

  track_mem("before vertex processing")

  # Get vertices from dodgr network and current coordinate reference system
  vertices <- dodgr_vertices(dodgr_net)
  current_crs <- st_crs(lines_sf)

  # Initialize data structures for storing all possible connections:
  # - artificial_lines: list to store the actual connection geometries
  # - distances: matrix to store distances between components for MST
  # - edge_indices: matrix to track which connection belongs to which component pair
  artificial_lines <- list()
  distances <- matrix(Inf, n_components, n_components)
  edge_indices <- matrix("", n_components, n_components)
  conn_counter <- 1

  track_mem("before component connections")

  # Calculate all possible connections between components
  for(i in 1:(n_components-1)) {
    for(j in (i+1):n_components) {
      # Extract vertices belonging to each component
      comp_i_verts <- vertices[vertices$component==components[i],]
      comp_j_verts <- vertices[vertices$component==components[j],]

      # Convert vertices to sf points for spatial operations
      comp_i_points <- st_as_sf(comp_i_verts, coords = c("x", "y"),
                                crs = current_crs)
      comp_j_points <- st_as_sf(comp_j_verts, coords = c("x", "y"),
                                crs = current_crs)

      # Find nearest points between components:
      # 1. Find closest point in component i to any point in component j
      # 2. Find closest point in component j to that specific point in component i
      nearest_i_idx <- st_nearest_feature(comp_j_points, comp_i_points)
      nearest_j_idx <- st_nearest_feature(comp_i_points[nearest_i_idx[1],],
                                          comp_j_points)

      # Create a straight line connecting the nearest points
      connection <- st_sfc(st_linestring(rbind(
        st_coordinates(comp_i_points[nearest_i_idx[1],]),
        st_coordinates(comp_j_points[nearest_j_idx[1],])
      )), crs = current_crs)

      # Store the distance in both directions (symmetric matrix)
      distances[i,j] <- distances[j,i] <- st_length(connection)

      # Create unique ID for this connection and store it in edge_indices
      conn_id <- paste0("CONN_", i, "_", j)
      edge_indices[i,j] <- edge_indices[j,i] <- conn_id

      # Store the connection geometry in artificial_lines using conn_id as key
      artificial_lines[[conn_id]] <- st_sf(
        geometry = connection,
        highway = connection_type,
        artificial = TRUE
      )
      # Add the connection ID as the new way ID
      artificial_lines[[conn_id]][[new_way_id]] <- conn_id

      conn_counter <- conn_counter + 1

      if(track_memory && i %% 100 == 0) {
        track_mem(sprintf("after processing %d component pairs", i))
      }
    }
  }

  track_mem("before MST")

  # Calculate Minimum Spanning Tree:
  # 1. Create graph from distance matrix
  # 2. Find minimum spanning tree
  # 3. Get list of edges in the MST
  g <- igraph::graph_from_adjacency_matrix(distances, weighted = TRUE,
                                           mode = "undirected")
  mst <- igraph::mst(g)
  edge_list <- igraph::as_edgelist(mst)

  # Clean up large objects no longer needed
  rm(distances, g, mst)

  track_mem("before final combining")

  # Get the connection IDs that correspond to the MST edges
  mst_connections <- sapply(1:nrow(edge_list), function(i) {
    from_idx <- edge_list[i,1]
    to_idx <- edge_list[i,2]
    # Look up the connection ID from our edge_indices matrix
    edge_indices[from_idx, to_idx]
  })

  # Keep only the connections that were selected by the MST
  artificial_edges <- bind_rows(artificial_lines[mst_connections])

  # Combine original network with the MST-selected connections
  complete_network <- bind_rows(
    lines_sf,
    artificial_edges
  )
  # Ensure all lines have a way ID, using original ID if no new one assigned
  complete_network[[new_way_id]] <- coalesce(complete_network[[new_way_id]],
                                             complete_network[[lines_way_id]])

  track_mem("end")

  return(list(
    complete_network = complete_network,
    artificial_edges = artificial_edges
  ))
}
