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
#' @param highway_type Character string specifying the highway type for new connections
#' @param way_id_column Name of the column in lines_sf containing original way IDs (typically OSM IDs)
#' @param track_memory Logical; whether to track memory usage
#'
#' @return A list containing:
#'   - complete_network: sf object with original and new connecting edges
#'   - artificial_edges: sf object with only the new connecting edges
#'
#' @import sf dplyr igraph dodgr lwgeom
#' @export
connect_network_components <- function(lines_sf,
                                       dodgr_net,
                                       highway_type = "artificial",
                                       way_id_column = "osm_id",
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

  # Initialize artificial column if it doesn't exist
  if(!"artificial" %in% names(lines_sf)) {
    lines_sf$artificial <- FALSE
  }

  # Create a guaranteed unique column name at the start
  make_unique_colname <- function(df, prefix = "tmp") {
    make.unique(c(names(df), prefix))[length(names(df)) + 1]
  }
  edge_marker_col <- make_unique_colname(lines_sf, "new_edge")

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

  # Process components and create connections using the same edge_marker_col
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

      # Get the actual points to connect between the lines
      nearest_points <- st_nearest_points(
        lines_sf[nearest_i_line_idx,],
        comp_j_lines[nearest_j_line_idx,]
      )

      # Extract the endpoints for connection
      nearest_endpoints <- st_cast(nearest_points, "POINT")
      point1 <- st_sfc(st_point(st_coordinates(nearest_endpoints[1])[1,]),
                       crs = current_crs)
      point2 <- st_sfc(st_point(st_coordinates(nearest_endpoints[2])[1,]),
                       crs = current_crs)

      # Use split_and_connect_lines with the found points and lines
      connection_result <- split_and_connect_lines(
        line1_sf = lines_sf[nearest_i_line_idx,],
        point1_sf = st_as_sf(point1),
        point2_sf = st_as_sf(point2),
        line2_sf = comp_j_lines[nearest_j_line_idx,],
        highway = highway_type,
        edge_marker_col = edge_marker_col  # Pass our unique column name
      )

      # Store connection info - now using our marker column to identify new edges
      connection_geom <- connection_result[connection_result[[edge_marker_col]],]
      connections[[paste(i,j)]] <- list(
        from_component = components[i],
        to_component = components[j],
        connection_line = st_geometry(connection_geom),
        distance = st_length(connection_geom),
        connection_result = connection_result
      )
      distances[i,j] <- distances[j,i] <- st_length(connection_geom)
    }
  }
  track_mem("after distances")

  # Find minimum spanning tree
  g <- igraph::graph_from_adjacency_matrix(distances, weighted = TRUE, mode = "undirected")
  mst <- igraph::mst(g)
  edge_list <- igraph::as_edgelist(mst)
  track_mem("after MST")

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
    # Combine all segments
    complete_network <- do.call(rbind, all_segments)

    # Identify artificial edges using our marker
    artificial_edges <- complete_network[complete_network[[edge_marker_col]],]

    # Add remaining unmodified lines
    modified_way_ids <- unique(complete_network$way_id)
    unmodified_lines <- lines_sf[!lines_sf[[way_id_column]] %in% modified_way_ids,]
    unmodified_lines[[edge_marker_col]] <- FALSE

    # Combine and update artificial status
    complete_network <- bind_rows(complete_network, unmodified_lines) %>%
      mutate(
        id = as.character(1:n()),
        # Update artificial status only for new edges
        artificial = if_else(.data[[edge_marker_col]], TRUE, artificial)
      ) %>%
      select(-all_of(edge_marker_col))  # Remove our temporary column

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
