#' Connect Points to Street Network
#'
#' This function connects a set of points to a street network by creating artificial
#' connections to the nearest network segments. It splits the original network segments
#' at the connection points and adds new artificial edges. Alternative to dodgr::add_nodes_to_graph
#'
#' @param lines_sf sf object containing the street network linestrings. Must have
#'   a way_id column and a highway column.
#' @param points_sf sf object containing the points to connect to the network
#' @param max_distance Maximum allowed distance for connections (in units of the CRS)
#' @param highway_type Character string specifying the highway type for new connections
#' @param way_id_column Name of the column in lines_sf containing way IDs
#' @param track_memory Logical; whether to track memory usage
#'
#' @return A list containing:
#'   - complete_network: sf object with original and new connecting edges
#'   - artificial_edges: sf object with only the new connecting edges
#'   - connection_points: sf object with the actual connection points on the network
#'
#' @import sf dplyr lwgeom
#' @export
connect_points_to_network <- function(lines_sf,
                                      points_sf,
                                      max_distance = Inf,
                                      min_distance = 5,
                                      highway_type = "artificial",
                                      way_id_column = "osm_id",
                                      track_memory = TRUE) {

  track_mem <- function(step) {
    if(track_memory) {
      cli::cli_inform("Memory at {step}")
    }
    gc(verbose = track_memory)
  }

  ## FIX:: are from/to/edge ids correctly being taken care of?

  track_mem("start")

  max_distance <- units::set_units(max_distance, "m")
  min_distance <- units::set_units(min_distance, "m")

  # Basic validation
  if(!"highway" %in% names(lines_sf))
    stop("lines_sf must have a 'highway' column")
  if(!way_id_column %in% names(lines_sf))
    stop(paste("lines_sf must have a", way_id_column, "column"))

  # Ensure same CRS
  current_crs <- st_crs(lines_sf)
  points_sf <- st_transform(points_sf, current_crs)
  nf <- st_nearest_feature(points_sf, lines_sf)
  track_mem("after calculating nearest lines")
  if ((min_distance>units::set_units(0, "m"))|(is.finite(max_distance))) {
    cli::cli_inform("Checking distance for {nrow(points_sf)} points.")
    nf_dist <- st_distance(points_sf,lines_sf[nf,], by_element = TRUE)
    track_mem("after calculating distances")
    nf_d <- (nf_dist<max_distance)&(nf_dist>min_distance)
    points_sf <- points_sf[nf_d,]
    nf <- nf[nf_d]
    cli::cli_inform("{nrow(points_sf)} points remaining within ({min_distance},{max_distance}] meters to lines.")
  }
  track_mem("before creating segments")
  # Process each point
  segments <- furrr::future_map(.x=seq_len(nrow(points_sf)), .f=function(i) {
    #browser()
    # Get current point
    current_point <- points_sf[i,]

    # Find nearest line segment
    nearest_line_idx <- nf[i]
    nearest_line <- lines_sf[nearest_line_idx,]

    # Find exact connection point on the line
    connection_point <- st_nearest_points(current_point, nearest_line) %>%
      st_cast("POINT") %>%
      .[2] %>%  # Second point is on the line
      st_sfc(crs = current_crs)

    # Split and connect the line
    connection_result <- split_and_connect_lines(
      line1_sf = nearest_line,
      point1_sf = current_point,
      point2_sf = st_as_sf(connection_point),
      line2_sf = NULL,
      highway = highway_type,
      edge_marker_col="point_connections"
    )#%>%mutate(component=component[1])

    # Return results
    list(all_segments=connection_result, artificial_segments=connection_result[connection_result$point_connections,], connection_points=st_as_sf(connection_point))
  }, .progress = TRUE, .options=furrr::furrr_options(seed = TRUE))
  track_mem("after processing points")
  all_segments <- lapply(segments, function(x) x[["all_segments"]])
  artificial_segments <- lapply(segments, function(x) x[["artificial_segments"]])
  connection_points <- lapply(segments, function(x) x[["connection_points"]])
  if(length(all_segments) > 0) {
    # Combine all segments
    complete_network <- do.call(rbind, all_segments)
    rm(all_segments)
    artificial_edges <- do.call(rbind, artificial_segments)
    rm(artificial_segments)
    connection_points_sf <- do.call(rbind, connection_points)
    rm(connection_points)
    track_mem("after final combining")

    # Add remaining unmodified lines
    # FIX: what happens when the same edge is closest to more than one point?
    # rough solution: add original lines which were splitted back to the map,
    # rough solution2: add original lines which were splitted *more than once* back to the map,
    modified_way_ids <- unique(complete_network$way_id)
    unmodified_lines <- lines_sf[!lines_sf[[way_id_column]] %in% modified_way_ids,]
    complete_network <- bind_rows(complete_network, unmodified_lines)
  } else {
    warning("No points were successfully connected")
    complete_network <- lines_sf
    artificial_edges <- NULL
    connection_points_sf <- NULL
  }

  track_mem("end")

  return(list(
    complete_network = complete_network %>%
      mutate(id=as.character(1:n())),
    artificial_edges = artificial_edges,
    connection_points = connection_points_sf
  ))
}
