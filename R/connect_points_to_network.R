#' Connect Points to Street Network
#'
#' This function connects a set of points to a street network by creating artificial
#' connections to the nearest network segments. It splits the original network segments
#' at the connection points and adds new artificial edges.
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

  max_distance <- units::set_units(max_distance, "m")

  # Basic validation
  if(!"highway" %in% names(lines_sf))
    stop("lines_sf must have a 'highway' column")
  if(!way_id_column %in% names(lines_sf))
    stop(paste("lines_sf must have a", way_id_column, "column"))

  # Ensure same CRS
  current_crs <- st_crs(lines_sf)
  points_sf <- st_transform(points_sf, current_crs)

  # Initialize lists to store results
  all_segments <- list()
  artificial_segments <- list()
  connection_points <- list()

  # Process each point
  for(i in seq_len(nrow(points_sf))) {
    # Get current point
    current_point <- points_sf[i,]

    # Find nearest line segment
    nearest_line_idx <- st_nearest_feature(current_point, lines_sf)
    nearest_line <- lines_sf[nearest_line_idx,]

    # Calculate distance to nearest line
    distance <- st_distance(current_point, nearest_line)

    # Skip if distance exceeds maximum
    if(distance > max_distance) {
      warning(sprintf("Point %d exceeds maximum distance", i))
      next
    }

    # Find exact connection point on the line
    connection_point <- st_nearest_points(current_point, nearest_line) %>%
      st_cast("POINT") %>%
      .[2] %>%  # Second point is on the line
      st_sfc(crs = current_crs)

    # Store connection point
    connection_points[[i]] <- st_as_sf(connection_point)

    # Split and connect the line
    connection_result <- split_and_connect_lines(
      line1_sf = nearest_line,
      point1_sf = current_point,
      point2_sf = st_as_sf(connection_point),
      line2_sf = nearest_line,  # Same line since we're just splitting it
      highway = highway_type
    )

    # Store results
    all_segments[[i]] <- connection_result
    artificial_segments[[i]] <- connection_result[connection_result$artificial,]
  }

  track_mem("after processing points")

  if(length(all_segments) > 0) {
    # Combine all segments
    complete_network <- do.call(rbind, all_segments)
    artificial_edges <- do.call(rbind, artificial_segments)
    connection_points_sf <- do.call(rbind, connection_points)

    # Add remaining unmodified lines
    modified_way_ids <- unique(complete_network$way_id)
    unmodified_lines <- lines_sf[!lines_sf[[way_id_column]] %in% modified_way_ids,]
    complete_network <- bind_rows(complete_network, unmodified_lines)

    track_mem("after final combining")
  } else {
    warning("No points were successfully connected")
    complete_network <- lines_sf
    artificial_edges <- NULL
    connection_points_sf <- NULL
  }

  track_mem("end")

  return(list(
    complete_network = complete_network %>%
      mutate(artificial = coalesce(artificial, FALSE), id=as.character(1:n())),
    artificial_edges = artificial_edges,
    connection_points = connection_points_sf
  ))
}
