#' Connect Points to Line Network
#'
#' @param points_sf An sf object containing points to be connected to the network
#' @param lines_sf An sf object containing LINESTRING geometries representing the network
#' @param connection_type Character string specifying the highway type for new connection lines
#' @param new_id_column Character string specifying the name of the column to use/create for line identification
#'
#' @return An sf object
#' @export
connect_points_to_lines <- function(points_sf, lines_sf,
                                    connection_type = "artificial",
                                    new_id_column = "line_id") {
  # Validate inputs
  if (!inherits(points_sf, "sf")) {
    stop("points_sf must be an sf object")
  }
  if (!inherits(lines_sf, "sf")) {
    stop("lines_sf must be an sf object")
  }
  if (!all(sf::st_geometry_type(lines_sf) == "LINESTRING")) {
    stop("lines_sf must contain only LINESTRING geometries")
  }
  if (!"highway" %in% names(lines_sf)) {
    stop("lines_sf must have a 'highway' column")
  }

  # Ensure same CRS
  if (sf::st_crs(points_sf) != sf::st_crs(lines_sf)) {
    points_sf <- sf::st_transform(points_sf, sf::st_crs(lines_sf))
  }

  # Add temporary internal ID column
  internal_id <- "temp_internal_id"
  lines_sf[[internal_id]] <- seq_len(nrow(lines_sf))
  next_id <- nrow(lines_sf) + 1

  # Initialize list for new lines
  processed_lines <- list()
  lines_to_remove <- integer()

  # Process each point
  for (i in 1:nrow(points_sf)) {
    point <- points_sf[i,]
    # Find nearest point on lines
    nearest_lines <- sf::st_nearest_points(point, lines_sf)
    min_idx <- which.min(sf::st_length(nearest_lines))
    target_line <- lines_sf[min_idx,]
    connection_point <- sf::st_cast(nearest_lines[min_idx], "POINT")[2]

    # Split line at connection point
    split_geom <- lwgeom::st_split(sf::st_geometry(target_line), connection_point)
    segments <- sf::st_collection_extract(split_geom, "LINESTRING")

    if (length(segments) == 2) {  # Valid split
      # Create two segments preserving all attributes
      segment1 <- segment2 <- target_line
      sf::st_geometry(segment1) <- segments[1]
      sf::st_geometry(segment2) <- segments[2]

      # Create connection line
      connection <- target_line
      sf::st_geometry(connection) <- sf::st_sfc(nearest_lines[min_idx],
                                                crs = sf::st_crs(lines_sf))

      # Update internal IDs
      segment1[[internal_id]] <- next_id
      segment2[[internal_id]] <- next_id + 1
      connection[[internal_id]] <- next_id + 2
      next_id <- next_id + 3

      # Update highway type only for connection
      connection$highway <- connection_type

      # Store new lines
      processed_lines[[length(processed_lines) + 1]] <- segment1
      processed_lines[[length(processed_lines) + 1]] <- segment2
      processed_lines[[length(processed_lines) + 1]] <- connection

      # Mark line for removal
      lines_to_remove <- c(lines_to_remove, min_idx)
    } else {
      warning(sprintf("Point %d did not create a valid split", i))
    }
  }

  if (length(processed_lines) > 0) {
    # Remove processed lines and combine with new ones
    new_lines_sf <- do.call(rbind, processed_lines)
    updated_lines <- rbind(
      lines_sf[!seq_len(nrow(lines_sf)) %in% lines_to_remove,],
      new_lines_sf
    )

    # Create new ID column if needed
    if (!new_id_column %in% names(updated_lines)) {
      updated_lines[[new_id_column]] <- updated_lines[[internal_id]]
    }

    # Remove temporary internal ID
    updated_lines[[internal_id]] <- NULL

  } else {
    warning("No lines were successfully processed")
    updated_lines <- lines_sf
    # Create new ID column if needed
    if (!new_id_column %in% names(updated_lines)) {
      updated_lines[[new_id_column]] <- seq_len(nrow(updated_lines))
    }
  }

  return(updated_lines)
}
