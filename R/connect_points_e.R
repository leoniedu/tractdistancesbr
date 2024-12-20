#' Connect Points to Street Network with Topology
#'
#' @description
#' Creates connections from points to a street network while maintaining network topology.
#' Each connection splits the target street segment and creates a new connecting line.
#'
#' @param streets sf object containing the street network (LINESTRING geometry)
#' @param points sf object containing points to connect (POINT geometry)
#' @param max_distance Maximum allowed distance for connections (numeric, in meters)
#' @param connection_profile Type of connection to create (e.g., "footway", "service")
#' @param highway_col Name of column containing street types (default: "highway")
#' @param street_id_col Name of column containing unique street identifiers
#' @param point_id_col Name of column containing unique point identifiers
#' @param segment_prefix Prefix for new segment IDs (default: "SEG")
#' @param connection_prefix Prefix for new connection IDs (default: "CONN")
#' @param waterways Optional sf object containing waterways to check for crossings
#' @param waterway_col Name of column containing waterway types (default: "waterway")
#' @param progress Show progress bar (default: TRUE)
#'
#' @return An sf object containing the updated street network including new connections
#'
#' @export
connect_points_e <- function(streets, points,
                             max_distance = Inf,
                             connection_profile = "footway",
                             highway_col = "highway",
                             street_id_col,
                             point_id_col,
                             segment_prefix = "SEG",
                             connection_prefix = "CONN",
                             waterways = NULL,
                             waterway_col = "waterway",
                             progress = TRUE) {

  # Input validation
  if (!inherits(points, "sf")) stop("points must be an sf object")
  if (!all(sf::st_geometry_type(points) == "POINT"))
    stop("points must have POINT geometry")
  if (!inherits(streets, "sf")) stop("streets must be an sf object")
  if (!all(sf::st_geometry_type(streets) %in% c("LINESTRING", "MULTILINESTRING")))
    stop("streets must have LINESTRING or MULTILINESTRING geometry")
  if (!street_id_col %in% names(streets))
    stop(sprintf("Column '%s' not found in streets", street_id_col))
  if (!point_id_col %in% names(points))
    stop(sprintf("Column '%s' not found in points", point_id_col))

  # Convert MULTILINESTRING to LINESTRING if needed
  if (any(sf::st_geometry_type(streets) == "MULTILINESTRING")) {
    streets <- sf::st_cast(streets, "LINESTRING")
  }

  # Validate waterways if provided
  if (!is.null(waterways)) {
    if (!inherits(waterways, "sf")) stop("waterways must be an sf object")
    if (!waterway_col %in% names(waterways)) {
      stop(sprintf("Column '%s' not found in waterways", waterway_col))
    }
    waterways <- sf::st_transform(waterways, sf::st_crs(streets))
  }

  max_distance <- units::set_units(max_distance, "m")

  # Transform points to streets CRS if needed
  points <- sf::st_transform(points, sf::st_crs(streets))
  browser()
  # Extract data needed by all workers
  shared_data <- list(
    streets = streets,
    points_geom = sf::st_geometry(points),
    points_ids = points[[point_id_col]],
    streets_crs = sf::st_crs(streets),
    max_distance = max_distance,
    connection_profile = connection_profile,
    highway_col = highway_col,
    street_id_col = street_id_col,
    point_id_col = point_id_col,
    waterways = waterways,
    waterway_col = waterway_col,
    segment_prefix = segment_prefix,
    connection_prefix = connection_prefix
  )

  # Process points in parallel with shared data
  new_connections <- furrr::future_map(
    1:nrow(points),
    function(i) {
      tryCatch({
        point <- sf::st_sf(geometry = shared_data$points_geom[i])

        # Calculate distance
        dist <- sf::st_distance(point, shared_data$streets)

        if (min(dist) > shared_data$max_distance) {
          warning(sprintf("Point %d is farther than max_distance from nearest road", i))
          return(NULL)
        }

        # Find nearest street and create connection
        nearest_idx <- which.min(dist)
        nearest_street <- shared_data$streets[nearest_idx,]
        browser()
        # Use connect_points_to_lines for single point
        point_connection <- connect_points_to_lines(
          points_sf = point,
          lines_sf = nearest_street,
          connection_type = shared_data$connection_profile,
          new_id_column = shared_data$street_id_col
        )

        # Add waterway crossing information if needed
        if (!is.null(shared_data$waterways)) {
          connections <- point_connection[point_connection[[shared_data$highway_col]] == shared_data$connection_profile,]
          intersections <- sf::st_intersects(connections, shared_data$waterways)
          waterway_crossings <- sapply(intersections, function(x) {
            if (length(x) > 0) {
              paste(unique(shared_data$waterways[[shared_data$waterway_col]][x]), collapse = ";")
            } else {
              NA_character_
            }
          })
          point_connection$waterway_crossing <- NA_character_
          point_connection$waterway_crossing[point_connection[[shared_data$highway_col]] == shared_data$connection_profile] <- waterway_crossings
        }

        # Add distance information
        point_connection$distance_to_street <- NA_real_
        conn_idx <- point_connection[[shared_data$highway_col]] == shared_data$connection_profile
        point_connection$distance_to_street[conn_idx] <- sf::st_length(point_connection[conn_idx,])

        # Add metadata
        point_connection$original_id <- NA_character_
        point_connection$artificial <- FALSE
        point_connection$artificial[conn_idx] <- TRUE

        return(point_connection)

      }, error = function(e) {
        message("Error processing point ", i, ": ", e$message)
        return(NULL)
      })
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = list(shared_data = shared_data)
    ),
    .progress = progress
  )

  # Remove NULL results
  new_connections <- new_connections[!sapply(new_connections, is.null)]

  # Handle connect_points_to_lines errors
  new_connections <- new_connections[sapply(new_connections, function(x) {
    inherits(x, "sf") && nrow(x) > 0
  })]

  if (length(new_connections) == 0) {
    # Return empty sf object if no connections
    empty_result <- data.frame(
      distance_to_street = numeric(0),
      waterway_crossing = character(0),
      original_id = character(0),
      artificial = logical(0)
    )
    empty_result[[highway_col]] <- character(0)
    empty_result[[point_id_col]] <- character(0)
    empty_result[[street_id_col]] <- character(0)

    return(sf::st_sf(
      empty_result,
      geometry = sf::st_sfc(crs = shared_data$streets_crs)
    ))
  }

  # Combine results using bind_rows
  result <- dplyr::bind_rows(new_connections)

  # Format IDs according to prefixes
  old_ids <- result[[street_id_col]]
  new_ids <- character(length(old_ids))

  # Segment IDs
  segment_idx <- !is.na(old_ids) & result[[highway_col]] != connection_profile
  new_ids[segment_idx] <- paste0(segment_prefix, "_", old_ids[segment_idx])

  # Connection IDs
  conn_idx <- result[[highway_col]] == connection_profile
  new_ids[conn_idx] <- paste0(connection_prefix, "_", seq_len(sum(conn_idx)))

  result[[street_id_col]] <- new_ids

  return(result)
}
