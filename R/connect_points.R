#' @title Street Network Analysis Functions
#' @name street_network
NULL

#' @describeIn street_network Connect points to road network
#' @export
connect_points <- function(streets, points, max_distance = Inf,
                           connection_profile = "footway",
                           highway_col = "highway",
                           street_id_col,
                           point_id_col,
                           waterways = NULL,
                           waterway_col = "waterway",
                           progress = TRUE) {

  # Input validation
  if (!inherits(points, "sf")) stop("points must be an sf object")
  if (!all(sf::st_geometry_type(points) == "POINT")) stop("points must have POINT geometry")
  if (!inherits(streets, "sf")) stop("streets must be an sf object")
  if (!all(sf::st_geometry_type(streets) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("streets must have LINESTRING or MULTILINESTRING geometry")
  }
  if (!street_id_col %in% names(streets)) {
    stop(sprintf("Column '%s' not found in streets", street_id_col))
  }
  if (!point_id_col %in% names(points)) {
    stop(sprintf("Column '%s' not found in points", point_id_col))
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

  # Transform points to streets CRS
  points <- sf::st_transform(points, sf::st_crs(streets))

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
    waterway_col = waterway_col
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

        # Find nearest street
        nearest_idx <- which.min(dist)
        nearest_street <- shared_data$streets[nearest_idx,]
        nearest_line <- sf::st_cast(sf::st_nearest_points(point, nearest_street)[1], "LINESTRING")

        # Check for water crossings if waterways provided
        waterway_type <- NA_character_
        if (!is.null(shared_data$waterways)) {
          connection_sf <- sf::st_sf(geometry = sf::st_sfc(nearest_line, crs = shared_data$streets_crs))
          intersections <- sf::st_intersects(connection_sf, shared_data$waterways)
          if (length(intersections[[1]]) > 0) {
            waterway_types <- shared_data$waterways[[shared_data$waterway_col]][intersections[[1]]]
            waterway_type <- paste(unique(waterway_types), collapse = ";")
          }
        }

        # Create result data frame
        result <- data.frame(
          distance_to_street = as.numeric(dist),
          waterway_type = waterway_type
        )
        result[[shared_data$highway_col]] <- shared_data$connection_profile
        result[[shared_data$point_id_col]] <- shared_data$points_ids[i]
        result[[shared_data$street_id_col]] <- nearest_street[[shared_data$street_id_col]]

        # Create sf object with streets CRS
        sf::st_sf(
          result,
          geometry = sf::st_sfc(nearest_line, crs = shared_data$streets_crs)
        )

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

  if (length(new_connections) > 0) {
    # Combine results
    result <- do.call(rbind, new_connections)
    return(result)
  }

  # Return empty sf object if no connections
  empty_result <- data.frame(
    distance_to_street = numeric(0),
    waterway_type = character(0)
  )
  empty_result[[highway_col]] <- character(0)
  empty_result[[point_id_col]] <- character(0)
  empty_result[[street_id_col]] <- character(0)

  return(sf::st_sf(
    empty_result,
    geometry = sf::st_sfc(crs = shared_data$streets_crs)
  ))
}
