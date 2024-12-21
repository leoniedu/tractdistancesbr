snap_split_linestring <- function(linestring_sf, point_sf,
                                  initial_buffer = 1e-8,
                                  buffer_factor = 10, verbose=FALSE) {
  # Validate number of features in inputs
  if (nrow(linestring_sf) > 1) {
    warning("Multiple linestrings provided. Only the first will be used.")
    linestring_sf <- linestring_sf[1,]
  }
  if (nrow(point_sf) > 1) {
    warning("Multiple points provided. Only the first will be used.")
    point_sf <- point_sf[1,]
  }

  # Validate feature types
  if (!all(sf::st_geometry_type(linestring_sf) == "LINESTRING")) {
    stop("linestring_sf must contain LINESTRING geometry")
  }
  if (!all(sf::st_geometry_type(point_sf) == "POINT")) {
    stop("point_sf must contain POINT geometry")
  }

  # Validate inputs have same CRS
  if (!identical(sf::st_crs(linestring_sf), sf::st_crs(point_sf))) {
    stop("Linestring and point must have the same CRS")
  }

  # Find the point on the linestring via nearest points
  nearest_pt_line <- sf::st_nearest_points(linestring_sf, point_sf)
  split_point <- sf::st_cast(nearest_pt_line, "POINT")[1]  # Point on line
  split_point_sf <- sf::st_sf(
    geometry = sf::st_sfc(split_point, crs = sf::st_crs(linestring_sf))
  )

  # Progressive buffer expansion
  current_buffer <- initial_buffer
  n_segments <- 1  # Initialize to trigger while loop

  while (n_segments < 2) {
    # Create buffer around the split point
    buffer_point <- sf::st_buffer(split_point_sf, dist = current_buffer)

    # Try splitting
    split_result <- lwgeom::st_split(linestring_sf, buffer_point)
    split_lines <- sf::st_collection_extract(split_result, "LINESTRING")
    n_segments <- nrow(split_lines)

    # Increase buffer for next iteration if needed
    current_buffer <- current_buffer * buffer_factor
  }

  # Keep first segment, union the rest if needed
  first_segment <- split_lines[1,]

  if (n_segments > 2) {
    remaining_segments <- sf::st_union(split_lines[-1,])

    # Get all attributes from the original linestring
    attrs <- as.list(first_segment)
    attrs$geometry <- remaining_segments

    # Create new sf object with all original attributes
    remaining_segment <- do.call(sf::st_sf, attrs)

    # Combine into final result
    result <- dplyr::bind_rows(first_segment, remaining_segment)
  } else {
    result <- split_lines
  }

  # Calculate distances from input point to both segments
  distances <- sf::st_distance(point_sf, result)
  min_dist <- min(distances)

  if (verbose && (as.numeric(min_dist) > 0.0001)) {
    cli::cli_alert(sprintf("Input point is %g units away from nearest resulting segment",
                    as.numeric(min_dist)))
  }

  return(result)
}
