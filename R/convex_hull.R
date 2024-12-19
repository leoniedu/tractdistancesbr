#' Calculate Memoized Convex Hull
#'
#' Calculate a convex hull using grDevices::chull with memoization and persistent local disk caching
#'
#' @param hull_tolerance Tolerance for simplifying the convex hull (optional)
#' @return A simplified convex hull polygon
#' @importFrom memoise memoise
#' @importFrom grDevices chull
#' @importFrom sf st_polygon st_sfc st_crs<-
#' @importFrom tools R_user_dir
#' @export
convex_hull <- function(coords, hull_tolerance = 0.01, crs = NA) {
  # Ensure coordinates are a matrix
  coords_matrix <- as.matrix(coords)
  chull_calc <- function(coords) {
    # Get convex hull indices
    hull_indices <- grDevices::chull(coords)

    # Extract hull coordinates
    hull_coords <- coords[hull_indices, , drop = FALSE]

    # Close the polygon by repeating the first point
    hull_coords <- rbind(hull_coords, hull_coords[1, ])

    # Create SF polygon
    hull_poly <- sf::st_polygon(list(hull_coords))

    return(hull_poly)
  }

  # Calculate the convex hull
  hull <- chull_calc(coords_matrix)

  # Simplify if tolerance is provided
  if (!is.null(hull_tolerance) && hull_tolerance > 0) {
    hull <- sf::st_simplify(sf::st_sfc(hull), dTolerance = hull_tolerance)
  } else {
    hull <- sf::st_sfc(hull)
  }

  # Set CRS if provided
  if (!is.na(crs)) {
    sf::st_crs(hull) <- crs
  }

  return(hull)
}

#' Optionally clear the memoization cache
#'
#' @export
clear_convex_hull_cache <- function() {
  if (exists("memoise_cache")) {
    memoise::forget(convex_hull_mem)
  }
}
