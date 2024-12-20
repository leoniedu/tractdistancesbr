#' Convert Mixed Geometry Types to Lines or Points
#'
#' @description
#' Converts an sf object containing mixed geometry types into a consistent geometry type,
#' either LINESTRING or POINT. The function processes POLYGON and MULTIPOLYGON geometries
#' by converting them to the specified output geometry type while preserving other attributes.
#'
#' @param m An sf object that may contain mixed geometry types
#' @param geom_out Character string specifying the desired output geometry type.
#'        Must be either "POINT" or "LINESTRING". Default is "POINT".
#'
#' @return An sf object with:
#'   * Consistent geometry type (either all LINESTRING or all POINT)
#'   * All original attributes preserved
#'   * Empty columns removed
#'   * New sequential ID column added
#'
#' @details
#' The function performs the following steps:
#' 1. Identifies geometry types in the input sf object
#' 2. Separates geometries that need conversion
#' 3. Converts POLYGON/MULTIPOLYGON to specified geometry type
#' 4. Combines converted geometries with original ones
#' 5. Cleans up empty columns and adds sequential IDs
#'
#' @examples
#' \dontrun{
#' # Convert mixed geometries to points
#' points_sf <- m2poly(mixed_sf, geom_out = "POINT")
#'
#' # Convert mixed geometries to lines
#' lines_sf <- m2poly(mixed_sf, geom_out = "LINESTRING")
#' }
#'
#' @importFrom sf st_cast
#' @importFrom dplyr bind_rows
#' @importFrom janitor remove_empty
#'
#' @export
m2poly <- function(m, geom_out = "POINT") {
  # Input validation
  if (!inherits(m, "sf")) {
    stop("Input 'm' must be an sf object")
  }

  valid_geom_types <- c("POINT", "LINESTRING")
  if (!geom_out %in% valid_geom_types) {
    stop(sprintf("geom_out must be one of: %s",
                 paste(valid_geom_types, collapse = ", ")))
  }

  # Check if input has geometries
  if (nrow(m) == 0) {
    warning("Input sf object is empty")
    return(m)
  }

  # Identify geometry types
  s <- sapply(m$geometry, function(z) class(z)[2])

  # Print summary of geometry types (useful for debugging)
  type_counts <- table(s)
  if (length(type_counts) > 0) {
    message("Input geometry types:")
    print(type_counts)
  }

  # Separate geometries that need conversion
  # (everything except LINESTRING and target geometry type)
  mpoly <- m[!s %in% c("LINESTRING", geom_out), ]

  # If there are geometries to convert
  if (nrow(mpoly) > 0) {
    # Convert to target geometry type
    mpoly2lines <- try(sf::st_cast(mpoly, "LINESTRING"), silent = TRUE)

    if (inherits(mpoly2lines, "try-error")) {
      warning("Error converting some geometries to LINESTRING")
      return(m)
    }

    # Combine converted geometries with original ones that didn't need conversion
    m <- dplyr::bind_rows(
      m[s %in% c("LINESTRING"), ],
      mpoly2lines
    )
  }

  # Clean up and add ID
  m <- janitor::remove_empty(m, which = "cols")
  m$id <- seq_len(nrow(m))

  return(m)
}
