#' Crop OSM Data to a Bounding Box
#'
#' @param osm_data An osmdata_sc object as returned by read_pbf
#' @param bbox A numeric vector of length 4 (xmin, ymin, xmax, ymax) or an sf/sfc object
#' @return A cropped osmdata_sc object
#' @export
#' @importFrom sf st_crop st_bbox st_as_sfc
#'
#' @examples
#' \dontrun{
#' # Read OSM data
#' osm_data <- read_pbf("path/to/file.osm.pbf")
#'
#' # Crop to bounding box
#' bbox <- c(xmin = -46.7, ymin = -23.6, xmax = -46.6, ymax = -23.5)
#' osm_cropped <- crop_osm(osm_data, bbox)
#'
#' # Or crop using an sf object
#' polygon <- sf::st_read("path/to/polygon.gpkg")
#' osm_cropped <- crop_osm(osm_data, polygon)
#' }
crop_osm <- function(osm_data, bbox) {
  # Check input
  if (!inherits(osm_data, "osmdata_sc")) {
    stop("osm_data must be an osmdata_sc object")
  }

  # Convert bbox to sfc if it's numeric
  if (is.numeric(bbox)) {
    if (length(bbox) != 4) {
      stop("bbox must be a numeric vector of length 4 (xmin, ymin, xmax, ymax)")
    }
    bbox <- sf::st_bbox(bbox)
    bbox <- sf::st_as_sfc(bbox)
  }

  # Function to safely crop a layer
  safe_crop <- function(layer) {
    if (is.null(layer) || nrow(layer) == 0) return(layer)
    tryCatch({
      sf::st_crop(layer, bbox)
    }, error = function(e) {
      warning("Error cropping layer: ", e$message)
      return(layer)
    })
  }

  # Crop each layer
  cropped <- structure(
    list(
      points = safe_crop(osm_data$points),
      lines = safe_crop(osm_data$lines),
      multilines = safe_crop(osm_data$multilines),
      multipolygons = safe_crop(osm_data$multipolygons)
    ),
    class = c("osmdata_sc", "osmdata")
  )

  return(cropped)
}
