#' Plot Points, Nearest Streets and Artificial Connections over OpenStreetMap
#'
#' @param streets An sf object with LINESTRING geometry representing the street network
#' @param points An sf object with POINT geometry
#' @param id_col Character string specifying the column name containing unique identifiers. Default is "osm_id"
#' @param artificial_prefix Character string specifying the prefix used for artificial connections. Default is "artificial_"
#' @return A leaflet map object
#' @export
#' @importFrom leaflet leaflet addTiles addPolylines addCircleMarkers
#' @importFrom sf st_transform st_cast st_distance
#'
#' @examples
#' \dontrun{
#' # Read OSM streets
#' streets_sf <- read_pbf("path/to/file.osm.pbf")
#'
#' # Points as sf object
#' points_sf <- sf::st_as_sf(
#'   data.frame(id = 1:2),
#'   coords = c(-46.633, -23.550),
#'   crs = 4326
#' )
#'
#' # Add connections
#' streets_connected <- connect_points(streets_sf, points_sf)
#'
#' # Plot the points, nearest streets and their connections over OSM
#' plot_connections(streets_connected, points_sf)
#' }
plot_connections <- function(streets, points,
                           id_col = "osm_id",
                           artificial_prefix = "artificial_") {

  # Validate inputs
  if (!inherits(streets, "sf") || !inherits(points, "sf")) {
    stop("Both streets and points must be sf objects")
  }

  if (!id_col %in% names(streets)) {
    stop(sprintf("Column '%s' not found in streets", id_col))
  }

  # Ensure data is in WGS84 for leaflet
  streets <- sf::st_transform(streets, 4326)
  points <- sf::st_transform(points, 4326)

  # Ensure streets are LINESTRING
  streets <- sf::st_cast(streets, "LINESTRING")

  # Separate artificial connections and regular streets
  is_artificial <- grepl(artificial_prefix, streets[[id_col]])
  regular_streets <- streets[!is_artificial, ]

  # Find nearest street for each point
  nearest_streets <- do.call(rbind, lapply(seq_len(nrow(points)), function(i) {
    distances <- sf::st_distance(points[i,], regular_streets)
    nearest_idx <- which.min(distances)
    regular_streets[nearest_idx,]
  }))

  # Create the base map with nearest streets and points
  map <- leaflet::leaflet() %>%
    # Add OpenStreetMap tiles as base layer
    leaflet::addTiles() %>%
    # Add nearest streets
    leaflet::addPolylines(
      data = nearest_streets,
      color = "blue",
      weight = 3,
      opacity = 0.8
    ) %>%
    # Add points
    leaflet::addCircleMarkers(
      data = points,
      radius = 6,
      color = "purple",
      fillOpacity = 0.8,
      stroke = FALSE
    )

  # Add artificial connections if they exist
  if (any(is_artificial)) {
    artificial_connections <- streets[is_artificial, ]
    map <- map %>%
      leaflet::addPolylines(
        data = artificial_connections,
        color = "red",
        weight = 3,
        opacity = 1,
        dashArray = "5,10",
        label = ~get(id_col)
      )
  }

  return(map)
}
