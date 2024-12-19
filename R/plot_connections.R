#' Plot Artificial Connections and Their Connected Street Segments over OpenStreetMap
#'
#' @param connections An sf object output from connect_points() function
#' @param streets An sf object with LINESTRING geometry representing the street network
#' @param street_id_col Character string specifying the column name containing street identifiers
#' @param tracts_sf Optional; an sf object with POLYGON geometry representing census tracts
#' @param tract_label_col Optional; column name in tracts_sf to use for labels
#' @param tract_id Optional; column name present in both connections and tracts_sf to link them
#' @return A leaflet map object
#' @export
#' @importFrom leaflet leaflet addTiles addPolylines addPolygons
#' @importFrom sf st_transform st_cast
#'
plot_connections <- function(connections, streets, street_id_col,
                             tracts_sf = NULL, tract_label_col = NULL,
                             tract_id = NULL) {
  # Validate inputs
  if (!inherits(connections, "sf") || !inherits(streets, "sf")) {
    stop("Both connections and streets must be sf objects")
  }
  if (!street_id_col %in% names(streets) || !street_id_col %in% names(connections)) {
    stop(sprintf("Column '%s' not found in both streets and connections", street_id_col))
  }
  streets <- streets|>dplyr::semi_join(connections|>sf::st_drop_geometry(), by=street_id_col)

  # Validate tracts if provided
  if (!is.null(tracts_sf)) {
    if (is.null(tract_id)) stop("Must provide tract_id")
    if (!inherits(tracts_sf, "sf")) {
      stop("tracts_sf must be an sf object")
    }
    if (!is.null(tract_label_col) && !tract_label_col %in% names(tracts_sf)) {
      stop(sprintf("Column '%s' not found in tracts_sf", tract_label_col))
    }
    if (!is.null(tract_id)) {
      if (!tract_id %in% names(tracts_sf)) {
        stop(sprintf("Column '%s' not found in tracts_sf", tract_id))
      }
      if (!tract_id %in% names(connections)) {
        stop(sprintf("Column '%s' not found in connections", tract_id))
      }
    }
    if (is.null(tract_label_col)) {
      tract_label_col <- tract_id
    }
    tracts_sf <- tracts_sf|>dplyr::semi_join(connections|>sf::st_drop_geometry(), by=tract_id)
  }

  # Ensure data is in WGS84 for leaflet
  connections <- sf::st_transform(connections, 4326)
  streets <- sf::st_transform(streets, 4326)
  if (!is.null(tracts_sf)) {
    tracts_sf <- sf::st_transform(tracts_sf, 4326)
  }

  # Ensure streets are LINESTRING
  streets <- sf::st_cast(streets, "LINESTRING")

  # Create the base map
  map <- leaflet::leaflet() %>%
    # Add OpenStreetMap tiles as base layer
    leaflet::addTiles()

  # Add census tracts if provided
  if (!is.null(tracts_sf)) {
    map <- map %>%
      leaflet::addPolygons(
        data = tracts_sf,
        fillColor = "lightblue",
        fillOpacity = 0.3,
        weight = 1,
        color = "blue",
        label = ~if (!is.null(tract_label_col)) {
            get(tract_label_col)
        } else {
          ifelse(has_connections, "Has connections", "No connections")
        },
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 0.5,
          bringToFront = TRUE
        ),
        group = "Census Tracts"
      )
  }

  # Add street network and connections
  map <- map %>%
    # Add connected streets
    leaflet::addPolylines(
      data = streets,
      color = "blue",
      weight = 3,
      opacity = 0.8,
      label = ~get(street_id_col),
      group = "Streets"
    ) %>%
    # Add artificial connections
    leaflet::addPolylines(
      data = connections,
      color = "red",
      weight = 3,
      opacity = 1,
      dashArray = "5,10",
      label = ~{
        label <- paste("Street:", get(street_id_col))
        if (!is.null(tract_id)) {
          label <- paste(label, "\nTract:", get(tract_id))
        }
        label
      },
      group = "Connections"
    ) %>%
    # Add layer controls
    leaflet::addLayersControl(
      overlayGroups = c("Census Tracts", "Streets", "Connections"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  return(map)
}
