library(leaflet)
library(sf)
library(dplyr)

visualize_network_connections <- function(lines_sf, artificial_edges_sf, id_roads="osm_id") {
  # Validate inputs
  if(!inherits(lines_sf, "sf")) stop("lines_sf must be an sf object")
  if(!inherits(artificial_edges_sf, "sf")) stop("artificial_edges_sf must be an sf object")

  # Transform to leaflet's CRS if needed
  if(st_crs(lines_sf)$epsg != 4326) {
    lines_sf <- st_transform(lines_sf, 4326)
    artificial_edges_sf <- st_transform(artificial_edges_sf, 4326)
  }
  connected_roads_sf <- lines_sf%>%semi_join(artificial_edges_sf%>%sf::st_drop_geometry(), by=id_roads)

  # Create base map
  m <- leaflet() %>%
    addTiles() %>%  # OpenStreetMap base layer
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")

  # Add connected original roads
  m <- m %>%
    addPolylines(
      data = connected_roads_sf,
      #data = lines_sf,
      color = "blue",
      weight = 2,
      opacity = 0.8,
      group = "Connected Roads",
      popup = ~paste("Original Road")
    )

  # Add artificial connections
  m <- m %>%
    addPolylines(
      data = artificial_edges_sf,
      color = "red",
      weight = 3,
      opacity = 0.8,
      dashArray = "5,10",
      group = "Artificial Connections",
      popup = "Artificial Connection"
    )

  # Add layer control
  m %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "CartoDB", "Satellite"),
      overlayGroups = c("Connected Roads", "Artificial Connections"),
      options = layersControlOptions(collapsed = FALSE)
    )
}
