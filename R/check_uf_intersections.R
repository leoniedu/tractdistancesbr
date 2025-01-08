#' Check Spatial Intersections Between Features
#'
#' @description
#' This function analyzes spatial intersections between administrative units (uf_data)
#' and map features (map_data), aggregating results based on graph components.
#'
#' @param uf_data sf object containing administrative unit geometries
#' @param map_data sf object containing map features with an 'id' column
#' @param graph_data data.frame or tibble containing network components with 'way_id' and 'component' columns
#'
#' @return A data.frame or tibble matching the structure of graph_data with an additional
#'         'intersect_uf' column indicating whether any part of each component intersects
#'         with the administrative units
#'
#' @importFrom sf st_transform st_crs st_intersects st_drop_geometry
#' @importFrom dplyr mutate left_join distinct group_by summarise %>%
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # Create sample administrative units (two polygons)
#' uf_data <- st_sfc(
#'   st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0)))),
#'   st_polygon(list(rbind(c(3,0), c(5,0), c(5,2), c(3,2), c(3,0))))
#' ) %>%
#'   st_sf(geometry = ., name = c("A", "B")) %>%
#'   st_set_crs(4326)
#'
#' # Create sample map features (three lines)
#' map_data <- st_sfc(
#'   st_linestring(rbind(c(0.5,0.5), c(1.5,1.5))),
#'   st_linestring(rbind(c(3.5,0.5), c(4.5,1.5))),
#'   st_linestring(rbind(c(6,0.5), c(7,1.5)))
#' ) %>%
#'   st_sf(geometry = ., id = 1:3) %>%
#'   st_set_crs(4326)
#'
#' # Create sample graph components
#' graph_data <- tibble(
#'   way_id = as.character(1:3),
#'   component = c(1, 1, 2)
#' )
#'
#' # Run the intersection check
#' result <- check_uf_intersections(uf_data, map_data, graph_data)
#' print(result)
#' # Expected output will show component 1 intersecting (TRUE) and component 2 not intersecting (FALSE)
#' }
#'
#' @export
check_uf_intersections <- function(uf_data, map_data, graph_data, map_way_id="id") {
  # Input validation
  if (!inherits(uf_data, "sf")) {
    stop("uf_data must be an sf object")
  }

  if (!inherits(map_data, "sf")) {
    stop("map_data must be an sf object")
  }

  if (!inherits(graph_data, "data.frame")) {
    stop("graph_data must be a data.frame or tibble")
  }

  # Check for required columns
  if (!"id" %in% names(map_data)) {
    stop("map_data must contain an 'id' column")
  }

  if (!all(c("way_id", "component") %in% names(graph_data))) {
    stop("graph_data must contain 'way_id' and 'component' columns")
  }

  # Check that CRS is defined for spatial objects
  if (is.na(st_crs(uf_data))) {
    stop("uf_data must have a defined coordinate reference system")
  }

  if (is.na(st_crs(map_data))) {
    stop("map_data must have a defined coordinate reference system")
  }

  # Check for empty geometries
  if (any(st_is_empty(uf_data))) {
    warning("uf_data contains empty geometries")
  }

  if (any(st_is_empty(map_data))) {
    warning("map_data contains empty geometries")
  }

  # Transform UF to match map_data's coordinate reference system
  uf_transformed <- st_transform(uf_data, crs = st_crs(map_data))

  # Create modified map dataset with way_id and join with graph components
  map_components <- map_data %>%
    inner_join(
      graph_data %>%
        as_tibble() %>%
        distinct(way_id, component),
      by = setNames("way_id", map_way_id)
    )

  # Check for any unmatched features after join
  if (any(is.na(map_components$component))) {
    warning("Some features in map_data have no matching component in graph_data")
  }

  # Check for intersections between map features and UF
  intersections <- st_intersects(map_components, uf_transformed)
  map_components$intersect_uf <- sapply(intersections, function(x) length(x) > 0)

  # Group by component and check if any intersection exists
  grouped_intersections <- map_components %>%
    sf::st_drop_geometry() %>%
    group_by(component) %>%
    summarise(intersect_uf = any(intersect_uf))%>%
    filter(intersect_uf)

  # Join back to original graph data
  result_graph <- graph_data[graph_data$component%in%grouped_intersections$component,]

  result_map <- map_components %>%
    inner_join(grouped_intersections, by = "component")%>%
    select(all_of(names(map_data)), "component")

  return(list(map=result_map, graph=result_graph))
}
