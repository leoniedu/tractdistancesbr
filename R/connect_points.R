#' Connect Points to Road Network
#'
#' @param streets An sf object with LINESTRING geometry representing the street network
#' @param points An sf object with POINT geometry
#' @param max_distance Maximum distance in meters to search for nearest road segment. Default is Inf (no limit)
#' @param connection_profile Character string specifying the profile for the new connection. Default is "footway"
#' @param highway_col Character string specifying the column name containing highway types. Default is "highway"
#' @param id_col Character string specifying the column name containing unique identifiers. Default is "osm_id"
#' @return An sf object with the original streets plus new connections
#' @export
#' @importFrom sf st_nearest_points st_distance st_cast st_sfc st_crs st_geometry_type st_transform
#' @importFrom units set_units
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
#' }
connect_points <- function(streets, points, max_distance = Inf,
                         connection_profile = "footway",
                         highway_col = "highway",
                         id_col = "osm_id") {

  # Convert max_distance to units object in meters
  max_distance <- units::set_units(max_distance, "m")

  # Validate input points
  if (!inherits(points, "sf")) {
    stop("points must be an sf object")
  }

  if (!all(sf::st_geometry_type(points) == "POINT")) {
    stop("points must have POINT geometry")
  }

  # Validate input streets
  if (!inherits(streets, "sf")) {
    stop("streets must be an sf object")
  }

  if (!all(sf::st_geometry_type(streets) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("streets must have LINESTRING or MULTILINESTRING geometry")
  }

  if (!highway_col %in% names(streets)) {
    stop(sprintf("Column '%s' not found in streets", highway_col))
  }

  if (!id_col %in% names(streets)) {
    stop(sprintf("Column '%s' not found in streets", id_col))
  }

  # Ensure same CRS
  streets <- sf::st_transform(streets, sf::st_crs(points))

  # For each point
  new_connections <- list()
  for (i in seq_len(nrow(points))) {
    if ((i%%100)==0) {
      print(i)
    }
    point <- points[i,]

    # Find nearest line and its distance
    dist <- min(sf::st_distance(point, streets))

    if (dist > max_distance) {
      warning(sprintf("Point %d is farther than max_distance from nearest road", i))
      next
    }

    # Find the nearest street segment
    nearest_idx <- which.min(sf::st_distance(point, streets))
    nearest_street <- streets[nearest_idx,]

    # Create connection between point and nearest location on street
    nearest_line <- sf::st_cast(sf::st_nearest_points(point, nearest_street)[1], "LINESTRING")

    # Create attributes matching the streets structure
    attrs <- as.data.frame(streets[1,])[0,]  # Empty row with same structure
    attrs[1, highway_col] <- connection_profile
    attrs[1, id_col] <- paste0("artificial_", i)
    attrs[1, 'distance_to_street'] <- dist

    # Create sf object for new connection
    new_connection_sf <- sf::st_sf(
      attrs,
      geometry = sf::st_sfc(nearest_line, crs = sf::st_crs(points))
    )
    new_connections[[i]] <- new_connection_sf
  }

  # Combine all new connections with original streets
  if (length(new_connections) > 0) {
    new_connections_sf <- do.call(rbind, new_connections)
    combined_streets <- rbind(streets, new_connections_sf)
    return(combined_streets)
  }

  return(streets)
}

#' Calculate Distances to Optimal Path
#'
#' @param streets An sf object with LINESTRING geometry representing the street network
#' @param from_points An sf object with POINT geometry representing origin points
#' @param to_points An sf object with POINT geometry representing destination points
#' @param max_distance Maximum distance in meters to search for nearest road segment. Default is Inf (no limit)
#' @param id_col Character string specifying the column name containing unique identifiers. Default is "osm_id"
#' @return A data frame containing distances from each "from" point to the nearest point on the optimal path
#' @export
#' @importFrom sf st_nearest_points st_distance st_linestring st_line_sample st_cast
#' @importFrom units set_units
#'
#' @examples
#' \dontrun{
#' # Read OSM streets
#' streets_sf <- read_pbf("path/to/file.osm.pbf")
#'
#' # Create from and to points as sf objects
#' from_points <- sf::st_as_sf(
#'   data.frame(id = 1:3),
#'   coords = matrix(c(-46.633, -23.550, -46.634, -23.551, -46.635, -23.552),
#'                  ncol = 2, byrow = TRUE),
#'   crs = 4326
#' )
#'
#' to_points <- sf::st_as_sf(
#'   data.frame(id = 1:2),
#'   coords = matrix(c(-46.640, -23.555, -46.641, -23.556),
#'                  ncol = 2, byrow = TRUE),
#'   crs = 4326
#' )
#'
#' # Calculate distances
#' distances <- calculate_path_distances(streets_sf, from_points, to_points)
#' }
calculate_path_distances <- function(streets, from_points, to_points,
                                   max_distance = Inf, id_col = "osm_id") {

  # Convert max_distance to units object in meters
  max_distance <- units::set_units(max_distance, "m")

  # Validate input points
  if (!inherits(from_points, "sf") || !inherits(to_points, "sf")) {
    stop("from_points and to_points must be sf objects")
  }

  if (!all(sf::st_geometry_type(from_points) == "POINT") ||
      !all(sf::st_geometry_type(to_points) == "POINT")) {
    stop("from_points and to_points must have POINT geometry")
  }

  # Ensure same CRS
  if (sf::st_crs(from_points) != sf::st_crs(streets)) {
    from_points <- sf::st_transform(from_points, sf::st_crs(streets))
  }
  if (sf::st_crs(to_points) != sf::st_crs(streets)) {
    to_points <- sf::st_transform(to_points, sf::st_crs(streets))
  }

  # Connect points to network
  streets_with_from <- connect_points(streets, from_points, max_distance = max_distance)
  streets_with_all <- connect_points(streets_with_from, to_points, max_distance = max_distance)

  # Find optimal path between each to point and the network
  optimal_paths <- list()
  for (i in seq_len(nrow(to_points))) {
    to_point <- to_points[i,]
    nearest_line <- sf::st_nearest_points(to_point, streets_with_all)
    optimal_paths[[i]] <- nearest_line
  }

  # Combine paths
  all_paths <- do.call(sf::st_sfc, optimal_paths)

  # Calculate distances from each from point to the paths
  distances <- matrix(NA, nrow = nrow(from_points), ncol = length(optimal_paths))

  for (i in seq_len(nrow(from_points))) {
    for (j in seq_along(optimal_paths)) {
      # Calculate distance to path
      dist <- min(sf::st_distance(from_points[i,], optimal_paths[[j]]))
      distances[i,j] <- as.numeric(dist)
    }
  }

  # Create results data frame
  result <- data.frame(
    from_id = from_points[[id_col]],
    distances = distances
  )

  colnames(result)[-1] <- paste0("to_", to_points[[id_col]])

  return(result)
}
