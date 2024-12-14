#' @title Street Network Analysis Functions
#' @name street_network
NULL

#' @describeIn street_network Connect points to road network
#' @export
connect_points <- function(streets, points, max_distance = Inf,
                           connection_profile = "footway",
                           highway_col = "highway",
                           id_col = "osm_id",
                           progress = TRUE) {

  # Input validation
  if (!inherits(points, "sf")) stop("points must be an sf object")
  if (!all(sf::st_geometry_type(points) == "POINT")) stop("points must have POINT geometry")
  if (!inherits(streets, "sf")) stop("streets must be an sf object")
  if (!all(sf::st_geometry_type(streets) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("streets must have LINESTRING or MULTILINESTRING geometry")
  }

  max_distance <- units::set_units(max_distance, "m")

  # Ensure same CRS
  streets <- sf::st_transform(streets, sf::st_crs(points))

  # Create minimal template for new connections
  template_df <- data.frame(
    highway = character(0),
    connection_id = integer(0)
  )
  names(template_df)[names(template_df) == "highway"] <- highway_col
  template_df[[id_col]] <- character(0)
  template_df[["distance_to_street"]] <- numeric(0)

  # Create empty geometry column
  empty_geom <- sf::st_sfc(crs = sf::st_crs(points))

  # Convert to sf object and explicitly set geometry column
  template <- sf::st_sf(template_df, geometry = empty_geom)
  sf::st_geometry(template) <- "geometry"

  # Process each point in parallel
  new_connections <- furrr::future_map(
    1:nrow(points),
    function(i) {
      point <- points[i,]
      dist <- min(sf::st_distance(point, streets))

      if (dist > max_distance) {
        warning(sprintf("Point %d is farther than max_distance from nearest road", i))
        return(NULL)
      }

      nearest_idx <- which.min(sf::st_distance(point, streets))
      nearest_street <- streets[nearest_idx,]
      nearest_line <- sf::st_cast(sf::st_nearest_points(point, nearest_street)[1], "LINESTRING")

      # Create new connection starting with template structure
      new_row <- template[1,]
      new_row$connection_id <- i
      new_row[[highway_col]] <- connection_profile
      new_row[[id_col]] <- paste0("artificial_", i)
      new_row[["distance_to_street"]] <- as.numeric(dist)

      # Explicitly set geometry
      geom_col <- attr(new_row, "sf_column")
      new_row[[geom_col]] <- sf::st_sfc(nearest_line, crs = sf::st_crs(points))
      sf::st_geometry(new_row) <- geom_col

      return(new_row)
    },
    .options = furrr::furrr_options(seed = TRUE),
    .progress = progress
  )

  # Remove NULL results
  new_connections <- new_connections[!sapply(new_connections, is.null)]

  if (length(new_connections) > 0) {
    # Combine results and ensure geometry column is set correctly
    result <- do.call(rbind, new_connections)
    sf::st_geometry(result) <- "geometry"
    return(result)
  }

  # Return empty template with geometry column properly set
  return(template)
}
