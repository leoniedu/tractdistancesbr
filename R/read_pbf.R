#' Read OpenStreetMap PBF File
#'
#' @param pbf_path Character string specifying the path to the OSM PBF file
#' @param layers Character vector specifying which OSM layers to read. Default is all layers.
#' @param quiet Logical, if TRUE suppresses status messages. Default is FALSE
#' @param extra_tags Character vector of additional OSM tags to extract. Default includes road-related attributes
#' @param for_dodgr Logical, if TRUE combines lines and multilinestrings into a single LINESTRING layer. Default is FALSE.
#' @return An osmdata_sc object containing OSM data in Simple Features format
#' @export
#' @importFrom osmextract oe_read
#' @importFrom sf st_cast st_collection_extract
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger
#'
#' @examples
#' \dontrun{
#' # Read all layers from a PBF file
#' osm_data <- read_pbf("path/to/file.osm.pbf")
#'
#' # Read only lines and multilines, combining them for dodgr
#' osm_data <- read_pbf("path/to/file.osm.pbf",
#'                      layers = c("lines", "multilinestrings"),
#'                      for_dodgr = TRUE)
#'
#' # Read only points
#' osm_data <- read_pbf("path/to/file.osm.pbf",
#'                      layers = "points")
#' }
read_pbf <- function(pbf_path,
                    layers = c("points", "lines", "multilinestrings", "multipolygons"),
                    quiet = FALSE,
                    extra_tags = c(
                      "highway", "surface", "maxspeed", "lanes", "bridge",
                      "tunnel", "oneway", "junction", "ref", "name"
                    ),
                    for_dodgr = FALSE) {
  
  if (!file.exists(pbf_path)) {
    cli::cli_alert_danger("File does not exist:", pbf_path)
    stop()
  }

  # Validate layers parameter
  valid_layers <- c("points", "lines", "multilinestrings", "multipolygons")
  invalid_layers <- setdiff(layers, valid_layers)
  if (length(invalid_layers) > 0) {
    stop("Invalid layers specified: ", paste(invalid_layers, collapse = ", "),
         "\nValid layers are: ", paste(valid_layers, collapse = ", "))
  }

  if (!quiet) cli::cli_alert_info("Reading OSM PBF file...")

  # Initialize empty lists for each layer
  osm_layers <- list(
    points = NULL,
    lines = NULL,
    multilines = NULL,
    multipolygons = NULL
  )

  # Read specified layers
  if ("points" %in% layers) {
    osm_layers$points <- osmextract::oe_read(
      file_path = pbf_path,
      layer = "points",
      quiet = quiet,
      extra_tags = extra_tags
    )
  }

  # Handle lines and multilines
  has_lines <- "lines" %in% layers
  has_multilines <- "multilinestrings" %in% layers

  if (has_lines) {
    osm_layers$lines <- osmextract::oe_read(
      file_path = pbf_path,
      layer = "lines",
      quiet = quiet,
      extra_tags = extra_tags
    )
    # Filter for highways
    osm_layers$lines <- osm_layers$lines[!is.na(osm_layers$lines$highway), ]
    if (!quiet) cli::cli_alert_info(sprintf("Found %d highways in lines", nrow(osm_layers$lines)))
  }

  if (has_multilines) {
    osm_layers$multilines <- osmextract::oe_read(
      file_path = pbf_path,
      layer = "multilinestrings",
      quiet = quiet,
      extra_tags = extra_tags
    )
    # Check if there are any highways in multilines
    highway_multilines <- !is.na(osm_layers$multilines$highway)
    if (any(highway_multilines)) {
      if (!quiet) cli::cli_alert_info(sprintf("Found %d highways in multilinestrings", sum(highway_multilines)))
      osm_layers$multilines <- osm_layers$multilines[highway_multilines, ]
    } else {
      if (!quiet) cli::cli_alert_info("No highways found in multilinestrings, skipping...")
      osm_layers$multilines <- NULL
    }
  }

  # If for_dodgr is TRUE and we have both lines and multilines with highways, combine them
  if (for_dodgr && has_lines && !is.null(osm_layers$multilines)) {
    if (!quiet) cli::cli_alert_info("Converting highway multilinestrings to linestrings...")
    
    # Convert multilines to lines
    multilines_as_lines <- sf::st_cast(osm_layers$multilines, "LINESTRING")
    
    # Get common columns
    line_cols <- names(osm_layers$lines)
    multiline_cols <- names(multilines_as_lines)
    common_cols <- intersect(line_cols, multiline_cols)
    
    if (!quiet) cli::cli_alert_info(sprintf("Common columns: %s", paste(common_cols, collapse = ", ")))
    
    # Combine with existing lines using only common columns
    osm_layers$lines <- rbind(
      osm_layers$lines[, common_cols],
      multilines_as_lines[, common_cols]
    )
    
    # Set multilines to NULL since they're now included in lines
    osm_layers$multilines <- NULL
    
    if (!quiet) cli::cli_alert_info("Lines and multilines combined successfully")
  }

  if ("multipolygons" %in% layers) {
    osm_layers$multipolygons <- osmextract::oe_read(
      file_path = pbf_path,
      layer = "multipolygons",
      quiet = quiet,
      extra_tags = extra_tags
    )
  }

  # Create an osmdata_sc-like structure
  osm_data <- structure(
    list(
      points = osm_layers$points,
      lines = osm_layers$lines,
      multilines = osm_layers$multilines,
      multipolygons = osm_layers$multipolygons
    ),
    class = c("osmdata_sc", "osmdata")
  )

  if (!quiet) cli::cli_alert_success("OSM data loaded successfully")

  return(osm_data)
}

#' Read OpenStreetMap PBF File optimized for dodgr
#'
#' A wrapper around read_pbf that sets appropriate defaults for use with dodgr.
#' Only reads lines and multilinestrings, combining them into a single LINESTRING layer.
#'
#' @inheritParams read_pbf
#' @return An osmdata_sc object containing OSM data in Simple Features format, with only the lines layer populated
#' @export
#' @examples
#' \dontrun{
#' # Read street network for dodgr
#' osm_data <- read_pbf_dodgr("path/to/file.osm.pbf")
#' graph <- dodgr::weight_streetnet(osm_data$lines)
#' }
read_pbf_dodgr <- function(pbf_path,
                          quiet = FALSE,
                          extra_tags = c(
                            "highway", "surface", "maxspeed", "lanes", "bridge",
                            "tunnel", "oneway", "junction", "ref", "name"
                          )) {
  read_pbf(
    pbf_path = pbf_path,
    layers = c("lines", "multilinestrings"),
    quiet = quiet,
    extra_tags = extra_tags,
    for_dodgr = TRUE
  )
}
