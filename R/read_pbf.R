#' Read OpenStreetMap PBF File
#'
#' @param pbf_path Character string specifying the path to the OSM PBF file
#' @param layers Character vector specifying which OSM layers to read. Default is all layers.
#' @param quiet Logical, if TRUE suppresses status messages. Default is FALSE
#' @param extra_tags Character vector of additional OSM tags to extract. Default includes road-related attributes
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
#' }
read_pbf <- function(pbf_path,
                    quiet = FALSE,
                    extra_tags = c(
                      "highway", "surface", "maxspeed", "lanes", "bridge",
                      "tunnel", "oneway", "junction", "ref", "name"
                    ), read_multiline=FALSE, ...) {
  if (!file.exists(pbf_path)) {
    cli::cli_alert_danger("File does not exist:", pbf_path)
    stop()
  }

  lines_sf <- osmextract::oe_read(pbf_path, layer = "lines",
                                      query = "SELECT * FROM lines WHERE highway IS NOT NULL", extra_tags = extra_tags, ...)
  if (nrow(lines_sf)==0) stop("No lines found.")
  if (read_multiline) {
    multilines_sf <- osmextract::oe_read(pbf_path, layer = "lines",
                                         query = "SELECT * FROM multilinestrings WHERE highway IS NOT NULL", extra_tags = extra_tags, ...)
  } else {
    multilines_sf <- data.frame()
  }
  if (nrow(lines_sf)>0 && nrow(multilines_sf)>0) {
    if (!quiet) cli::cli_alert_info("Converting highway multilinestrings to linestrings...")
    # Convert multilines to lines
    multilines_as_lines <- sf::st_cast(multilines_sf, "LINESTRING")

    # Get common columns
    line_cols <- names(lines_sf)
    multiline_cols <- names(multilines_as_lines)
    common_cols <- intersect(line_cols, multiline_cols)
    if (!quiet) cli::cli_alert_info(sprintf("Common columns: %s", paste(common_cols, collapse = ", ")))
    # Combine with existing lines using only common columns
    lines_sf <- rbind(
      lines_sf[, common_cols],
      multilines_as_lines[, common_cols]
    )
    if (!quiet) cli::cli_alert_info("Lines and multilines combined successfully")
  }
  return(lines_sf)
}
