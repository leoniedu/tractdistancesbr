#' Split and Connect Network Lines
#'
#' @description
#' Creates a connection from a line to either another line or a point by splitting
#' the line(s) at specified points and generating a new connecting segment.
#' Each resulting segment gets a unique identifier.
#'
#' @param line1_sf sf object with LINESTRING geometry representing the first line
#' @param point1_sf sf object with POINT geometry where line1 should be split
#' @param point2_sf sf object with POINT geometry for the connection endpoint
#' @param line2_sf sf object with LINESTRING geometry representing the second line,
#'        or NULL if connecting to a point (default: NULL)
#' @param connection_id character string identifying the new connection
#' @param highway character string specifying the type of connection
#' @param id_prefix character string to prefix generated segment IDs (default: "SEG")
#' @param ... additional attributes to be added to the connection segment
#'
#' @return An sf object containing all segments including the new connection.
#'         Each feature has a unique segment_id.
#'
#' @export
split_and_connect_lines <- function(line1_sf, point1_sf, point2_sf,
                                    line2_sf = NULL,
                                    highway = "artificial",
                                    edge_marker_col,
                                    ...) {
    if(is.null(edge_marker_col)) {
        stop("edge_marker_col is required to track new edges")
    }
    if (!inherits(line1_sf, "sf")) {
        stop("line1_sf must be an sf object")
    }
    if (!all(sapply(list(point1_sf, point2_sf), inherits, "sf"))) {
        stop("point1_sf and point2_sf must be sf objects")
    }

    check_geometry <- function(x, geometry_type) {
        all(st_geometry_type(x) == geometry_type)
    }

    # Geometry checks remain the same...
    if (!all(sapply(list(point1_sf, point2_sf), check_geometry, "POINT"))) {
        stop("point1_sf and point2_sf must have POINT geometry")
    }
    if (!check_geometry(line1_sf, "LINESTRING")) {
        stop("line1_sf must have LINESTRING geometry")
    }
    if (!is.null(line2_sf)) {
        if (!inherits(line2_sf, "sf")) {
            stop("line2_sf must be an sf object")
        }
        if (!check_geometry(line2_sf, "LINESTRING")) {
            stop("line2_sf must have LINESTRING geometry")
        }
    }

    # CRS check remains the same...
    objects_to_check <- list(line1_sf, point1_sf, point2_sf)
    if (!is.null(line2_sf)) {
        objects_to_check <- c(objects_to_check, list(line2_sf))
    }
    crs_list <- lapply(objects_to_check, st_crs)
    if (!all(sapply(crs_list[-1], identical, crs_list[[1]]))) {
        stop("All input sf objects must have the same CRS")
    }

    # Initialize new lines list
    new_lines <- list()

    # Split first line - all attributes are preserved
    segments1 <- snap_split_linestring(line1_sf, point1_sf)
    # Mark as not new
    segments1[[edge_marker_col]] <- FALSE
    new_lines <- c(new_lines, list(segments1))

    # Create connection line
    connection_coords <- rbind(
        st_coordinates(point1_sf)[1,],
        st_coordinates(point2_sf)[1,]
    )
    connection_geom <- st_linestring(connection_coords)

    # Create connection with minimal attributes
    connection <- st_sf(
        geometry = st_sfc(connection_geom, crs = st_crs(line1_sf)),
        way_id = NA,  # No way_id for artificial connection
        highway = highway,
        artificial = TRUE,  # This might exist in the input but we know this is a new edge
        ...
    )
    # Mark as new
    connection[[edge_marker_col]] <- TRUE

    # Process second line if provided
    if (!is.null(line2_sf)) {
        segments2 <- snap_split_linestring(line2_sf, point2_sf)
        # Mark as not new
        segments2[[edge_marker_col]] <- FALSE
        new_lines <- c(new_lines, list(segments2))
    }

    # Add connection and combine all
    new_lines <- c(new_lines, list(connection))
    result <- bind_rows(new_lines)

    return(result)
}
