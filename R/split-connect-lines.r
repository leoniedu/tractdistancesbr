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
                                  connection_id = "CONN_1", 
                                  highway = "artificial",
                                  id_prefix = "SEG",
                                  ...) {
    # Validate primary inputs
    if (!inherits(line1_sf, "sf")) {
        stop("line1_sf must be an sf object")
    }
    if (!all(sapply(list(point1_sf, point2_sf), inherits, "sf"))) {
        stop("point1_sf and point2_sf must be sf objects")
    }
    
    # Ensure points are POINT geometry
    if (!all(sapply(list(st_geometry(point1_sf), st_geometry(point2_sf)), 
                    inherits, "POINT"))) {
        stop("point1_sf and point2_sf must have POINT geometry")
    }
    
    # Ensure lines are LINESTRING geometry
    if (!inherits(st_geometry(line1_sf), "LINESTRING")) {
        stop("line1_sf must have LINESTRING geometry")
    }
    
    # Validate line2_sf if provided
    if (!is.null(line2_sf)) {
        if (!inherits(line2_sf, "sf")) {
            stop("line2_sf must be an sf object")
        }
        if (!inherits(st_geometry(line2_sf), "LINESTRING")) {
            stop("line2_sf must have LINESTRING geometry")
        }
    }
    
    # Ensure all objects have the same CRS
    objects_to_check <- list(line1_sf, point1_sf, point2_sf)
    if (!is.null(line2_sf)) {
        objects_to_check <- c(objects_to_check, list(line2_sf))
    }
    crs_list <- lapply(objects_to_check, st_crs)
    if (!all(sapply(crs_list[-1], identical, crs_list[[1]]))) {
        stop("All input sf objects must have the same CRS")
    }
    
    # Initialize segment counter
    next_segment_id <- 1
    
    # Split first line at point
    split1 <- st_split(st_geometry(line1_sf), st_geometry(point1_sf))
    segments1 <- st_collection_extract(split1, "LINESTRING")
    
    # Create new line features list
    new_lines <- list()
    
    # Process first line segments
    if (length(segments1) > 0) {
        for (seg in segments1) {
            new_line <- line1_sf
            st_geometry(new_line) <- st_sfc(seg, crs = st_crs(line1_sf))
            new_line$segment_id <- paste0(id_prefix, "_", next_segment_id)
            new_line$original_id <- line1_sf$way_id  # preserve original ID
            next_segment_id <- next_segment_id + 1
            new_lines[[length(new_lines) + 1]] <- new_line
        }
    } else {
        new_line <- line1_sf
        new_line$segment_id <- paste0(id_prefix, "_", next_segment_id)
        new_line$original_id <- line1_sf$way_id
        next_segment_id <- next_segment_id + 1
        new_lines[[1]] <- new_line
    }
    
    # Process second line if provided
    if (!is.null(line2_sf)) {
        split2 <- st_split(st_geometry(line2_sf), st_geometry(point2_sf))
        segments2 <- st_collection_extract(split2, "LINESTRING")
        
        if (length(segments2) > 0) {
            for (seg in segments2) {
                new_line <- line2_sf
                st_geometry(new_line) <- st_sfc(seg, crs = st_crs(line2_sf))
                new_line$segment_id <- paste0(id_prefix, "_", next_segment_id)
                new_line$original_id <- line2_sf$way_id  # preserve original ID
                next_segment_id <- next_segment_id + 1
                new_lines[[length(new_lines) + 1]] <- new_line
            }
        } else {
            new_line <- line2_sf
            new_line$segment_id <- paste0(id_prefix, "_", next_segment_id)
            new_line$original_id <- line2_sf$way_id
            next_segment_id <- next_segment_id + 1
            new_lines[[length(new_lines) + 1]] <- new_line
        }
    }
    
    # Create connection line
    connection_coords <- rbind(
        st_coordinates(point1_sf)[1,],
        st_coordinates(point2_sf)[1,]
    )
    connection_geom <- st_linestring(connection_coords)
    
    # Create new connection with required attributes
    connection <- st_sf(
        geometry = st_sfc(connection_geom, crs = st_crs(line1_sf)),
        segment_id = connection_id,  # use connection_id as segment_id for connection
        original_id = NA,           # no original ID for artificial connection
        connection_id = connection_id,
        highway = highway,
        artificial = TRUE,
        ...
    )
    
    # Add connection to new lines and combine all
    new_lines[[length(new_lines) + 1]] <- connection
    result <- do.call(rbind, new_lines)
    
    return(result)
}
