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

    check_geometry <- function(x, geometry_type) {
        all(st_geometry_type(x) == geometry_type)
    }

    # Ensure points are POINT geometry
    if (!all(sapply(list(point1_sf, point2_sf), check_geometry, "POINT"))) {
        stop("point1_sf and point2_sf must have POINT geometry")
    }

    # Ensure lines are LINESTRING geometry
    if (!check_geometry(line1_sf, "LINESTRING")) {
        stop("line1_sf must have LINESTRING geometry")
    }

    # Validate line2_sf if provided
    if (!is.null(line2_sf)) {
        if (!inherits(line2_sf, "sf")) {
            stop("line2_sf must be an sf object")
        }
        if (!check_geometry(line2_sf, "LINESTRING")) {
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

    # Initialize new lines list
    new_lines <- list()
    next_segment_id <- 1

    # Split first line using snap_split_linestring
    segments1 <- snap_split_linestring(line1_sf, point1_sf)

    # Process first line segments - preserve all attributes
    for (i in 1:nrow(segments1)) {
        new_line <- segments1[i,]
        # Add new columns while preserving all original attributes
        new_line$segment_id <- paste0(id_prefix, "_", next_segment_id)
        # way_id is kept from original line since we preserved all attributes
        new_line$artificial <- FALSE  # These are real segments
        next_segment_id <- next_segment_id + 1
        new_lines[[length(new_lines) + 1]] <- new_line
    }

    # Create connection line
    connection_coords <- rbind(
        st_coordinates(point1_sf)[1,],
        st_coordinates(point2_sf)[1,]
    )
    connection_geom <- st_linestring(connection_coords)

    # Create new connection with minimal attributes
    connection <- st_sf(
        geometry = st_sfc(connection_geom, crs = st_crs(line1_sf)),
        segment_id = connection_id,
        way_id = NA,  # No original way_id
        highway = highway,  # Use specified connection type
        artificial = TRUE,  # This is an artificial connection
        ...
    )

    # Process second line if provided
    if (!is.null(line2_sf)) {
        segments2 <- snap_split_linestring(line2_sf, point2_sf)

        for (i in 1:nrow(segments2)) {
            new_line <- segments2[i,]
            # Add new columns while preserving all original attributes
            new_line$segment_id <- paste0(id_prefix, "_", next_segment_id)
            # way_id is kept from original line since we preserved all attributes
            new_line$artificial <- FALSE  # These are real segments
            next_segment_id <- next_segment_id + 1
            new_lines[[length(new_lines) + 1]] <- new_line
        }
    }

    # Add connection to new lines and combine all
    new_lines[[length(new_lines) + 1]] <- connection
    result <- bind_rows(new_lines)

    return(result)
}





#' Visualize Network Split and Connection
#'
#' @description
#' Creates a ggplot2 visualization of the network segments resulting from
#' split_and_connect_lines, showing original lines and new connections.
#'
#' @param result_sf sf object returned by split_and_connect_lines
#'
#' @return A ggplot2 object
#'
#' @import ggplot2
#' @import sf
#'
#' @export
plot_network_connection <- function(result_sf) {
    # Validate input
    if (!inherits(result_sf, "sf")) {
        stop("Input must be an sf object")
    }

    # Extract connection line
    connection_df <- result_sf[result_sf$artificial, ]

    # Extract split segments
    segments_df <- result_sf[!result_sf$artificial, ]

    # Extract points from connection line endpoints
    connection_coords <- st_coordinates(connection_df)
    points_df <- st_sf(
        geometry = st_sfc(
            c(st_point(connection_coords[1, 1:2]),
              st_point(connection_coords[2, 1:2])),
            crs = st_crs(result_sf)
        )
    )

    # Create the visualization
    p <- ggplot() +
        # Add split segments
        geom_sf(data = segments_df,
                aes(geometry = geometry),
                color = "blue",
                size = 1) +
        # Add connection line
        geom_sf(data = connection_df,
                aes(geometry = geometry),
                color = "red",
                linetype = "dashed",
                size = 1) +
        # Add points
        geom_sf(data = points_df,
                color = "black",
                size = 3,
                shape = 16) +
        # Add segment IDs
        geom_sf_text(data = st_centroid(segments_df),
                     aes(label = segment_id),
                     color = "blue",
                     nudge_y = 0.1,
                     size = 3) +
        geom_sf_text(data = st_centroid(connection_df),
                     aes(label = segment_id),
                     color = "red",
                     nudge_y = 0.1,
                     size = 3) +
        # Styling
        theme_minimal(base_size = 11) +
        theme(
            panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_line(color = "grey95"),
            axis.text = element_text(size = rel(0.8)),
            plot.title = element_text(size = rel(1.2)),
            legend.position = "none"
        ) +
        labs(
            title = "Network Connection Visualization",
            subtitle = paste("Total segments:", nrow(result_sf))
        ) +
        coord_sf()

    return(p)
}

#' Example usage:
#' ```r
#' # Example 1: Line to point connection
#' line1_sf <- st_sf(
#'   way_id = 1,
#'   highway = "residential",
#'   geometry = st_sfc(st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE)),
#'   crs = 3857)
#' )
#'
#' point1_sf <- st_sf(
#'   geometry = st_sfc(st_point(c(1,1)),
#'   crs = 3857)
#' )
#'
#' point2_sf <- st_sf(
#'   geometry = st_sfc(st_point(c(2,1)),
#'   crs = 3857)
#' )
#'
#' # Split and connect to point
#' result1 <- split_and_connect_lines(line1_sf, point1_sf, point2_sf)  # Returns 3 rows
#' plot_network_connection(result1)
#'
#' # Example 2: Line to line connection
#' line2_sf <- st_sf(
#'   way_id = 2,
#'   highway = "residential",
#'   geometry = st_sfc(st_linestring(matrix(c(2,0, 2,2), ncol=2, byrow=TRUE)),
#'   crs = 3857)
#' )
#'
#' # Split and connect two lines
#' result2 <- split_and_connect_lines(line1_sf, point1_sf, point2_sf, line2_sf)  # Returns 5 rows
#' plot_network_connection(result2)
#' ```
