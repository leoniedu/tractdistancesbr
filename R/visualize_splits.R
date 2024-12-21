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
visualize_splits <- function(result_sf) {
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
