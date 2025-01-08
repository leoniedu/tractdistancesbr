#' Add nodes to a street network graph with descriptive edge IDs and custom weight profiles
#' @param graph A graph object in dodgr format
#' @param xy Matrix or data.frame of coordinates to be added
#' @param dist_tol Tolerance for detecting duplicate points (default = 0.000001)
#' @param intersections_only If TRUE, only add intersection points (default = FALSE)
#' @param wt_profile Profile to use for point-to-intersection connections (default = "bicycle")
#' @param wt_profile_file Custom weight profile file (default = NULL)
#' @param highway_type Highway type for point-to-intersection connections (default = "path")
#' @param .workers Number of parallel workers (default = future::availableCores() - 1)
#' @importFrom dodgr match_pts_to_graph dodgr_graph_cols
#' @importFrom geodist geodist
#' @importFrom furrr future_map future_map_dfr
#' @importFrom future plan multisession
#' @return Modified graph with new nodes and edges added
add_nodes_to_graph_labeled <- function(graph, xy, dist_tol = 0.000001,
                                       intersections_only = FALSE,
                                       wt_profile = "bicycle",
                                       wt_profile_file = NULL,
                                       highway_type = "path") {
  # Check required packages
  required_packages <- c("dodgr", "geodist", "furrr", "future")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }


  # Load weight profile
  if (!is.null(wt_profile_file)) {
    wt_profile_df <- jsonlite::fromJSON(wt_profile_file)
  } else {
    profiles <- dodgr::weighting_profiles
    wt_profile_df <- profiles[[wt_profile]]
    if (is.null(wt_profile_df)) {
      stop("Invalid weight profile specified")
    }
  }

  # Validate highway type
  if (is.null(wt_profile_df$weights[[highway_type]])) {
    stop("Specified highway_type does not exist in the weight profile")
  }

  pts <- dodgr::match_pts_to_graph(graph, xy, distances = TRUE)
  xy <- if (is.matrix(xy)) xy else as.matrix(xy)
  pts$x0 <- xy[, 1]
  pts$y0 <- xy[, 2]

  gr_cols <- dodgr::dodgr_graph_cols(graph)
  gr_cols <- unlist(gr_cols[which(!is.na(gr_cols))])
  graph_std <- graph[, gr_cols]
  names(graph_std) <- names(gr_cols)

  # Match points to edges in parallel
  index <- furrr::future_map_dfr(seq_along(pts$index),
                                 function(i) {
                                   out <- which((graph_std$from == graph_std$from[pts$index[i]] &
                                                   graph_std$to == graph_std$to[pts$index[i]]) |
                                                  (graph_std$from == graph_std$to[pts$index[i]] &
                                                     graph_std$to == graph_std$from[pts$index[i]]))
                                   data.frame(n = rep(i, length(out)), index = out)
                                 }, .options = furrr::furrr_options(seed = TRUE))

  # Helper functions
  genhash <- function(len = 10, prefix = "") {
    if (prefix == "") {
      paste0(sample(c(0:9, letters, LETTERS), size = len), collapse = "")
    } else {
      paste0(prefix, "_",
             paste0(sample(c(0:9, letters, LETTERS), size = len-nchar(prefix)-1),
                    collapse = ""))
    }
  }

  calculate_weights <- function(d, highway_type = "path",
                                is_oneway = FALSE, gradient = 0) {
    wt <- wt_profile_df$weights[[highway_type]]
    if (is.null(wt)) wt <- wt_profile_df$weights$path

    if (is_oneway && !is.null(wt_profile_df$penalties$oneway)) {
      wt <- wt * wt_profile_df$penalties$oneway
    }

    if (gradient != 0 && !is.null(wt_profile_df$penalties$gradient)) {
      grad_penalty <- wt_profile_df$penalties$gradient
      wt <- wt * (1 + abs(gradient) * grad_penalty)
    }

    list(
      d_weighted = d * wt,
      time = d * wt_profile_df$time_penalty,
      time_weighted = d * wt_profile_df$time_penalty * wt
    )
  }

  edges_to_split <- graph_std[index$index, ]
  graph_to_add <- graph[index$index, ]
  graph_std <- graph_std[-index$index, ]
  graph <- graph[-index$index, ]
  edges_to_split$n <- index$n

  # Process edges in parallel
  edges_split <- furrr::future_map_dfr(unique(index$n), function(i) {
    edges_to_split_i <- edges_to_split[which(edges_to_split$n == i), ]

    # Process each edge for this point in parallel
    edge_results <- furrr::future_map_dfr(seq_len(nrow(edges_to_split_i)), function(e) {
      edge_i <- edges_to_split_i[c(e, e), ]
      original_from <- edge_i$from[1]
      original_to <- edge_i$to[1]

      split_point_id <- genhash(10L, paste0("SP_", original_from, "_", original_to))
      edge_i$to[1] <- edge_i$from[2] <- split_point_id

      edge_i$xto[1] <- pts$x[i]
      edge_i$yto[1] <- pts$y[i]
      edge_i$xfr[2] <- pts$x[i]
      edge_i$yfr[2] <- pts$y[i]

      xy_i <- data.frame(
        x = as.numeric(c(edge_i[1, "xfr"], edge_i[1, "xto"], edge_i[2, "xto"])),
        y = as.numeric(c(edge_i[1, "yfr"], edge_i[1, "yto"], edge_i[2, "yto"]))
      )

      dmat <- geodist::geodist(xy_i)
      d_i <- geodist::geodist(pts[i, c("x", "y")], pts[i, c("x0", "y0")])
      d_i <- as.numeric(d_i[1, 1])

      if (any(dmat[upper.tri(dmat)] < dist_tol)) {
        edge_i <- edges_to_split_i[e, ]
        edge_i_new <- rbind(edge_i, edge_i)
        edge_i_new$from[2] <- edge_i_new$to[1]
        edge_i_new$to[2] <- edge_i_new$from[1]
        edge_i_new$xfr[2] <- edge_i_new$xto[1]
        edge_i_new$xto[2] <- edge_i_new$xfr[1]
        edge_i_new$yfr[2] <- edge_i_new$yto[1]
        edge_i_new$yto[2] <- edge_i_new$yfr[1]
        d_i_min <- c(1, 1, 2)[which.min(dmat[upper.tri(dmat)])]
        if (d_i_min == 1) {
          edge_i_new <- edge_i_new[2:1, ]
        }
      } else {
        edge_i$d[1] <- dmat[1, 2]
        edge_i$d[2] <- dmat[2, 3]

        # Calculate weights for split edges
        weights <- purrr::map(1:2, function(j) {
          calculate_weights(
            d = edge_i$d[j],
            highway_type = edge_i$highway[j],
            is_oneway = !is.na(edge_i$oneway[j]),
            gradient = 0
          )
        })

        edge_i$d_weighted <- sapply(weights, `[[`, "d_weighted")
        edge_i$time <- sapply(weights, `[[`, "time")
        edge_i$time_weighted <- sapply(weights, `[[`, "time_weighted")

        edge_i$edge_id <- paste0(
          "SPLIT_", original_from, "_", original_to, "_",
          "part", LETTERS[seq_len(nrow(edge_i))]
        )
        edge_i_new <- edge_i
      }

      if (!intersections_only) {
        orig_point_id <- genhash(10L, paste0("OP_", original_from, "_", original_to))
        edge_i_new$from[1] <- edge_i_new$to[2] <- orig_point_id
        edge_i_new$xfr[1] <- pts$x0[i]
        edge_i_new$yfr[1] <- pts$y0[i]
        edge_i_new$xto[2] <- pts$x0[i]
        edge_i_new$yto[2] <- pts$y0[i]
        edge_i_new$d <- d_i

        weights <- calculate_weights(
          d = d_i,
          highway_type = highway_type,
          is_oneway = FALSE,
          gradient = 0
        )
        edge_i_new$d_weighted <- weights$d_weighted
        edge_i_new$time <- weights$time
        edge_i_new$time_weighted <- weights$time_weighted

        edge_i_new$highway <- highway_type

        edge_i_new$edge_id <- paste0(
          "P2P_", original_from, "_", original_to, "_",
          edge_i_new$from, "_to_", edge_i_new$to
        )
        edge_i <- rbind(edge_i, edge_i_new)
      }

      edge_i
    }, .options = furrr::furrr_options(seed = TRUE))

    edge_results
  }, .options = furrr::furrr_options(seed = TRUE))

  graph_to_add <- graph_to_add[edges_split$n, ]
  gr_cols <- gr_cols[which(!is.na(gr_cols))]

  for (g in seq_along(gr_cols)) {
    graph_to_add[, gr_cols[g]] <- edges_split[[names(gr_cols)[g]]]
  }

  return(rbind(graph, graph_to_add))
}
