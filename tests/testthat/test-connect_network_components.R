library(testthat)
library(sf)
library(dplyr)

# Helper function to create simple test networks
create_test_network <- function(components) {
  network_list <- list()
  for(i in seq_along(components)) {
    coords <- components[[i]]
    lines <- do.call(rbind, lapply(1:(nrow(coords)-1), function(j) {
      matrix(c(coords[j,], coords[j+1,]), ncol=2, byrow=TRUE)
    }))
    network_list[[i]] <- st_linestring(lines)
  }

  network_sf <- st_sf(
    geometry = st_sfc(network_list, crs = 4326),
    component = rep(1:length(components), times = 1),
    highway = "residential",
    line_id = 1:length(components)
  )
  return(network_sf)
}

test_that("function handles single component correctly", {
  # Create a simple single-component network
  component <- list(matrix(c(
    0, 0,
    1, 0,
    1, 1
  ), ncol=2, byrow=TRUE))

  network <- create_test_network(component)

  result <- connect_network_components(network)

  expect_identical(result$complete_network, network)
  expect_null(result$artificial_edges)
})

test_that("function connects two separate components", {
  # Create two separate components
  components <- list(
    matrix(c(
      0, 0,
      1, 0
    ), ncol=2, byrow=TRUE),
    matrix(c(
      3, 0,
      4, 0
    ), ncol=2, byrow=TRUE)
  )

  network <- create_test_network(components)

  result <- connect_network_components(network)

  # Check that result has expected structure
  expect_s3_class(result$complete_network, "sf")
  expect_s3_class(result$artificial_edges, "sf")

  # Check that artificial edges were created
  expect_equal(nrow(result$artificial_edges), 1)
  expect_equal(result$artificial_edges$highway, "artificial")

  # Check that the total number of features is correct
  expect_equal(nrow(result$complete_network), 3) # 2 original + 1 artificial
})

test_that("function preserves CRS", {
  components <- list(
    matrix(c(0,0, 1,0), ncol=2, byrow=TRUE),
    matrix(c(3,0, 4,0), ncol=2, byrow=TRUE)
  )

  network <- create_test_network(components)
  st_crs(network) <- 4326

  result <- connect_network_components(network)

  expect_equal(st_crs(result$complete_network), st_crs(network))
  expect_equal(st_crs(result$artificial_edges), st_crs(network))
})

test_that("function handles missing required columns", {
  components <- list(
    matrix(c(0,0, 1,0), ncol=2, byrow=TRUE),
    matrix(c(3,0, 4,0), ncol=2, byrow=TRUE)
  )

  network <- create_test_network(components)

  # Test missing component column
  network_no_component <- network
  network_no_component$component <- NULL
  expect_error(connect_network_components(network_no_component),
               "Column 'component' not found in lines_sf")

  # Test missing highway column
  network_no_highway <- network
  network_no_highway$highway <- NULL
  expect_error(connect_network_components(network_no_highway),
               "lines_sf must have a 'highway' column")
})

test_that("function connects multiple components optimally", {
  # Create three components in a triangle formation
  components <- list(
    matrix(c(0,0, 1,0), ncol=2, byrow=TRUE),
    matrix(c(3,0, 4,0), ncol=2, byrow=TRUE),
    matrix(c(2,2, 2,3), ncol=2, byrow=TRUE)
  )

  network <- create_test_network(components)

  result <- connect_network_components(network)

  # Should create 2 artificial edges (minimum spanning tree)
  expect_equal(nrow(result$artificial_edges), 2)

  # Total features should be original lines + 2 artificial edges
  expect_equal(nrow(result$complete_network), 5)
})

test_that("function handles empty geometries", {
  # Create network with empty geometry
  empty_network <- st_sf(
    geometry = st_sfc(crs = 4326),
    component = numeric(),
    highway = character(),
    line_id = numeric()
  )

  expect_warning(
    result <- connect_network_components(empty_network),
    "No lines were successfully processed"
  )
})

test_that("function correctly assigns new IDs", {
  components <- list(
    matrix(c(0,0, 1,0), ncol=2, byrow=TRUE),
    matrix(c(3,0, 4,0), ncol=2, byrow=TRUE)
  )

  network <- create_test_network(components)
  network$custom_id <- 100:101

  result <- connect_network_components(network, new_id_column = "custom_id")

  # Check that new IDs are assigned correctly
  expect_true(all(result$artificial_edges$custom_id > max(network$custom_id)))
  expect_true(length(unique(result$complete_network$custom_id)) ==
                nrow(result$complete_network))
})

test_that("function handles custom connection type", {
  components <- list(
    matrix(c(0,0, 1,0), ncol=2, byrow=TRUE),
    matrix(c(3,0, 4,0), ncol=2, byrow=TRUE)
  )

  network <- create_test_network(components)

  result <- connect_network_components(network, connection_type = "bridge")

  expect_equal(result$artificial_edges$highway, "bridge")
})
