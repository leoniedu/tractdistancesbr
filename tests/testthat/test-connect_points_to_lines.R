test_that("connect_points_to_lines handles basic case correctly", {
  # Create simple test data
  points <- sf::st_as_sf(data.frame(x = c(0, 1), y = c(0, 1)),
                         coords = c("x", "y"),
                         crs = 3857)

  lines <- sf::st_as_sf(data.frame(id = 1, highway = "residential"),
                        geometry = sf::st_sfc(
                          sf::st_linestring(matrix(c(-1, 2, 0, 2), ncol = 2)),
                          crs = 3857))

  # Run function with matching highway type
  result <- connect_points_to_lines(points, lines, connection_type = "residential")

  # Tests
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 6)  # 2 points * 3 new lines each
  expect_true("line_id" %in% names(result))
  expect_true("highway" %in% names(result))
  expect_true(all(sf::st_geometry_type(result) == "LINESTRING"))
})

test_that("function validates geometry types strictly", {
  points <- sf::st_as_sf(data.frame(x = 0, y = 0),
                         coords = c("x", "y"),
                         crs = 3857)

  # Create MULTILINESTRING data
  multi_line <- sf::st_as_sf(
    data.frame(
      id = 1,
      highway = "residential"
    ),
    geometry = sf::st_sfc(
      sf::st_multilinestring(
        list(matrix(c(-1, 1, 0, 1), ncol = 2),
             matrix(c(0, 1, 1, 1), ncol = 2))
      ),
      crs = 3857
    )
  )

  # Should error on MULTILINESTRING
  expect_error(
    connect_points_to_lines(points, multi_line, connection_type = "residential"),
    "lines_sf must contain only LINESTRING geometries"
  )
})

test_that("function requires and validates highway column", {
  points <- sf::st_as_sf(data.frame(x = 0, y = 0),
                         coords = c("x", "y"),
                         crs = 3857)

  lines <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(-1, 1, 0, 1), ncol = 2)),
      crs = 3857
    )
  )

  # Test missing highway column
  expect_error(
    connect_points_to_lines(points, lines),
    "lines_sf must have a 'highway' column"
  )
})

test_that("function preserves attributes in split lines", {
  points <- sf::st_as_sf(data.frame(x = 0, y = 0),
                         coords = c("x", "y"),
                         crs = 3857)

  lines <- sf::st_as_sf(
    data.frame(
      id = 1,
      highway = "residential",
      speed = 50,
      lanes = 2
    ),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(-1, 1, 0, 1), ncol = 2)),
      crs = 3857
    )
  )

  result <- connect_points_to_lines(points, lines, connection_type = "residential")

  # Check attributes are preserved in split segments
  split_lines <- result[result$highway == "residential",]
  expect_equal(unique(split_lines$speed), lines$speed)
  expect_equal(unique(split_lines$lanes), lines$lanes)
})

test_that("function handles CRS differences correctly", {
  points <- sf::st_as_sf(data.frame(x = 0, y = 0),
                         coords = c("x", "y"),
                         crs = 4326)  # WGS84

  lines <- sf::st_as_sf(
    data.frame(
      id = 1,
      highway = "residential"
    ),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(-1, 1, 0, 1), ncol = 2)),
      crs = 3857    # Web Mercator
    )
  )

  result <- connect_points_to_lines(points, lines, connection_type = "residential")

  expect_equal(sf::st_crs(result), sf::st_crs(lines))
})
