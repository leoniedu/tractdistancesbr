#' @srr_function
test_that("split_and_connect_lines handles basic line-to-line connection", {
    # Create test data
    line1 <- st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE))
    line2 <- st_linestring(matrix(c(2,0, 0,2), ncol=2, byrow=TRUE))
    point1 <- st_point(c(1,1))
    point2 <- st_point(c(1,0.5))
    
    line1_sf <- st_sf(
        geometry = st_sfc(line1, crs = 4326),
        way_id = 1,
        highway = "residential"
    )
    
    line2_sf <- st_sf(
        geometry = st_sfc(line2, crs = 4326),
        way_id = 2,
        highway = "residential"
    )
    
    point1_sf <- st_sf(geometry = st_sfc(point1, crs = 4326))
    point2_sf <- st_sf(geometry = st_sfc(point2, crs = 4326))
    
    result <- split_and_connect_lines(
        line1_sf, point1_sf, point2_sf, line2_sf,
        connection_id = "test_conn"
    )
    
    # Tests
    expect_s3_class(result, "sf")
    expect_true("connection_id" %in% names(result))
    expect_true("artificial" %in% names(result))
    expect_true(any(result$connection_id == "test_conn"))
    expect_true(any(result$artificial))
    
    # Should have 5 features: 2 segments from line1, 2 from line2, 1 connection
    expect_equal(nrow(result), 5)
})

test_that("split_and_connect_lines handles line-to-point connection", {
    line1 <- st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE))
    point1 <- st_point(c(1,1))
    point2 <- st_point(c(2,0))
    
    line1_sf <- st_sf(
        geometry = st_sfc(line1, crs = 4326),
        way_id = 1,
        highway = "residential"
    )
    
    point1_sf <- st_sf(geometry = st_sfc(point1, crs = 4326))
    point2_sf <- st_sf(geometry = st_sfc(point2, crs = 4326))
    
    result <- split_and_connect_lines(
        line1_sf, point1_sf, point2_sf,
        connection_id = "test_conn"
    )
    
    # Should have 3 features: 2 segments from line1, 1 connection
    expect_equal(nrow(result), 3)
    expect_true(any(result$artificial))
})

test_that("split_and_connect_lines preserves attributes", {
    line1 <- st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE))
    point1 <- st_point(c(1,1))
    point2 <- st_point(c(2,0))
    
    line1_sf <- st_sf(
        geometry = st_sfc(line1, crs = 4326),
        way_id = 1,
        highway = "residential",
        name = "Main St",
        lanes = 2
    )
    
    point1_sf <- st_sf(geometry = st_sfc(point1, crs = 4326))
    point2_sf <- st_sf(geometry = st_sfc(point2, crs = 4326))
    
    result <- split_and_connect_lines(
        line1_sf, point1_sf, point2_sf,
        connection_id = "test_conn",
        surface = "paved"
    )
    
    # Check original attributes are preserved in segments
    segments <- result[!result$artificial,]
    expect_equal(unique(segments$way_id), 1)
    expect_equal(unique(segments$name), "Main St")
    expect_equal(unique(segments$lanes), 2)
    
    # Check new attributes in connection
    connection <- result[result$artificial,]
    expect_equal(connection$surface, "paved")
})

test_that("split_and_connect_lines validates inputs correctly", {
    line1 <- st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE))
    point1 <- st_point(c(1,1))
    point2 <- st_point(c(2,0))
    
    line1_sf <- st_sf(
        geometry = st_sfc(line1, crs = 4326),
        way_id = 1
    )
    
    point1_sf <- st_sf(geometry = st_sfc(point1, crs = 4326))
    point2_sf <- st_sf(geometry = st_sfc(point2, crs = 4326))
    
    # Test invalid geometries
    invalid_line <- st_sf(
        geometry = st_sfc(point1, crs = 4326),
        way_id = 2
    )
    
    invalid_point <- st_sf(
        geometry = st_sfc(line1, crs = 4326)
    )
    
    expect_error(
        split_and_connect_lines(invalid_line, point1_sf, point2_sf),
        "must have LINESTRING geometry"
    )
    
    expect_error(
        split_and_connect_lines(line1_sf, invalid_point, point2_sf),
        "must have POINT geometry"
    )
    
    # Test CRS mismatch
    point_wrong_crs <- st_sf(
        geometry = st_sfc(point1, crs = 3857)
    )
    
    expect_error(
        split_and_connect_lines(line1_sf, point1_sf, point_wrong_crs),
        "must have the same CRS"
    )
})

test_that("split_and_connect_lines handles coincident points", {
    line1 <- st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE))
    line2 <- st_linestring(matrix(c(2,0, 0,2), ncol=2, byrow=TRUE))
    # Points are at the same location
    point1 <- st_point(c(1,1))
    point2 <- st_point(c(1,1))
    
    line1_sf <- st_sf(
        geometry = st_sfc(line1, crs = 4326),
        way_id = 1
    )
    
    line2_sf <- st_sf(
        geometry = st_sfc(line2, crs = 4326),
        way_id = 2
    )
    
    point1_sf <- st_sf(geometry = st_sfc(point1, crs = 4326))
    point2_sf <- st_sf(geometry = st_sfc(point2, crs = 4326))
    
    result <- split_and_connect_lines(
        line1_sf, point1_sf, point2_sf, line2_sf
    )
    
    # Connection should have zero length
    connection <- result[result$artificial,]
    expect_equal(st_length(connection), 0)
})

test_that("split_and_connect_lines handles additional attributes via ...", {
    line1 <- st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE))
    point1 <- st_point(c(1,1))
    point2 <- st_point(c(2,0))
    
    line1_sf <- st_sf(
        geometry = st_sfc(line1, crs = 4326),
        way_id = 1
    )
    
    point1_sf <- st_sf(geometry = st_sfc(point1, crs = 4326))
    point2_sf <- st_sf(geometry = st_sfc(point2, crs = 4326))
    
    result <- split_and_connect_lines(
        line1_sf, point1_sf, point2_sf,
        connection_id = "test_conn",
        highway = "path",
        speed_limit = 30,
        surface = "paved",
        width = 2.5
    )
    
    connection <- result[result$artificial,]
    expect_equal(connection$speed_limit, 30)
    expect_equal(connection$surface, "paved")
    expect_equal(connection$width, 2.5)
})
