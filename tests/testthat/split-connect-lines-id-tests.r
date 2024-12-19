test_that("split_and_connect_lines creates unique segment IDs", {
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
        connection_id = "CONN_1"
    )
    
    # Check segment_id column exists
    expect_true("segment_id" %in% names(result))
    
    # Check all segments have unique IDs
    expect_equal(length(unique(result$segment_id)), nrow(result))
    
    # Check format of segment IDs
    expect_true(all(grepl("^(SEG_\\d+|CONN_1)$", result$segment_id)))
    
    # Check original IDs are preserved
    expect_true("original_id" %in% names(result))
    expect_equal(sum(!is.na(result$original_id)), nrow(result) - 1)  # all but connection
    expect_true(is.na(result$original_id[result$artificial]))
})

test_that("split_and_connect_lines handles custom ID prefix", {
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
        connection_id = "CONN_1",
        id_prefix = "TEST"
    )
    
    # Check format of segment IDs with custom prefix
    segments <- result[!result$artificial,]
    expect_true(all(grepl("^TEST_\\d+$", segments$segment_id)))
})
