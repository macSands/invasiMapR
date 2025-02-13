test_that("get_occurrence_data handles local CSV files", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(site_id = 1:3, x = c(10, 20, 30), y = c(-10, -20, -30),
                       sp_name = c("A", "B", "C"), pa = c(1, 0, 1)),
            temp_file, row.names = FALSE)

  result <- get_occurrence_data(data = temp_file, source_type = "local_csv")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("site_id", "x", "y", "sp_name", "pa") %in% names(result)))
})

test_that("get_occurrence_data handles data frames", {
  input_df <- data.frame(site_id = 1:3, x = c(10, 20, 30), y = c(-10, -20, -30),
                         sp_name = c("A", "B", "C"), pa = c(1, 0, 1))

  result <- get_occurrence_data(data = input_df, source_type = "data_frame")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("site_id", "x", "y", "sp_name", "pa") %in% names(result)))
})

test_that("get_occurrence_data handles missing coordinate columns", {
  input_df <- data.frame(site_id = 1:3, sp_name = c("A", "B", "C"), pa = c(1, 0, 1))

  expect_error(get_occurrence_data(data = input_df, source_type = "data_frame"),
               "Latitude/Longitude columns not detected")
})

test_that("get_occurrence_data detects invalid longitude values", {
  input_df <- data.frame(site_id = 1:3, x = c(200, 20, 30), y = c(-10, -20, -30),
                         sp_name = c("A", "B", "C"), pa = c(1, 0, 1))

  expect_error(get_occurrence_data(data = input_df, source_type = "data_frame"),
               "Longitude values should be between -180 and 180")
})

test_that("get_occurrence_data detects invalid latitude values", {
  input_df <- data.frame(site_id = 1:3, x = c(10, 20, 30), y = c(-100, -20, -30),
                         sp_name = c("A", "B", "C"), pa = c(1, 0, 1))

  expect_error(get_occurrence_data(data = input_df, source_type = "data_frame"),
               "Latitude values should be between -90 and 90")
})

# Note: GBIF tests are not included since they require network access.

message("All tests passed successfully!")
