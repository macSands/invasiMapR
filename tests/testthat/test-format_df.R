# If the shorthand .() is not available, define it as an alias for list()
if (!exists(".", mode = "function", inherits = FALSE)) {
  `.` <- function(...) list(...)
}

library(testthat)
library(dplyr)
library(data.table)

test_that("format_df handles long format correctly", {
  # Example long-format data
  long_data <- data.frame(
    x = c(10, 10, 20),
    y = c(50, 50, 60),
    species = c("sp1", "sp2", "sp1"),
    pa = c(1, 1, 0),
    coordinateUncertaintyInMeters = c(100, 100, 200),
    stringsAsFactors = FALSE
  )

  # Call the function using "pa" as the value column
  res <- format_df(data = long_data, format = "long", value_col = "pa")

  # Check that both outputs are returned
  expect_true("site_obs" %in% names(res))
  expect_true("site_sp" %in% names(res))

  # site_obs should have these columns: site_id, x, y, species, value, and coordUnM
  expected_obs_cols <- c("site_id", "x", "y", "species", "value", "coordUnM")
  expect_true(all(expected_obs_cols %in% names(res$site_obs)))

  # Number of rows in site_obs should equal input row count
  expect_equal(nrow(res$site_obs), nrow(long_data))

  # Pivoted wide version (site_sp) should have one row per unique (x, y)
  expect_equal(nrow(res$site_sp), 2)

  # For group (x=10, y=50): expect sp1 = 1 and sp2 = 1
  group_10_50 <- res$site_sp[res$site_sp$x == 10 & res$site_sp$y == 50, ]
  expect_equal(as.numeric(group_10_50[["sp1"]]), 1)
  expect_equal(as.numeric(group_10_50[["sp2"]]), 1)

  # For group (x=20, y=60): expect sp1 = 0 and sp2 = 0 (filled as 0)
  group_20_60 <- res$site_sp[res$site_sp$x == 20 & res$site_sp$y == 60, ]
  expect_equal(as.numeric(group_20_60[["sp1"]]), 0)
  expect_equal(as.numeric(group_20_60[["sp2"]]), 0)
})

test_that("format_df handles wide format correctly", {
  # Example wide-format data
  wide_data <- data.frame(
    x = c(10, 10, 20),
    y = c(50, 50, 60),
    species1 = c(1, 1, 0),
    species2 = c(2, 2, 1),
    recordedBy = c("A", "A", "A"),
    eventDate = c("2024-01-01", "2024-01-01", "2024-01-03"),
    stringsAsFactors = FALSE
  )

  # Specify sp_col_range to indicate columns 3-4 are species columns
  res <- format_df(data = wide_data, format = "wide", sp_col_range = 3:4)

  # In wide format, only site_sp should be returned
  expect_true("site_sp" %in% names(res))
  expect_false("site_obs" %in% names(res))

  # Expected columns: site_id, x, y, recordedBy, eventDate, species1, species2
  expected_wide_cols <- c("site_id", "x", "y", "recordedBy", "eventDate", "species1", "species2")
  expect_true(all(expected_wide_cols %in% names(res$site_sp)))

  # For (x=10, y=50): aggregation should yield species1 = 1+1 = 2 and species2 = 2+2 = 4
  group_10_50 <- res$site_sp[res$site_sp$x == 10 & res$site_sp$y == 50, ]
  expect_equal(nrow(group_10_50), 1)
  expect_equal(as.numeric(group_10_50[["species1"]]), 2)
  expect_equal(as.numeric(group_10_50[["species2"]]), 4)

  # For (x=20, y=60), a single row: species1 = 0, species2 = 1
  group_20_60 <- res$site_sp[res$site_sp$x == 20 & res$site_sp$y == 60, ]
  expect_equal(nrow(group_20_60), 1)
  expect_equal(as.numeric(group_20_60[["species1"]]), 0)
  expect_equal(as.numeric(group_20_60[["species2"]]), 1)
})

test_that("format_df creates site_id if missing", {
  # Data without any column matching expected site_id names
  data_no_site <- data.frame(
    x = c(5, 5, 15),
    y = c(10, 10, 20),
    species = c("spA", "spB", "spA"),
    pa = c(1, 0, 1),
    stringsAsFactors = FALSE
  )

  res <- format_df(data = data_no_site, format = "long", value_col = "pa")

  # Check that site_obs contains a site_id column that is not all NA
  expect_true("site_id" %in% names(res$site_obs))
  expect_false(all(is.na(res$site_obs$site_id)))
})

test_that("format_df errors when essential columns are missing", {
  # Data missing the x-coordinate
  data_missing <- data.frame(
    y = c(1, 2, 3),
    species = c("a", "b", "c"),
    pa = c(1, 0, 1),
    stringsAsFactors = FALSE
  )

  expect_error(format_df(data = data_missing, format = "long"),
               regexp = "is not TRUE")
})
