# tests/testthat/test-format_df.R

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

  # Call function: explicitly specifying value_col = "pa"
  result <- format_df(data = long_data, value_col = "pa", format = "long")

  # Check that both outputs are returned
  expect_true("site_obs" %in% names(result))
  expect_true("site_sp" %in% names(result))

  # Check columns in site_obs
  # coordinateUncertaintyInMeters should be renamed to coordUnM
  expect_true(all(c("site_id", "x", "y", "species", "value", "coordUnM") %in% names(result$site_obs)))
  expect_equal(nrow(result$site_obs), 3)  # same as input row count

  # Check site_sp was pivoted correctly:
  # For (10,50) sp1 => 1, sp2 => 1; For (20,60) sp1 => 0
  expect_equal(nrow(result$site_sp), 2)  # (10,50) & (20,60)

  # Row for (10,50)
  row_10_50 <- result$site_sp[result$site_sp$x == 10 & result$site_sp$y == 50, ]
  expect_equal(as.numeric(row_10_50$sp1), 1)
  expect_equal(as.numeric(row_10_50$sp2), 1)

  # Row for (20,60)
  row_20_60 <- result$site_sp[result$site_sp$x == 20 & result$site_sp$y == 60, ]
  # sp1 is 0, sp2 not present for that site, so 0
  expect_equal(as.numeric(row_20_60$sp1), 0)
  expect_true("sp2" %in% names(result$site_sp))  # column exists
  expect_equal(as.numeric(row_20_60$sp2), 0)
})

test_that("format_df handles wide format correctly", {
  # Example wide-format data
  wide_data <- data.frame(
    x = c(10, 10, 20),
    y = c(50, 50, 60),
    species1 = c(1, 1, 0),
    species2 = c(2, 2, 1),
    recordedBy = c("A", "A", "B"),
    eventDate = c("2024-01-01", "2024-01-01", "2024-01-03"),
    stringsAsFactors = FALSE
  )

  # Indicate columns 3-4 are species columns
  result <- format_df(data = wide_data, format = "wide", sp_col_range = 3:4)

  # Expect only site_sp in output
  expect_true("site_sp" %in% names(result))
  expect_false("site_obs" %in% names(result))

  # site_sp columns
  expected_cols <- c("site_id", "x", "y", "recordedBy", "eventDate", "species1", "species2")
  expect_true(all(expected_cols %in% names(result$site_sp)))

  # Rows with (10,50) should be combined and species1 => 2, species2 => 4
  row_10_50 <- result$site_sp[result$site_sp$x == 10 & result$site_sp$y == 50, ]
  expect_equal(nrow(row_10_50), 1)
  expect_equal(as.numeric(row_10_50$species1), 2)  # 1 + 1
  expect_equal(as.numeric(row_10_50$species2), 4)  # 2 + 2

  # (20,60) is single row: species1 => 0, species2 => 1
  row_20_60 <- result$site_sp[result$site_sp$x == 20 & result$site_sp$y == 60, ]
  expect_equal(as.numeric(row_20_60$species1), 0)
  expect_equal(as.numeric(row_20_60$species2), 1)
})

test_that("format_df creates site_id if missing", {
  # This data has no site_id col
  data_no_site <- data.frame(
    x = c(5, 5, 15),
    y = c(10, 10, 20),
    species = c("spA", "spB", "spA"),
    pa = c(1, 0, 1),
    stringsAsFactors = FALSE
  )

  # The function should create site_id automatically
  result <- format_df(data = data_no_site, value_col = "pa", format = "long")
  expect_true("site_id" %in% names(result$site_obs))
  expect_false(all(is.na(result$site_obs$site_id)))
})

test_that("format_df errors when essential columns are missing", {
  # Missing the x coordinate
  data_missing <- data.frame(
    y = c(1, 2, 3),
    species = c("a", "b", "c"),
    pa = c(1, 0, 1),
    stringsAsFactors = FALSE
  )

  # Should throw an error about missing x
  expect_error(format_df(data = data_missing, format = "long"),
               regexp = "is not TRUE")
})
