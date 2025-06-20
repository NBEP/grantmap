test_that("format_coordinates works", {
  good_coord <- data.frame(
    "Latitude" = c(56, 12, 47),
    "Longitude" = c(24, 85, 98)
  )
  
  bad_coord <- data.frame(
    "Latitude" = c(56, 12, 98),
    "Longitude" = c(186, NA, 98)
  )
  
  expect_equal(
    format_coordinates(good_coord),
    good_coord
  )
  
  expect_equal(
    suppressWarnings(
      format_coordinates(bad_coord)
    ),
    data.frame(
      "Latitude" = c(56, 12, NA),
      "Longitude" = c(NA, NA, 98)
    )
  )
})

test_that("format_coordinates error messages", {
  missing_coord <- data.frame(
    "Latitude" = c(56, NA, 47),
    "Longitude" = c(24, 85, NA)
  )
  
  bad_coord <- data.frame(
    "Latitude" = c(56,-92, 47),
    "Longitude" = c(224, 85, 98)
  )
  
  expect_error(
    missing_coord %>% format_coordinates(lat_min = 95),
    regexp = "Invalid coordinate boundaries"
  )
  
  expect_warning(
    format_coordinates(missing_coord),
    regexp = "Missing coordinates. Check rows 2, 3"
  )
  
  expect_warning(
    format_coordinates(bad_coord),
    regexp = "Invalid coordinates. Check rows 1, 2"
  )
})

test_that("format_town works", {
  df_RI <- data.frame(
    "Town" = c("Providence", "North Kingstown"),
    "State" = c("Rhode Island", "RI")
  )
  expect_equal(
    format_town(df_RI),
    data.frame(
      "Town" = c("Providence", "North Kingstown")
    )
  )
  
  df_multistate <- data.frame(
    "Town" = c("Providence", "Worcester"),
    "State" = c("Rhode Island", "Massachusetts")
  )
  expect_equal(
    format_town(df_multistate),
    data.frame(
      "Town" = c("Providence, RI", "Worcester, MA")
    )
  )
})

test_that("format_town error messages", {
  df_no_town <- data.frame(
    "State" = c("RI", "MA")
  )
  expect_error(
    format_town(df_no_town),
    regexp = 'Column "Town" not found'
  )
  
  df_some_town <- data.frame(
    "Town" = c("Providence", NA, NA),
    "State" = c("RI", "RI", "MA")
  )
  expect_warning(
    format_town(df_some_town),
    regexp = 'Town is missing in rows 2, 3'
  )
  
  df_no_state <- data.frame(
    "Town" = c("Providence", "North Kingstown")
  )
  expect_warning(
    format_town(df_no_state),
    regexp = 'Column "State" not found'
  )
  
  df_some_state <- data.frame(
    "Town" = c("Providence", "North Kingstown", "Worcester"),
    "State" = c(NA, "RI", NA)
  )
  expect_warning(
    format_town(df_some_state),
    regexp = 'State is missing in rows 1, 3'
  )
})

test_that("chk_unique works", {
  df_test <- data.frame(
    "foo" = c(1, 2, 3, 4, 5),
    "bar" = c(2, 2, 3, 2, 5),
    "banana" = c(NA, NA, 3, 4, 5),
    "bananana" = NA
  )
  
  # Normal conditions
  expect_no_condition(
    df_test %>% chk_unique("foo")
  )
  
  expect_error(
    chk_unique(df_test, "bar"),
    regexp = "All values in bar must be unique. Check rows 1, 2, 4"
  )
  
  # NA handling
  expect_error(
    chk_unique(df_test, "banana"),
    regexp = "All values in banana must be unique. Check rows 1, 2"
  )
  
  expect_no_condition(
    df_test %>% chk_unique("banana", ignore_na = TRUE)
  )
  
  expect_no_condition(
    df_test %>% chk_unique("bananana", ignore_na = TRUE)
  )
})