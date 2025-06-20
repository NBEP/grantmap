test_that("intersect_list works", {
  expect_equal(
    intersect_list(
      c("wow", "fluffy", "owl"),
      "owl, is, fluffy"
    ),
    c("fluffy", "owl")
  )
  expect_equal(
    intersect_list(
      c("wow", "fluffy", "owl"),
      "owl is fluffy",
      delim = " "
    ),
    c("fluffy", "owl")
  )
  expect_equal(
    intersect_list(
      c("wow", "fluffy", "owl"),
      "owl is fluffy"
    ),
    NA
  )
})

test_that("multifilter works", {
  df <- data.frame(
    "birds" = c("barn owl", "great horned owl", "kingfisher"),
    "not_birds" = c("pine martin", "cat", "plastic owl")
  )
  
  expect_equal(
    multifilter(df, "birds", c("owl", "eagle", "hawk"), delim = " "),
    data.frame(
      "birds" = c("barn owl", "great horned owl"),
      "not_birds" = c("pine martin", "cat")
    )
  )
  expect_equal(
    multifilter(df, "not_birds", c("owl", "eagle", "hawk"), delim = " "),
    data.frame(
      "birds" = "kingfisher",
      "not_birds" = "plastic owl"
    )
  )
  expect_equal(
    multifilter(df, "birds", c("foo", "bar"), delim = " "),
    data.frame(
      "birds" = character(),
      "not_birds" = character()
    )
  )
})