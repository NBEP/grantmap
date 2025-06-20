# popup_text ----
test_that("popup_text works", {
  expect_equal(
    popup_text("foo", "bar", "foofy"),
    "foo<br><b>bar:</b> foofy"
  )

  # Extra options
  expect_equal(
    popup_text("foo", "bar", NA, na_value = "missing"),
    "foo<br><b>bar:</b> missing"
  )
  expect_equal(
    popup_text("foo", "bar", NA, hide_na = TRUE),
    "foo"
  )
  expect_equal(
    popup_text("foo", "bar", 12, style = "in_title: $in_data.00", delim = "\n"),
    "foo\nbar: $12.00"
  )

  # NA handling
  expect_equal(
    popup_text(NA, "bar", 12),
    "<b>bar:</b> 12"
  )
  expect_equal(
    popup_text("foo", "bar", NA),
    "foo<br><b>bar:</b> -"
  )
  expect_equal(
    popup_text("foo", "bar", NA, style = "<b>in_title:</b> startin_dataend"),
    "foo<br><b>bar:</b> start-end"
  )
  expect_equal(
    popup_text(NA, "bar", NA, hide_na = TRUE),
    NA
  )

  # Pipes
  expect_equal(
    "<h1>Suberb Owls</h1>" %>%
      popup_text(
        c("Great Horned Owl", "Saw-whet Owl"),
        c("big", "small"),
        style = "<b>in_title:</b> it is in_data"
      ) %>%
      popup_text("Barn Owl", "it has a heart-shaped face"),
    "<h1>Suberb Owls</h1><br><b>Great Horned Owl:</b> it is big<br><b>Saw-whet Owl:</b> it is small<br><b>Barn Owl:</b> it has a heart-shaped face"
  )
})

test_that("popup_text error message", {
  expect_error(
    popup_text("superb", "birb", c("owl", "duck")),
    regexp = "in_title and in_data must be the same length"
  )
  expect_error(
    popup_text("superb", "birb", "owl", style = "foo"),
    regexp = "style must include in_title and in_data"
  )
  expect_error(
    popup_text("superb", "birb", c("owl", "duck"), style = "foo"),
    regexp = "in_title and in_data must be the same length\n  style must include in_title and in_data"
  )
})

# popup_column ----
test_that("popup_column works", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", "Ongoing"),
    "Year" = c(2012, 2016)
  )

  # Test basic
  expect_equal(
    popup_column(df_in, c("Project_Name", "Status", "Year")),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", "Ongoing"),
      "Year" = c(2012, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> 2012",
        "<b>Project Name:</b> bar<br><b>Status:</b> Ongoing<br><b>Year:</b> 2016"
      )
    )
  )

  # Test complex
  expect_equal(
    df_in %>%
      popup_column(
        col_name = c("Project_Name", "Year"),
        col_title = c("Project", "Start Year"),
        target_col = "Info",
        style = "&emsp;in_title: in_data",
        delim = "\n"
      ),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", "Ongoing"),
      "Year" = c(2012, 2016),
      "Info" = c(
        "&emsp;Project: foo\n&emsp;Start Year: 2012",
        "&emsp;Project: bar\n&emsp;Start Year: 2016"
      )
    )
  )
})

test_that("popup_column handles NA values", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", NA),
    "Year" = c(NA, 2016)
  )

  # Default settings
  expect_equal(
    popup_column(df_in, c("Project_Name", "Status", "Year")),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", NA),
      "Year" = c(NA, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> -",
        "<b>Project Name:</b> bar<br><b>Status:</b> -<br><b>Year:</b> 2016"
      )
    )
  )
  # Custom NA value
  expect_equal(
    popup_column(
      df_in,
      c("Project_Name", "Status", "Year"),
      na_value = ""
    ),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", NA),
      "Year" = c(NA, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> ",
        "<b>Project Name:</b> bar<br><b>Status:</b> <br><b>Year:</b> 2016"
      )
    )
  )
  # Hide lines with NA value
  expect_equal(
    popup_column(
      df_in,
      c("Project_Name", "Status", "Year"),
      hide_na = TRUE
    ),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", NA),
      "Year" = c(NA, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete",
        "<b>Project Name:</b> bar<br><b>Year:</b> 2016"
      )
    )
  )
})

test_that("popup_column accepts pipes", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", "Ongoing"),
    "Year" = c(2012, 2016)
  )

  # Default settings
  expect_equal(
    popup_column(df_in, "Project_Name") %>%
      popup_column("Status") %>%
      popup_column("Year"),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", "Ongoing"),
      "Year" = c(2012, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> 2012",
        "<b>Project Name:</b> bar<br><b>Status:</b> Ongoing<br><b>Year:</b> 2016"
      )
    )
  )
})

test_that("popup_column error messages", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", "Ongoing"),
    "Year" = c(2012, 2016)
  )

  expect_error(
    popup_column(df_in, "foofy"),
    regexp = "Column foofy does not exist"
  )
  expect_error(
    popup_column(
      df_in,
      col_name = c("Project_Name", "Status", "Year"),
      col_title = "Project Name"
    ),
    regexp = "col_name and col_title must be same length"
  )
  expect_error(
    popup_column(df_in, "Project_Name", style = "foo"),
    regexp = "style must include in_title and in_data"
  )
})
