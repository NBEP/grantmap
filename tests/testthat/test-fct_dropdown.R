# tidy_list -----
test_that("tidy_list works", {
  # Basic list
  expect_equal(
    tidy_list(c(4, 5, 1, 2, 5)),
    c(1, 2, 4, 5)
  )
  expect_equal(
    tidy_list(c("foo", "bar", "foofy", "bar", "foofy", NA)),
    c("bar", "foo", "foofy", "Other")
  )
  expect_equal(
    tidy_list("banana"),
    "banana"
  )
  expect_equal(
    tidy_list(c(NA, NA)),
    NA
  )

  # Named list
  test <- c("foo", "bar", "foofy")
  names(test) <- c("wow", "superb", "owl")

  test2 <- c("foofy", "bar", "foo")
  names(test2) <- c("owl", "superb", "wow")

  expect_equal(tidy_list(test), test2)

  # Additional options
  expect_equal(
    tidy_list(
      c("foo", "bar", "foofy", "bar", "foofy"),
      sort_list = FALSE
    ),
    c("foo", "bar", "foofy")
  )
  expect_equal(
    tidy_list(
      c("foo", "bar", "foofy", "bar", "foofy"),
      decreasing = TRUE
    ),
    c("foofy", "foo", "bar")
  )
  expect_equal(
    tidy_list(
      c("foo", "bar", "foofy", "bar", "foofy", NA),
      add_other = FALSE
    ),
    c("bar", "foo", "foofy")
  )
})

# wrap_text ----
test_that("wrap_text works", {
  expect_equal(
    wrap_text(
      c(
        "Prow scuttle parrel provost Sail ho shrouds spirits boom mizzenmast",
        "Pinnace holystone mizzenmast quarter crow's nest nipperkin grog",
        "Deadlights jack lad schooner"
      )
    ),
    c(
      "Prow scuttle parrel provost Sail ho<br>shrouds spirits boom mizzenmast",
      "Pinnace holystone mizzenmast quarter<br>crow's nest nipperkin grog",
      "Deadlights jack lad schooner"
    )
  )
  expect_equal(
    wrap_text(c("short little sentence", "oh my", NA)),
    c("short little sentence", "oh my", NA)
  )
  expect_equal(wrap_text(NULL), NULL)
  expect_equal(wrap_text(c(NA, NA)), NULL)
})

# select_dropdown ----
test_that("select_dropdown works", {
  owl_list <- c("Bubo virginianus", "Tyto furcata", "Aegolius acadicus")
  names(owl_list) <- c("Great Horned Owl", "Barn Owl", "Saw-whet Owl")

  abc_owls <- c("Tyto furcata", "Bubo virginianus", "Aegolius acadicus")
  names(abc_owls) <- c("Barn Owl", "Great Horned Owl", "Saw-whet Owl")

  zyx_owls <- c("Aegolius acadicus", "Bubo virginianus", "Tyto furcata")
  names(zyx_owls) <- c("Saw-whet Owl", "Great Horned Owl", "Barn Owl")

  # Default options
  expect_equal(
    select_dropdown(
      id = "owls",
      label = "Superb Owl",
      choices = c("Bubo virginianus", "Tyto furcata", "Aegolius acadicus"),
      choice_names = c("Great Horned Owl", "Barn Owl", "Saw-whet Owl")
    ),
    shinyWidgets::pickerInput(
      "owls",
      label = "Superb Owl",
      choices = abc_owls,
      selected = abc_owls,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3",
        `max-options` = NULL,
        container = "body"
      ),
      multiple = TRUE,
      choicesOpt = list(content = NULL)
    )
  )

  # Text wrap
  ipsum_in <- c(
    "Deadlights jack lad schooner",
    "Prow scuttle parrel provost Sail ho shrouds spirits boom mizzenmast."
  )

  expect_equal(
    select_dropdown(
      id = "ipsum",
      label = "Pirate Ipsum",
      choices = ipsum_in
    ),
    shinyWidgets::pickerInput(
      "ipsum",
      label = "Pirate Ipsum",
      choices = ipsum_in,
      selected = ipsum_in,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3",
        `max-options` = NULL,
        container = "body"
      ),
      multiple = TRUE,
      choicesOpt = list(
        content = c(
          "Deadlights jack lad schooner",
          "Prow scuttle parrel provost Sail ho<br>shrouds spirits boom mizzenmast."
        )
      )
    )
  )

  ipsum_out <- c("pirate", "ipsum")
  names(ipsum_out) <- c(
    "Deadlights jack lad schooner",
    "Prow scuttle parrel provost Sail ho shrouds spirits boom mizzenmast."
  )

  expect_equal(
    select_dropdown(
      id = "ipsum",
      label = "Pirate Ipsum",
      choices = c("pirate", "ipsum"),
      choice_names = c(
        "Deadlights jack lad schooner",
        "Prow scuttle parrel provost Sail ho shrouds spirits boom mizzenmast."
      )
    ),
    shinyWidgets::pickerInput(
      "ipsum",
      label = "Pirate Ipsum",
      choices = ipsum_out,
      selected = ipsum_out,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3",
        `max-options` = NULL,
        container = "body"
      ),
      multiple = TRUE,
      choicesOpt = list(
        content = c(
          "Deadlights jack lad schooner",
          "Prow scuttle parrel provost Sail ho<br>shrouds spirits boom mizzenmast."
        )
      )
    )
  )

  # Extra Options
  expect_equal(
    select_dropdown(
      id = "owls",
      label = "Superb Owl",
      choices = c("Bubo virginianus", "Tyto furcata", "Aegolius acadicus"),
      choice_names = c("Great Horned Owl", "Barn Owl", "Saw-whet Owl"),
      sort_choices = FALSE,
      multiple = FALSE
    ),
    shinyWidgets::pickerInput(
      "owls",
      label = "Superb Owl",
      choices = owl_list,
      selected = owl_list[1],
      options = list(
        `actions-box` = FALSE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3",
        `max-options` = NULL,
        container = "body"
      ),
      multiple = FALSE,
      choicesOpt = list(content = NULL)
    )
  )
  expect_equal(
    select_dropdown(
      id = "owls",
      label = "Superb Owl",
      choices = c("Bubo virginianus", "Tyto furcata", "Aegolius acadicus"),
      choice_names = c("Great Horned Owl", "Barn Owl", "Saw-whet Owl"),
      decreasing = TRUE,
      max_options = 2
    ),
    shinyWidgets::pickerInput(
      "owls",
      label = "Superb Owl",
      choices = zyx_owls,
      selected = zyx_owls[1],
      options = list(
        `actions-box` = FALSE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3",
        `max-options` = 2,
        container = "body"
      ),
      multiple = TRUE,
      choicesOpt = list(content = NULL)
    )
  )
})

test_that("select_dropdown error messages", {
  expect_error(
    select_dropdown(
      id = "owls",
      label = "Superb Owl",
      choices = c("Bubo virginianus", "Tyto furcata", "Aegolius acadicus"),
      choice_names = c("Great Horned Owl", "Barn Owl")
    ),
    regexp = "choices and choice_names must be the same length"
  )
})
