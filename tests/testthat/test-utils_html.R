test_that("external_link works", {
  expect_equal(
    external_link(" this is a link ", "https://www.nbep.org/"),
    '<a href="https://www.nbep.org/" rel="noreferrer" target="_blank">this is a link<span class="visually-hidden"> (opens in new tab)</span></a>'
  )

  expect_error(
    external_link("descriptive title", "nbep.org"),
    regexp = "Invalid link"
  )
})

test_that("image_link works", {
  expect_equal(
    image_link(
      "https://www.nbep.org/",
      "image.png",
      "NBEP",
      width = "300px",
      height = "500px"
    ),
    "<a href=\"https://www.nbep.org/\" rel=\"noreferrer\" target=\"_blank\"><img src=\"image.png\" width = \"300px\" height = \"500px\" alt = \"NBEP\"></a>"
  )

  expect_equal(
    image_link(
      "https://www.nbep.org/",
      "image.png",
      "NBEP"
    ),
    "<a href=\"https://www.nbep.org/\" rel=\"noreferrer\" target=\"_blank\"><img src=\"image.png\" width = \"auto\" height = \"auto\" alt = \"NBEP\"></a>"
  )

  expect_error(
    image_link("nbep.org", "image.png", "NBEP"),
    regexp = "Invalid link"
  )
})
