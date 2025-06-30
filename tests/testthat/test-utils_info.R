test_that("desc_grant works", {
  df_test <- data.frame(
    Status = c("Ongoing", "Complete"),
    Funding_Amount = c(12, NA),
    Organization = "NBEP",
    Start_Year = 1985,
    End_Year = 2025,
    Funding_Source = "BIL",
    Report = c(NA, "https://www.nbep.org/")
  )
  
  expect_equal(
    desc_grant(df_test[1,]),
    paste0(
      '<span class="badge bg-secondary">Ongoing</span>',
      '<p><b>Organization:</b> NBEP<br><b>Start Year:</b> 1985',
      '<br><b>End Year:</b> 2025</p><p><b>Funding:</b> $12',
      '<br><b>Funding Source:</b> BIL</p>'
    )
  )
  
  expect_equal(
    desc_grant(df_test[2,]),
    paste0(
      '<span class="badge bg-primary">Complete</span>',
      '<p><b>Organization:</b> NBEP<br><b>Start Year:</b> 1985',
      '<br><b>End Year:</b> 2025</p><p><b>Funding:</b> -',
      '<br><b>Funding Source:</b> BIL</p>',
      '<p><a href="https://www.nbep.org/" rel="noreferrer" target="_blank">', 
      'Read Final Report',
      '<span class="visually-hidden"> (opens in new tab)</span></a></p>'
    )
  )
  
  expect_equal(
    desc_grant(df_test[0,]),
    ""
  )
})

test_that("desc_project works", {
  df_test <- data.frame(
    Grant = "foo",
    Project = c("superb", "owl"),
    Category = c("Outreach", "Restoration"),
    Description = c("lorem ipsum", "dolor sit amet")
  )
  
  expect_equal(
    desc_project(df_test),
    paste0(
      '<h2>foo</h2>',
      '<h3>superb</h3><p><b>Category:</b> Outreach',
      '<br><b>Description:</b> lorem ipsum</p>',
      '<h3>owl</h3><p><b>Category:</b> Restoration',
      '<br><b>Description:</b> dolor sit amet</p>'
    )
  )
  
  expect_equal(
    desc_project(df_test[1,]),
    paste0(
      '<h2>foo</h2>',
      '<p><b>Category:</b> Outreach<br><b>Description:</b> lorem ipsum</p>'
    )
  )
  
  expect_equal(
    desc_project(df_test[0,]),
    "Error: Unknown Grant"
  )
})