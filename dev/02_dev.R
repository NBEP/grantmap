# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

usethis::use_import_from("magrittr", "%>%")
usethis::use_import_from("data.table", ":=")
usethis::use_import_from("rlang", ".data")
usethis::use_import_from("stats", "setNames")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "map", with_test = TRUE)
golem::add_module(name = "sidebar", with_test = TRUE)
golem::add_module(name = "table", with_test = TRUE)

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("dropdown", with_test = TRUE)
golem::add_fct("multifilter", with_test = TRUE)
golem::add_fct("popup_text", with_test = TRUE)
golem::add_utils("import_data", with_test = TRUE)
golem::add_utils("html", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")
# golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "df", open = FALSE)

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")

# Documentation

## Vignette ----
# usethis::use_vignette("nbepShinyTemplate")
# devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
# usethis::use_github()

# GitHub Actions
usethis::use_github_action("test-coverage")
usethis::use_github_action("check-standard")

# # Add action for PR
# usethis::use_github_action_pr_commands()
#
# # Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()
#
# # Jenkins
# usethis::use_jenkins()
#
# # GitLab CI
# usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")