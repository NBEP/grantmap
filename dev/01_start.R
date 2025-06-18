# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "grantmap", # The name of the golem package containing the app (typically lowercase, no underscore or periods)
  pkg_title = "NBEP funded projects", # What the Package Does (One Line, Title Case, No Period)
  pkg_description = "An interactive map of projects funded by the Narragansett Bay Estuary Program.", # What the package does (one paragraph).
  authors = person(
    given = "Mariel", 
    family = "Sorlien", 
    email = "info@nbep.org", 
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0001-7102-2918")
  ),
  repo_url = NULL, # The URL of the GitHub repo (optional),
  pkg_version = "0.0.0.9000", # The version of the package containing the app
  set_options = TRUE # Set the global golem options
)

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license("Narragansett Bay Estuary Program") # You can set another license here
# golem::use_readme_rmd(open = FALSE)
# devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
usethis::use_code_of_conduct(contact = "Narragansett Bay Estuary Program")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
# golem::use_recommended_tests()

## Favicon ----
golem::use_favicon("inst/app/www/NBEP_logo_square.png") 

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
usethis::use_git()
## Sets the remote associated with 'name' to 'url'
usethis::use_git_remote(
  name = "origin",
  url = "https://github.com/nbep/grantmap.git"
)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
