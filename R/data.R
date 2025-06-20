#' Funded projects (dataframe)
#'
#' Dataframe containing project details.
#'
#' @format A dataframe with 14 columns:
#'  \describe{
#'    \item{Grant}{Grant name}
#'    \item{Project}{Project name}
#'    \item{Organization}{Organization receiving grant}
#'    \item{Status}{Project status. Options: Ongoing, Complete}
#'    \item{Start_Year}{Initial funding year}
#'    \item{End_Year}{Final funding year}
#'    \item{Category}{Project category. Options: Capacity-Building, Education, 
#'      Implementation, Monitoring, Outreach, Planning, Research, Restoration}
#'    \item{Funding_Source}{Funding source. Options: BIL, NEP, SNEP}
#'    \item{Funding_Amount}{Amount of grant funding (US dollars)}
#'    \item{Report}{Link to final report}
#'    \item{Description}{Project description}
#'    \item{Latitude}{Latitude}
#'    \item{Longitude}{Longitude}
#'    \item{Popup}{Popup text for map}
#'  }
"df_raw"

#' NBEP boundaries (shapefile)
#'
#' Shapefile showing the Narragansett Bay Estuary Program's study area.
#'
#' @format A dataframe with 4 columns and 3 rows:
#'  \describe{
#'    \item{Study_Area}{The name of the NBEP study area.}
#'    \item{DataSource}{The source(s) of the primary data used to produce this 
#'      dataset.}
#'    \item{SourceYear}{The publication year(s) of the primary data used to 
#'      produce this dataset.}
#'    \item{NBEPYear}{The publication year of the NBEP State of Narragansett 
#'      Bay & Its Watershed Technical Report associated with this dataset. }
#'    \item{geometry}{Shapefile geometry}
#'  }
"shp_nbep"