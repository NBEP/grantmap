#  TITLE: fun_popup_text.R
#  DESCRIPTION: Function to generate popup text for leaflet map
#  AUTHOR(S): Mariel Sorlien
#  DATE LAST UPDATED: 2023-06-21
#  GIT REPO:
#  R version 4.2.3 (2023-03-15 ucrt)  x86_64

project_popup <- function(
    PROJECT_TITLE, START_YEAR, END_YEAR, CONTRACTOR, REPORT, STATUS, 
    PROJECT_COST, FUNDING_SOURCE, PROJECT_DESCRIPTION
    ){
  
  popup_header <- paste0(
    '<b><i>Project: </i></b>', PROJECT_TITLE, 
    '<br/><b><i>Organization: </i></b>', CONTRACTOR,
    '<br/><b><i>Start Year: </i></b>', START_YEAR, 
    '<br/><b><i>End Year: </i></b>',  END_YEAR)
  
  popup_footer <- paste0(
    '<br/><br/><b><i>Status: </i></b>', STATUS,
    '<br/><b><i>Cost: $</i></b>', PROJECT_COST,
    '<br/><b><i>Funding Source: </i></b>', FUNDING_SOURCE,
    '<br/><br/><b><i>Description</i></b>: ', PROJECT_DESCRIPTION)
  
  ifelse(
    nchar(REPORT) > 0,
    popup_text <- paste0(popup_header, popup_footer),
    popup_text <- paste0(
      popup_header, 
      '<br/><br/><a href="', REPORT, '">Final Report</a>',
      popup_footer)
  )
  
  return(popup_text)
  
  }