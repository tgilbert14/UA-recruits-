library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(leaflet)
library(htmlwidgets)
library(htmltools)

safe_query <- function(conn, query) {
  tryCatch({
    result <- dbGetQuery(conn, query)
    if (nrow(result) == 0) return(data.frame(sport = character(0)))
    result
  }, error = function(e) {
    warning("Query failed: ", conditionMessage(e))
    data.frame(sport = character(0))
  })
}


