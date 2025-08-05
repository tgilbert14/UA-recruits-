library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(DBI)
library(RSQLite)

# setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
# app_path <<- getwd()
# 
# # processing data
# 
# # recruit pipeline evals data
# #all_data <- read_csv("data/all_data.csv")
# 
# conn <- dbConnect(RSQLite::SQLite(), paste0(app_path,"/data/recruiting.db"))
# dbGetQuery(conn, "SELECT * FROM recruit_class LIMIT 5")

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


