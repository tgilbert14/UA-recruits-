library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)


# processing data

# recruit pipeline evals data
all_data <- read_csv("football_and_basketball_data.csv")

# function to update data if needed
