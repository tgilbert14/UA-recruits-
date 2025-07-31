library(shiny)
library(shinydashboard)
library(tidyverse)

# # get input data
# start_date <- format(input$year_range[1], "%Y")
# end_date <- format(input$year_range[2], "%Y")
# team <- input$team
# sport <- input$sport
# # filter team
# data_byTeam <- data_raw %>% 
#   filter(School == input$team)
# 
# # filter by date
# data_byTeam_Date <- data_byTeam %>%
#   filter(Year >= start_date &
#            Year <= end_date)

team_selections <- unique(data_raw_football$School)

ui <- dashboardPage(
  
  # Header --------------------------------------------------------------------
  dashboardHeader(title = "Recruiting Pipeline Evaluations"),
  
  # Sidebar -------------------------------------------------------------------
  dashboardSidebar(
    width = 300,
    
    # Styling for sidebar
    tags$head(
      tags$style(HTML("
    /* the group’s main label */
    .box .shiny-input-radiogroup > .control-label {
      color: black !important;
    }

    /* each radio choice (block) */
    .box .shiny-input-radiogroup .radio label {
      color: black !important;
    }

    /* each radio choice (inline) */
    .box .shiny-input-radiogroup .radio-inline {
      color: black !important;
    }
  "))
    ),
    
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("filter")),
      menuItem("Summary",  tabName = "summary",  icon = icon("chart-bar"))
    ),
    
    # Input Box
    box(
      title       = "Recruiting Year Range",
      status      = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width       = NULL,
      
      dateRangeInput(
        inputId    = "year_range",
        label      = "Select Start and End Recruiting Dates",
        start      = Sys.Date() - years(1),
        end        = Sys.Date(),
        min        = "2010-01-01",
        max        = Sys.Date(),
        format     = "yyyy",
        startview  = "year",
        separator  = " to ",
        width      = "100%"
      ),
      
      radioButtons(
        inputId = "sport",
        label = "Select Sport",
        choices = c("football","basketball"),
        selected = NULL,
        inline = TRUE,
        width = "90%"),
      
      selectInput(
        inputId = "team",
        label = "Select Team to Evaluate",
        choices = c(team_selections),
        selected = FALSE,
        multiple = FALSE,
        selectize = FALSE,
        width = "90%",
        size = 3),
      
      actionButton(
        inputId = "make_map",
        label = "Create Map",
        #icon = ,
        width = "90%")
      
    )
    
  ),
  
  # Body ----------------------------------------------------------------------
  dashboardBody(
    
    # Styling for Body
    tags$head(
      tags$style(HTML("
        /* force all box labels (and form labels) to a dark color */
        .box .control-label,
        .shiny-input-container > label {
          color: black !important;
        }
      "))
    ),
    
    fluidRow(
      box(
        title       = "Selection Info",
        status      = "info",
        solidHeader = TRUE,
        width       = 12,
        verbatimTextOutput("selections")
      )
    ),
    
    fluidRow(
      box(
        title       = "Pipeline Grid",
        status      = "info",
        solidHeader = TRUE,
        width       = 12,
        plotOutput("gridPlot", height = "800px")
      )
    ),
    
    fluidRow(
      box(
        title       = "Summary",
        status      = "info",
        solidHeader = TRUE,
        width       = 12,
        verbatimTextOutput("summary")
      )
    )
    
  )
)

# Server ----------------------------------------------------------------------
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$team)
    
    # get input data
    start_date <- format(input$year_range[1], "%Y")
    end_date <- format(input$year_range[2], "%Y")
    team <- input$team
    sport <- input$sport
    
    # select correct data depending on sport
    if (sport == "basketball") {
      data <- data_raw_basketball
    }
    if (sport == "football") {
      data <- data_raw_football
    }
    
    # filter team
    data_byTeam <- data %>% 
      filter(School == input$team)
    
    # filter by date
    data_byTeam_Date <- data_byTeam %>%
      filter(Year >= start_date &
               Year <= end_date)
  })
  
  output$selections <- renderPrint({
    cat("Selected recruiting classes from",format(input$year_range[1],"%Y"),"to",format(input$year_range[2],"%Y"),"\n")
    cat("for",input$team,input$sport)
    
  })
  
  output$gridPlot <- renderPlot({
  })
  
  output$summary <- renderPrint({
    head(filtered_data())
  })
  
}

shinyApp(ui, server)