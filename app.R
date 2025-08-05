library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
app_path <<- getwd()
conn <- dbConnect(RSQLite::SQLite(), paste0(app_path,"/data/recruiting.db"))

# dbGetQuery(conn, "SELECT * FROM recruit_class LIMIT 5")
# Precompute your choices
team_selections <- safe_query(conn, "SELECT DISTINCT School FROM recruit_class")
sport_selections <- safe_query(conn, "SELECT DISTINCT sport FROM recruit_class")

## UI -->
ui <- dashboardPage(
  dashboardHeader(title = "Recruiting Pipeline Evaluations"),
  
  dashboardSidebar(
    width = 120,
    collapsed = TRUE,
    
    tags$head(
      tags$style(HTML("
        .box .control-label,
        .shiny-input-container > label { color: black !important; }
      "))
    ),
    sidebarMenu(
      menuItem("Data",    tabName = "filters", icon = icon("filter")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      
      # Filters tab
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  title = "Recruiting Year Range", status = "primary",
                  solidHeader = TRUE, width = 4,
                  
                  dateRangeInput(
                    "year_range", "Select Dates",
                    start     = Sys.Date() - years(1),
                    end       = Sys.Date(),
                    format    = "yyyy",
                    startview = "year",
                    separator = " to ",
                    width = "100%",
                    min = "2016-01-01",
                    max = "2026-12-31"
                  ),
                  
                  actionButton(
                    inputId = "choose_sport",
                    label = tagList("Select Sport", tags$span(style = "margin-left: 10px; color: steelblue;", icon("mouse-pointer"))),
                    width   = "100%"
                  ),
                  br(), br(),
                  
                  selectInput(
                    "team", "Select Team",
                    choices   = sort(team_selections$School),
                    selectize = FALSE,
                    width     = "100%",
                    size      = 3
                  ),
                  
                  actionButton("make_map", "Create Map", width = "100%")
                ),
                
                box(
                  title = "Selection Info", status = "info",
                  solidHeader = TRUE, width = 8,
                  verbatimTextOutput("selections")
                ),
                
                box(
                  title = "Data Preview", status = "info",
                  solidHeader = TRUE, width = 8,
                  tableOutput("summary_preview")
                )
              )

      ),
      
      # Summary tab
      tabItem(tabName = "summary",
              
              
              fluidRow(
                box(
                  title = "Pipeline Grid", status = "info",
                  solidHeader = TRUE, width = 12,
                  plotOutput("gridPlot", height = "800px")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # hold the one‐time sport choice
  chosenSport <- reactiveVal(NULL)
  
  # launch modal on button click
  observeEvent(input$choose_sport, {
    showModal(modalDialog(
      title = "Pick your sport",
      radioButtons(
        "sport_modal", NULL,
        choices = sort(sport_selections$sport)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_sport", "Confirm")
      ),
      easyClose = FALSE
    ))
  })
  
  # confirm & lock in choice
  observeEvent(input$confirm_sport, {
    req(input$sport_modal)
    chosenSport(input$sport_modal)
    removeModal()
    disable("choose_sport")
  })
  
  # reactive filtering uses chosenSport()
  filtered_data <- reactive({
    req(chosenSport(), input$team, input$year_range)
    yrs <- as.integer(format(input$year_range, "%Y"))
    
    geting_data <- paste0("Select * from recruit_class where sport = '",chosenSport(), 
    "' AND School = '",input$team,"' AND Year >= ",yrs[1]," AND Year <= ",yrs[2])

    all_data <- safe_query(conn, geting_data)
  })
  
  # render outputs
  output$selections <- renderPrint({
    req(chosenSport())
    cat(
      "Selected recruiting classes from",
      format(input$year_range[1], "%Y"),
      "to", format(input$year_range[2], "%Y"), "\n",
      "for", input$team, chosenSport()
    )
  })
  
  output$summary_preview <- renderTable({
    filtered_data()
  })
  
  output$gridPlot <- renderPlot({
    selected_data <- filtered_data()
    
  })
  
  output$summary <- renderTable({
    filtered_data()
  })
  
  # # Close connection when app stops
  # session$onStop(function() {
  #   if (dbIsValid(conn)) {
  #     dbDisconnect(conn)
  #     cat("Database connection closed.\n")
  #   }
  # })
  
}

shinyApp(ui, server)