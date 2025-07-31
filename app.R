library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(lubridate)  # for years()

# Precompute your choices
team_selections  <- unique(all_data$School)
sport_selection  <- unique(all_data$sport)

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
    useShinyjs(),   # <<— only here
    
    tabItems(
      
      # 1) Filters tab
      tabItem(tabName = "filters",
              fluidRow(
                box(
                  title = "Recruiting Year Range", status = "primary",
                  solidHeader = TRUE, width = 4,
                  
                  dateRangeInput(
                    "year_range", "Select Dates",
                    start     = Sys.Date() - years(1),
                    end       = Sys.Date(),
                    format    = "yyyy", startview = "year",
                    separator = " to ", width = "100%"
                  ),
                  
                  actionButton(
                    inputId = "choose_sport",
                    label = tagList("Select Sport", tags$span(style = "margin-left: 10px; color: steelblue;", icon("mouse-pointer"))),
                    width   = "90%"
                  ),
                  br(), br(),
                  
                  selectInput(
                    "team", "Select Team",
                    choices   = team_selections,
                    selectize = FALSE,
                    width     = "90%",
                    size      = 3
                  ),
                  
                  actionButton("make_map", "Create Map", width = "90%")
                ),
                
                box(
                  title = "Selection Info", status = "info",
                  solidHeader = TRUE, width = 8,
                  verbatimTextOutput("selections")
                ),
                
                box(
                  title = "Data Preview", status = "info",
                  solidHeader = TRUE, width = 8,
                  verbatimTextOutput("summary_preview")
                )
              ),
              
              fluidRow(
                box(
                  title = "Pipeline Grid", status = "info",
                  solidHeader = TRUE, width = 12,
                  plotOutput("gridPlot", height = "800px")
                )
              )
      ),
      
      # 2) Summary tab
      tabItem(tabName = "summary",
              fluidRow(
                box(
                  title = "Summary", status = "info",
                  solidHeader = TRUE, width = 12,
                  verbatimTextOutput("summary")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # 1) hold the one‐time sport choice
  chosenSport <- reactiveVal(NULL)
  
  # 2) launch modal on button click
  observeEvent(input$choose_sport, {
    showModal(modalDialog(
      title = "Pick your sport",
      radioButtons(
        "sport_modal", NULL,
        choices = sort(sport_selection)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_sport", "Confirm")
      ),
      easyClose = FALSE
    ))
  })
  
  # 3) confirm & lock in choice
  observeEvent(input$confirm_sport, {
    req(input$sport_modal)
    chosenSport(input$sport_modal)
    removeModal()
    disable("choose_sport")
  })
  
  # 4) reactive filtering uses chosenSport()
  filtered_data <- reactive({
    req(chosenSport(), input$team, input$year_range)
    yrs <- as.integer(format(input$year_range, "%Y"))
    
    all_data %>%
      filter(
        sport  == chosenSport(),
        School == input$team,
        Year   >= yrs[1],
        Year   <= yrs[2]
      )
  })
  
  # 5) render outputs
  output$selections <- renderPrint({
    req(chosenSport())
    cat(
      "Selected recruiting classes from",
      format(input$year_range[1], "%Y"),
      "to", format(input$year_range[2], "%Y"), "\n",
      "for", input$team, chosenSport()
    )
  })
  
  output$summary_preview <- renderPrint({
    head(filtered_data(), 10)
  })
  
  output$gridPlot <- renderPlot({
    # your plotting code here, using filtered_data()
  })
  
  output$summary <- renderTable({
    filtered_data()
  })
}

shinyApp(ui, server)