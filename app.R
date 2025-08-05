

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
app_path <<- getwd()
conn <- dbConnect(RSQLite::SQLite(), paste0(app_path,"/data/recruiting.db"))

# Pre-compute your choices
team_selections.1 <- safe_query(conn, "SELECT DISTINCT School FROM recruit_class_football")
team_selections.2 <- safe_query(conn, "SELECT DISTINCT School FROM recruit_class_basketball")
team_selections <- union(team_selections.1,team_selections.2)

sport_selections.1 <- safe_query(conn, "SELECT DISTINCT sport FROM recruit_class_football")
sport_selections.2 <- safe_query(conn, "SELECT DISTINCT sport FROM recruit_class_basketball")
sport_selections <- union(sport_selections.1,sport_selections.2)

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
    sidebarMenu(id = "tabs",
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
                  leafletOutput("gridPlot", height = "800px")
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
    
    # set database based on sport ->
    if (chosenSport() == "basketball") {
      db_table = "recruit_class_basketball"
    }
    if (chosenSport() == "football") {
      db_table = "recruit_class_football"
    }
    
    geting_data <- paste0("Select * from ",db_table," where sport = '",chosenSport(), 
    "' AND School = '",input$team,"' AND Year >= ",yrs[1]," AND Year <= ",yrs[2],
    " ORDER BY Ranking, NationalRank desc, StateRank desc, PositionRank desc, Name")

    all_data <- safe_query(conn, geting_data)
    
    all_data <- all_data %>%
      mutate(
        Height_in = str_extract(Height, "[0-9]+") %>% as.numeric() * 12 +
          str_extract(Height, "(?<=-)[0-9.]+") %>% as.numeric()
        )
    
    all_data$lat <- as.numeric(all_data$lat)
    all_data$long <- as.numeric(all_data$long)
    all_data$college_lat <- as.numeric(all_data$college_lat)
    all_data$college_long <- as.numeric(all_data$college_long)
    all_data$Ranking <- as.numeric(all_data$Ranking)
    all_data$NationalRank <- as.numeric(all_data$NationalRank)
    all_data$PositionRank <- as.numeric(all_data$PositionRank)
    all_data$StateRank <- as.numeric(all_data$PositionRank)
    
    all_data
    
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
    filtered_data() %>% 
      arrange(desc(Ranking), NationalRank, StateRank, PositionRank, Name)
  })
  
  # create map button
  observeEvent(input$make_map, {
    req(chosenSport(), input$team, input$year_range)
    
    # move tabs
    updateTabItems(session, "tabs", "summary")  # switch to Summary tab

    # render plot
    output$gridPlot <- renderLeaflet({
      
      data_to_map <- filtered_data()
      data_to_map <<- data_to_map
      
      university <- unique(str_to_title(data_to_map$School))
      
      map <- leaflet(data_to_map) %>%
        addTiles() %>%
        # Polylines to show movement paths
        addPolylines(lng = ~c(long, college_long),
                     lat = ~c(lat, college_lat),
                     color = ~Position,
                     group = "Paths") %>%
        # Dots for High School locations
        addCircleMarkers(lng = ~long,
                         lat = ~lat,
                         radius = 5,
                         color = "blue",
                         stroke = TRUE,
                         fillOpacity = 0.7,
                         label = ~paste0(Name,": ",Location_Clean)) %>%
        # Dots for College destinations
        addCircleMarkers(lng = ~college_long,
                         lat = ~college_lat,
                         radius = 5,
                         color = "red",
                         stroke = TRUE,
                         fillOpacity = 0.7,
                         label = ~paste(School_Clean))
      
      map_with_legend <- map %>%
        addLegend("bottomright",
                  pal = colorFactor(c("blue","red"), domain = c("Recruited from HS", university)),
                  values = c("Recruited from HS",university),
                  title = "Location Type",
                  opacity = 0.7)
      
      styled_map <- prependContent(map,
                                   # Prepend header styling and subtitle
                                   prependContent(
                                     tagList(
                                       tags$style(HTML("
        .subtitle {
          color: grey;
          text-align: center;
          padding: 2px;
        }
        .leaflet-bottom.leaflet-right {
          margin-bottom: 50px;
          margin-right: 10px;
        }
        @media screen and (max-width: 600px) {
          h2 { font-size: 18px; }
          h4.subtitle { font-size: 14px; }
        }
      ")),
                                       tags$div(
                                         style = "
          background-color: rgb(235, 246, 246);
          padding: 1% 2%;
          margin-bottom: 2px;
          text-align: center;
          border-radius: 4px;
          box-shadow: 0 2px 2px rgba(0, 0, 0, 0.1);
          display: block;",
                                         tags$h2(paste0(university, " Recruiting Map (", min(data_to_map$Year), "-", max(data_to_map$Year), ")")),
                                         tags$h4(class = "subtitle", "Recruiting Pipeline from Commits (via 247Sports)"),
                                         tags$div(style = "text-align:center; margin-top:0px;",
                                                  tags$small("⬤ HS", style = "color:blue; margin-right:2px;"),
                                                  tags$small(paste0("⬤ ", university), style = "color:red;")
                                         )
                                       )
                                     )
                                   )
      )
      # set view/zoom
      zoom_long <- data_to_map$college_long[data_to_map$School==input$team][1]
      zoom_lat <- data_to_map$college_lat[data_to_map$School==input$team][1]
      
      final_map <- styled_map %>% 
        setView(lng = zoom_long, lat = zoom_lat, zoom = 4.5)
      
      final_map
    })
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