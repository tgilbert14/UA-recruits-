library(tidyverse)
library(tidygeocoder)
library(networkD3)

# recruit pipeline evals
data_raw <- read_csv("recruit_csvs/all_recruits_BIG12_football.csv")
#print(head(data))

data_filtered <- data_raw %>% 
  filter(School == "arizona") %>% 
  filter(Year %in% c(2023:2026)) %>%
  filter(Type == "Commit")

data_cleaned <- data_filtered %>%
  mutate(
    # Extract what's inside parentheses
    loc_inside_parens = str_extract(Location, "(?<=\\()[^)]+"),
    # Extract name before parentheses and trim
    location_name = str_trim(str_extract(Location, "^[^(]+")),
    # Final cleaned version: add "High School," before the parentheses content
    Location_Clean = paste0(location_name, " High School, ", loc_inside_parens)
  )

# clean and geocode high school locations
hs_data <- data_cleaned %>%
  distinct(Location_Clean, .keep_all = TRUE) %>%
  mutate(Location_Clean = str_trim(Location_Clean))

# use geo code to get lat/long coordinates
hs_geo <- hs_data %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)
#View(hs_geo) #checking coordinates makes sense

data_cleaned <- data_cleaned %>% mutate(
  School_Clean = paste0("University of ", School)
)

# try to do the same for college/school destinations (or look it up)
school_geo <- data_cleaned %>%
  distinct(School, .keep_all = TRUE) %>%
  mutate(School = str_trim(School)) %>%
  geocode(address = School, method = "osm", lat = college_lat, long = college_long)

# coordinates were wrong, fixing for U of Arizona...
school_geo$college_lat[school_geo$School == "arizona"] <- 32.231
school_geo$college_long[school_geo$School == "arizona"] <- -110.951

# decided to map by city instead of HS..
# 'loc_inside_parens' easier to get coordinates

# Merge high school coordinates
data <- data_cleaned %>%
  left_join(hs_geo %>% select(Location, lat, long), by = "Location") %>%
  rename(hs_lat = lat, hs_long = long)
View(data)

# Merge college coordinates
data_merged <- data %>%
  left_join(school_geo %>% select(School, college_lat, college_long), by = "School")
View(data_merged)

data_merged <- data_merged %>% 
  arrange(State, loc_inside_parens)

# Create nodes and links
nodes <- tibble(name = unique(c(data$loc_inside_parens, data_merged$School)))

links <- data_merged %>%
  count(loc_inside_parens, School) %>%
  transmute(
    source = match(loc_inside_parens, nodes$name) - 1,
    target = match(School, nodes$name) - 1,
    value = n
  )

# Generate Sankey plot
networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "name")



library(leaflet)

map <- leaflet(data_merged) %>%
  addTiles() %>%
  # Polylines to show movement paths
  addPolylines(lng = ~c(hs_long, college_long),
               lat = ~c(hs_lat, college_lat),
               color = ~Position,
               group = "Paths") %>%
  # Dots for High School locations
  addCircleMarkers(lng = ~hs_long,
                   lat = ~hs_lat,
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

#map

# Export to HTML
library(htmlwidgets)

university <- unique(str_to_title(data_merged$School_Clean))

map_with_legend <- map %>%
  addLegend("bottomright",
            pal = colorFactor(c("blue","red"), domain = c("Recruited from HS", university)),
            values = c("Recruited from HS",university),
            title = "Location Type",
            opacity = 0.7)

library(htmltools)

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
                                   tags$h2(paste0(university, " Recruiting Map (", min(data_merged$Year), "-", max(data_merged$Year), ")")),
                                   tags$h4(class = "subtitle", "Recruiting Pipeline from Commits (via 247Sports)"),
                                   tags$div(style = "text-align:center; margin-top:0px;",
                                            tags$small("⬤ HS", style = "color:blue; margin-right:2px;"),
                                            tags$small(paste0("⬤ ", university), style = "color:red;")
                                   )
                                 )
                               )
                             )
)
styled_map
zoom_long <- data_merged$college_long[data_merged$School=="arizona"][1]
zoom_lat <- data_merged$college_lat[data_merged$School=="arizona"][1]

# set view/zoom
final_map <- styled_map %>% 
setView(lng = zoom_long, lat = zoom_lat, zoom = 4.5)# %>%
  # addLegend("bottomright",
  #           pal = colorFactor(c("blue","red"), domain = c("Recruited from HS", university)),
  #           values = c("Recruited from HS",university),
  #           title = "Location Type",
  #           opacity = 0.7)
final_map

# save as html
saveWidget(final_map, "az_football_recruit_map_2023_2026.html", selfcontained = TRUE)

# library(webshot)
# #Requires PhantomJS — install it once via:
# #webshot::install_phantomjs()
# 
# # Save still image from HTML
# webshot("plots/az_football_recruit_map_2023_2026.html", "map_snapshot.png", vwidth = 1200, vheight = 800)
# 
# # to switch from leaflet to mapview
# mapview::mapshot(map, file = "map_snapshot.png")
  






