# cleaning data for shiny app
all_data <- read_csv("recruit_csvs/all_recruits_BIG12_football.csv")

data_cleaned <- all_data %>%
  mutate(
    # Extract what's inside parentheses
    loc_inside_parens = str_extract(Location, "(?<=\\()[^)]+"),
    # Extract name before parentheses and trim
    location_name = str_trim(str_extract(Location, "^[^(]+")),
    # Final cleaned version: add "High School," before the parentheses content
    Location_Clean = paste0(location_name, " High School, ", loc_inside_parens)
  )
#View(data_cleaned)

# clean and geocode high school locations
hs_data <- data_cleaned %>%
  #distinct(Location_Clean, .keep_all = TRUE) %>%
  mutate(Location_Clean = str_trim(Location_Clean))

# only want commits
hs_data <- hs_data %>% 
  filter(Type == "Commit")

library(tidygeocoder)

# have to do in sections..
hs_data.1 <- hs_data[1:280,]
hs_data.2 <- hs_data[281:540,]
hs_data.3 <- hs_data[541:860,]
hs_data.4 <- hs_data[861:1100,]
hs_data.5 <- hs_data[1101:1390,]
hs_data.6 <- hs_data[1391:1600,]
hs_data.7 <- hs_data[1601:1900,]
hs_data.6 <- hs_data[1901:2200,]
hs_data.7 <- hs_data[2201:2500,]
hs_data.8 <- hs_data[2501:2800,]
hs_data.9 <- hs_data[2801:3050,]
hs_data.10 <- hs_data[3051:3300,]
hs_data.11 <- hs_data[3301:nrow(hs_data),]

# use geo code to get lat/long coordinates
hs_geo_1 <- hs_data.1 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_2 <- hs_data.2 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_3 <- hs_data.3 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_4 <- hs_data.4 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_5 <- hs_data.5 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_6 <- hs_data.6 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_7 <- hs_data.7 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_8 <- hs_data.8 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_9 <- hs_data.9 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_10 <- hs_data.10 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

hs_geo_11 <- hs_data.11 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

# bind together
t1 <- rbind(hs_geo_1,hs_geo_2)
t2 <- rbind(hs_geo_3,hs_geo_4)
t3 <- rbind(hs_geo_5,hs_geo_6)
t4 <- rbind(hs_geo_7,hs_geo_8)
t5 <- rbind(hs_geo_9,hs_geo_10)

t6 <- rbind(t1,hs_geo_11)
t7 <- rbind(t2,t3)
t8 <- rbind(t4,t5)

t9 <- rbind(t6,t7)
t10 <- rbind(t8,t9)

data_cleaned_raw <- t10

data_cleaned <- data_cleaned_raw %>% mutate(
  School_Clean = paste0("University of ", School)
)

# add city col, then fill
data_cleaned$School_City <- NA
data_cleaned$School_City[data_cleaned$School=="arizona-state"] <- "Tempe, AZ"
data_cleaned$School_City[data_cleaned$School=="arizona"] <- "Tucson, AZ"
data_cleaned$School_City[data_cleaned$School=="baylor"] <- "Waco, TX"
data_cleaned$School_City[data_cleaned$School=="byu"] <- "Provo, UT"
data_cleaned$School_City[data_cleaned$School=="texas-tech"] <- "Lubbock, TX"
data_cleaned$School_City[data_cleaned$School=="utah"] <- "Salt Lake City, UT"
data_cleaned$School_City[data_cleaned$School=="west-virginia"] <- "Morgantown, WV"
data_cleaned$School_City[data_cleaned$School=="central-florida"] <- "Orlando, FL"
data_cleaned$School_City[data_cleaned$School=="cincinnati"] <- "Cincinnati, OH"
data_cleaned$School_City[data_cleaned$School=="colorado"] <- "Boulder, CO"
data_cleaned$School_City[data_cleaned$School=="houston"] <- "Houston, TX"
data_cleaned$School_City[data_cleaned$School=="iowa-state"] <- "Ames, IA"
data_cleaned$School_City[data_cleaned$School=="kansas-state"] <- "Manhattan, KS"
data_cleaned$School_City[data_cleaned$School=="kansas"] <- "Lawrence, KS"
data_cleaned$School_City[data_cleaned$School=="oklahoma-state"] <- "Stillwater, OK"
data_cleaned$School_City[data_cleaned$School=="tcu"] <- "Fort Worth, TX"

#View(data_cleaned)
# try to do the same for college/school destinations (or look it up)
school_geo <- data_cleaned %>%
  distinct(School, .keep_all = TRUE) %>%
  mutate(School = str_trim(School)) %>%
  geocode(address = School_City, method = "osm", lat = college_lat, long = college_long)

school_geo_cleaned <- school_geo %>% 
  select(School, School_Clean, School_City, college_lat, college_long)

# merge data_cleaned back with school coordinates...
data_merged <- left_join(data_cleaned, school_geo_cleaned, by=c("School", "School_Clean", "School_City"))
#View(data_merged)

# check weight
no_w <- data_merged %>% 
  filter(Weight==0)
View(no_w)

data_merged$Weight[data_merged$Name=="Mason Fletcher"] <- 215

# need locations..
no_coor <- data_merged %>% 
  filter(is.na(lat))
View(no_coor)

data_merged$lat[data_merged$Name=="Jamarye Joiner"] <- "32.0005"
data_merged$long[data_merged$Name=="Jamarye Joiner"] <- "-110.7009"

data_merged$lat[data_merged$Name=="Edward Vesterinen"] <- "60.1699"
data_merged$long[data_merged$Name=="Edward Vesterinen"] <- "24.9384"

data_merged <- unique(data_merged)
write_csv(data_merged, "recruit_csvs/all_data_football.csv")

## putting in in sql database...
library(readxl)      # For reading Excel files
library(DBI)         # For database interface
library(RSQLite)     # For SQLite backend

conn <- dbConnect(RSQLite::SQLite(), "data/recruiting.db")

dbExecute(conn, "CREATE TABLE IF NOT EXISTS [recruit_class_football](
  [Name] VARCHAR(50), 
  [Location] VARCHAR(50), 
  [Height] VARCHAR(15), 
  [Weight] INT, 
  [Ranking] INT, 
  [NationalRank] INT, 
  [PositionRank] INT, 
  [StateRank] INT, 
  [State] VARCHAR2(15), 
  [Position] VARCHAR2(15), 
  [Year] INT(4), 
  [School] VARCHAR(50), 
  [Type] VARCHAR(15), 
  [count] INT, 
  [sport] VARCHAR(50), 
  [loc_inside_parens] VARCHAR(50), 
  [location_name] VARCHAR(50), 
  [Location_Clean] VARCHAR(50), 
  [lat] FLOAT, 
  [long] FLOAT, 
  [School_Clean] VARCHAR(50), 
  [School_City] VARCHAR(50), 
  [college_lat] FLOAT, 
  [college_long] FLOAT);")

# This appends your Excel data to the existing table
dbWriteTable(conn, "recruit_class_football", data_merged, append = TRUE, row.names = FALSE)

dbGetQuery(conn, "SELECT * FROM recruit_class_football LIMIT 5")

dbDisconnect(conn)
