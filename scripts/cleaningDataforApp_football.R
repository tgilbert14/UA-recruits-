library(sp)

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



library(tidygeocoder)

# filter to commits only
hs_data <- hs_data %>% filter(Type == "Commit")

# # break down into chunks by 300 to help with timeout issues
# chunks <- split(hs_data, ceiling(seq_along(1:nrow(hs_data)) / 300))
# 
# geo_chunks <- lapply(chunks, function(df) {
#   geocode(df, address = loc_inside_parens, method = "osm", lat = lat, long = long)
# })
# # bind all the data
# hs_geo_all <- bind_rows(geo_chunks)
# data_cleaned_raw <- hs_geo_all

# have to do in sections..
hs_data.1 <- hs_data[1:300,]
hs_data.2 <- hs_data[301:600,]
hs_data.3 <- hs_data[601:900,]
hs_data.4 <- hs_data[901:1200,]
hs_data.5 <- hs_data[1201:1500,]
hs_data.6 <- hs_data[1501:1800,]
hs_data.7 <- hs_data[1801:2100,]
hs_data.8 <- hs_data[2101:2400,]
hs_data.9 <- hs_data[2401:2700,]
hs_data.10 <- hs_data[2701:3000,]
hs_data.11 <- hs_data[3001:3300,]
hs_data.12 <- hs_data[3301:nrow(hs_data),]

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

hs_geo_12 <- hs_data.12 %>%
  geocode(address = loc_inside_parens, method = "osm", lat = lat, long = long)

# bind together
t1 <- rbind(hs_geo_1, hs_geo_2)
t2 <- rbind(hs_geo_3, hs_geo_4)
t3 <- rbind(hs_geo_5, hs_geo_6)
t4 <- rbind(hs_geo_7, hs_geo_8)
t5 <- rbind(hs_geo_9, hs_geo_10)
t6 <- rbind(hs_geo_11, hs_geo_12)

t1_t2_merged <- rbind(t1, t2)
t3_t4_merged <- rbind(t3, t4)
t5_t6_merged <- rbind(t5, t6)

t1_t2_t3_t4_merged <- rbind(t1_t2_merged, t3_t4_merged)

data_cleaned_raw <- rbind(t1_t2_t3_t4_merged, t5_t6_merged)
View(data_cleaned_raw)

# cleaning data up -->
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
data_merged$Weight[data_merged$Name=="Josh Watts"] <- 200

# check height
no_h <- data_merged %>% 
  filter(Height=="0-0")
View(no_h)

data_merged$Height[data_merged$Name=="Josh Watts"] <- "6-4"

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
