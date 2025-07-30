

library(tidyverse)

# read in UA recruits
data <- read_csv("recruit_csvs/football/all_recruits_arizona.csv")
head(data)

# height not usable as string, need to convert into numeric (inches)


# Convert height strings to inches
data <- data %>%
  mutate(
    Height_in = str_extract(Height, "[0-9]+") %>% as.numeric() * 12 +
      str_extract(Height, "(?<=-)[0-9.]+") %>% as.numeric()
  )
#View(data)


# By Position on same team...
ggplot(data, aes(x = Position, y = Weight)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(title = "Player Weight Distribution by Team", y = "Weight (lbs)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Height_in, y = Weight, color = Position)) +
  geom_point(alpha = 0.8) +
  labs(title = "Height vs Weight by Team", x = "Height (inches)", y = "Weight (lbs)") +
  theme_minimal()

avg_profile <- data %>%
  group_by(Position) %>%
  summarise(
    Avg_Height = mean(Height_in, na.rm = TRUE),
    Avg_Weight = mean(Weight, na.rm = TRUE)
  )

ggplot(avg_profile, aes(x = Position, y = Avg_Weight)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Average Player Weight", y = "Avg Weight")

ggplot(avg_profile, aes(x = Position, y = Avg_Height)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Average Player Height", y = "Avg Height")

## Compare by TEAM

# Get all data
files_here <- paste0(getwd(),"/recruit_csvs/football/")
files <- dir(files_here)

x=1
while (x <= length(files)) {
  if (x==1) {
    all_data <- read_csv(paste0(files_here,"/",files[x]))
  }else {
    more_data <- read_csv(paste0(files_here,"/",files[x]))
    all_data <-rbind(all_data, more_data)
  }
  x=x+1
}
View(all_data)

# save file as backup
#write_csv(all_data, "all_recruits_BIG12.csv")

# Convert height strings to inches
all_data <- all_data %>%
  mutate(
    Height_in = str_extract(Height, "[0-9]+") %>% as.numeric() * 12 +
      str_extract(Height, "(?<=-)[0-9.]+") %>% as.numeric()
  )
base::summary(all_data)

# clean up data, dont want NA's or 0's
all_data <- all_data %>% 
  filter(Height_in > 0) %>% 
  filter(Weight > 0)
# girth metrics (Height x Weight)
all_data <- all_data %>%
  mutate(Girth_Score = Height_in * Weight)
all_data_size <- all_data %>%
  mutate(Size_Index = round((Weight / (Height_in)^2) * 703, 1))
all_data_size <- all_data_size %>%
  mutate(Size_Z = scale(Height_in * Weight))

# custom to target positions ->
# look at positions to select from
unique(all_data$Position)

# Offensive linemen
big_men_O <- all_data_size %>% 
  filter(Position %in% c("OG","OT","OC"))
#View(big_men_O)

# Deffensive lineman
big_men_d <- all_data_size %>% 
  filter(Position %in% c("WDE","SDE","DT","DL"))

big_men_all <- all_data_size %>% 
  filter(Position %in% c("OG","OT","OC","WDE","SDE","DT","DL"))

# For specific years
big_men_all <- big_men_all %>% 
  filter(Year %in% c("2025","2026"))

ggplot(big_men_all, aes(x = School, y = Girth_Score)) +
  geom_boxplot(fill = "firebrick", alpha = 0.6) +
  labs(title = "Player Size (Girth Score - Height x Weight - BMI) by Team", y = "Height × Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(big_men_all, aes(x = School, y = Size_Index)) +
  geom_boxplot(fill = "firebrick", alpha = 0.6) +
  labs(title = "Player Size (Girth Score - Height x Weight - BMI) by Team", y = "BMI (~Height × Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(big_men_all, aes(x = School, y = Size_Z)) +
  geom_boxplot(fill = "firebrick", alpha = 0.6) +
  labs(title = "Player Size (Girth Score - Height x Weight - Normalized) by Team", y = "Z-score (~Height × Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# add label ->
label_data <- big_men_all %>%
  group_by(School) %>%
  slice_max(order_by = Size_Z, n = 1, with_ties = FALSE) %>%
  ungroup()

ggplot(big_men_all, aes(x = School, y = Size_Z)) +
  geom_boxplot(fill = "firebrick", alpha = 0.6) +
  geom_text(data = label_data,
            aes(label = Name),  # or use paste(Name, round(Size_Z, 1))
            vjust = -0.5,
            size = 3,
            color = "black") +
  labs(title = "Player Size (Girth Score - Height x Weight - Normalized) by Team",
       y = "Z-score (~Height × Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## lets do it for basketabll ->
# Get all data
files_here <- paste0(getwd(),"/recruit_csvs/basketball/")
files <- dir(files_here)

x=1
while (x <= length(files)) {
  if (x==1) {
    all_data <- read_csv(paste0(files_here,"/",files[x]))
  }else {
    more_data <- read_csv(paste0(files_here,"/",files[x]))
    all_data <-rbind(all_data, more_data)
  }
  x=x+1
}
View(all_data)

# save file as backup
#write_csv(all_data, "all_recruits_BIG12_basketball.csv")

# Convert height strings to inches
all_data <- all_data %>%
  mutate(
    Height_in = str_extract(Height, "[0-9]+") %>% as.numeric() * 12 +
      str_extract(Height, "(?<=-)[0-9.]+") %>% as.numeric()
  )
base::summary(all_data)

# clean up data, dont want NA's or 0's
all_data <- all_data %>% 
  filter(Height_in > 0) %>% 
  filter(Weight > 0)
# girth metrics (Height x Weight)
all_data <- all_data %>%
  mutate(Girth_Score = Height_in * Weight)
all_data_size <- all_data %>%
  mutate(Size_Index = round((Weight / (Height_in)^2) * 703, 1))
all_data_size <- all_data_size %>%
  mutate(Size_Z = scale(Height_in * Weight))
#View(all_data_size)

# restrict years
all_data_size_byYear <- all_data_size %>%
  filter(Year %in% c("2023","2024","2025"))
#View(all_data_size_byYear)

# add label ->
# label_data <- all_data_size_byYear %>%
#   group_by(School) %>%
#   slice_max(order_by = Size_Z, n = 1, with_ties = FALSE) %>%
#   ungroup()

label_data <- all_data_size_byYear %>%
  group_by(School) %>%
  slice_max(order_by = Size_Z, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label_combo = paste0(Name, "\n(",Height,",",Weight,"lbs.)"))

ggplot(all_data_size_byYear, aes(x = School, y = Size_Z)) +
  geom_boxplot(fill = "firebrick", alpha = 0.6) +
  geom_text(data = label_data,
            aes(label = label_combo),  # or use paste(Name, round(Size_Z, 1))
            vjust = -0.5,
            size = 3,
            color = "black") +
  labs(title = "Player Size (Girth Score - Height x Weight - Normalized) by Team",
       y = "Z-score (~Height × Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## update with dots
ggplot(all_data_size_byYear, aes(x = School, y = Size_Z)) +
  geom_boxplot(fill = "firebrick", alpha = 0.6) +
  geom_point(data = label_data,
             aes(x = School, y = Size_Z),
             color = "black", size = 2) +  # or use team color if you have one
  geom_text(data = label_data,
            aes(label = label_combo),
            vjust = -0.5,
            size = 3,
            color = "black") +
  labs(title = "Player Size (Girth Score - Height x Weight - Normalized) by Team",
       y = "Z-score (~Height × Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
