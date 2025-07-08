# Ranked UA recruiting evaluations from 2010 to present (unranked players not included)
# this is also excluding transfers

# Load the necessary library
library(rvest)
library(dplyr)
library(ggplot2)


## set environment path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
app_path<-getwd()

# years to evaluate, rankings started on 247 in 2010
year <- c(2011:2026)
i=1
sport <- "basketball"
# get player data for each year and create dataframe with all data
while (i <= length(year)) {
  call <- paste0("https://247sports.com/college/arizona/season/",year[i],"-",sport,"/commits/")
  page <- read_html(call)
  
  # give time to connect to webpage
  Sys.sleep(15)
  
  scores <- page %>%
    html_nodes(".score , .ri-page__name-link") %>%
    html_text(trim = TRUE)
  
  # clean data
  playerRankings <- scores[scores != "Rating"]
  playerRankings <- playerRankings[!grepl("^N/A",playerRankings)]
  playerRankings <- playerRankings[!grepl("^Commit",playerRankings)]
  playerRankings <- playerRankings[!grepl("\\(HS\\)",playerRankings)]
  playerRankings <- playerRankings[!grepl("\\(T\\)",playerRankings)]
  playerRankings <- playerRankings[!is.na(playerRankings)]
  
  # Reshape into a data frame by alternating pairs
  name_vec <- playerRankings[seq(1, length(playerRankings), by = 2)]
  rank_vec <- playerRankings[seq(2, length(playerRankings), by = 2)]
  
  player_df <- data.frame(
    Name = name_vec,
    Ranking = rank_vec,
    stringsAsFactors = FALSE
  )
  
  playerRankings
  year[i]
  
  # clean up unranked players
  player_df <- player_df[player_df$Ranking != 'NA',]
  
  # add year
  player_df$Year <- year[i]
  
  if (i==1) {
    rec_data <- player_df
  } else {
    rec_data <-rbind(rec_data, player_df)
  }
  
  print(paste0("Year ",year[i]," complete..."))
  # move on to next year
  i = i+1
  
  if (i==length(year)+1) {
    
    # Convert Ranking to numeric (if it's a character)
    rec_data$Ranking <- as.numeric(rec_data$Ranking)
    
    # Sort by Year (ascending), then Ranking (descending), then Name (alphabetically)
    ordered_recruits <- rec_data %>%
      arrange(Year, desc(Ranking), Name)
    
    View(rec_data)
    print("Complete!")
  }
  
}

all_recruits <- rec_data

# Plot individual rankings with year on x-axis
ggplot(all_recruits, aes(x = Year, y = Ranking)) +
  geom_point(alpha = 0.6, color = "steelblue") +  # individual recruit dots
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed") +  # trend line
  scale_y_continuous(limits = c(0, 100)) +  # adjust based on ranking scale
  labs(
    title = "UA Recruit Rankings by Year",
    x = "Recruiting Year",
    y = "Player Ranking",
    caption = "Each dot represents an individual recruit. Trend line shows average recruiting score over time."
  ) +
  theme_minimal()


# Compute average by year
avg_rank <- all_recruits %>%
  group_by(Year) %>%
  summarize(Average_Ranking = mean(Ranking, na.rm = TRUE))

# Plot average trend
ggplot(avg_rank, aes(x = Year, y = Average_Ranking)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Average Recruit Ranking Over Time",
    x = "Year",
    y = "Average Ranking",
    caption = "Higher values = stronger recruits"
  ) +
  theme_minimal()

## ->
# Build combined plot
ggplot(all_recruits, aes(x = Year, y = Ranking)) +
  geom_point(alpha = 0.6, color = "steelblue") +                         # individual recruit dots
  geom_smooth(method = "loess", se = FALSE, color = "firebrick", linetype = "dashed") +  # overall trend
  geom_line(data = avg_rank, aes(x = Year, y = Average_Ranking), 
            color = "orange", size = 1.2) +                              # yearly average line
  geom_point(data = avg_rank, aes(x = Year, y = Average_Ranking), 
             color = "orange", size = 2.2) +                             # highlight average points
  scale_y_continuous(limits = c(50, 100)) +
  labs(
    title = "Recruit Rankings and Yearly Trends",
    x = "Recruiting Year",
    y = "Player Ranking",
    caption = "Blue dots = individual recruits, orange line = yearly average, dashed red = smoothed trend"
  ) +
  theme_minimal()


## more!!
# Find top recruit per year
top_recruits <- all_recruits %>%
  group_by(Year) %>%
  filter(Ranking == max(Ranking, na.rm = TRUE)) %>%
  slice(1) %>%  # handles ties by taking the first
  ungroup() %>%
  mutate(label = paste0(Name, " (", Ranking, ")"))


ggplot(all_recruits, aes(x = Year, y = Ranking)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick", linetype = "dashed") +
  geom_line(data = avg_rank, aes(x = Year, y = Average_Ranking), color = "orange", size = 1.2) +
  geom_point(data = avg_rank, aes(x = Year, y = Average_Ranking), color = "orange", size = 2.2) +
  geom_text(data = top_recruits, aes(x = Year, y = Ranking, label = label), 
            color = "red", vjust = -1, fontface = "bold", size = 1.7) +
  scale_y_continuous(limits = c(65, 100)) +
  labs(
    title = "UA Recruit Rankings (2010-2026)",
    x = "Recruiting Class Year",
    y = "247Sports Player Ranking",
    caption = "Blue dots = recruits, orange line = yearly average, green labels = top recruit each year"
  ) +
  theme_minimal()



## okay moreee!!!
percentile_data <- all_recruits %>%
  group_by(Year) %>%
  summarize(
    p25 = quantile(Ranking, 0.25, na.rm = TRUE),
    p50 = quantile(Ranking, 0.50, na.rm = TRUE),
    p75 = quantile(Ranking, 0.75, na.rm = TRUE)
  )

## hmm...
lowest_recruits <- all_recruits %>%
  group_by(Year) %>%
  filter(Ranking == min(Ranking, na.rm = TRUE)) %>%
  slice(1) %>%  # in case of ties
  ungroup() %>%
  mutate(label = paste0(Name, " (", Ranking, ")"))

ggplot(all_recruits, aes(x = Year, y = Ranking)) +
  # Percentile ribbon for distribution insight
  geom_ribbon(data = percentile_data, aes(x = Year, ymin = p25, ymax = p75),
              inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  
  # Individual recruit dots
  geom_point(alpha = 0.7, color = "steelblue") +
  
  # LOESS smoothed trend line with confidence band
  geom_smooth(method = "loess", se = TRUE, color = "firebrick",
              fill = "pink", linetype = "dashed", alpha = 0.2) +
  
  # Line for yearly average
  geom_line(data = avg_rank, aes(x = Year, y = Average_Ranking), 
            color = "orange", size = 1.2) +
  geom_point(data = avg_rank, aes(x = Year, y = Average_Ranking), 
             color = "orange", size = 2.2) +
  
  # Top recruit label
  geom_text(data = top_recruits, aes(x = Year, y = Ranking, label = label),
            color = "darkgreen", fontface = "bold", size = 2, vjust = -1) +
  
  # # Lowest recruit label
  # geom_text(data = lowest_recruits, aes(x = Year, y = Ranking, label = label),
  #           color = "cadetblue", fontface = "bold", size = 1.4, vjust = -1) +
  # 
  
  # Elite recruit marker line
  geom_hline(yintercept = 90, linetype = "dotted", color = "gray") +
  annotate("text", x = min(all_recruits$Year), y = 91.5,
           label = "Elite Tier (90+)", hjust = 0, color = "gray", size = 2.5) +
  
  # Plot styling
  scale_y_continuous(limits = c(65, 100)) +
  labs(
    title = "UA Football Commits Class Rankings (2010-2026)",
    x = "Recruiting Class Year",
    y = "247Sports Player Ranking",
    subtitle = "Shaded areas show percentile ranges, dashed line shows trend, orange marks yearly average",
    caption = "*Green labels = top recruit each year (excluding transfers)"
  ) +
  theme_minimal()

# exported as pdf, 5 x 10
































## check out as plotly

all_recruits <- all_recruits %>%
  mutate(hover_label = Name)

plot <- ggplot(all_recruits, aes(x = Year, y = Ranking)) +
  # Percentile ribbon for distribution insight
  geom_ribbon(data = percentile_data, aes(x = Year, ymin = p25, ymax = p75),
              inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  
  # Individual recruit dots
  geom_point(alpha = 0.7, color = "steelblue") +
  
  # LOESS smoothed trend line with confidence band
  geom_smooth(method = "loess", se = TRUE, color = "firebrick",
              fill = "pink", linetype = "dashed", alpha = 0.2) +
  
  # Line for yearly average
  geom_line(data = avg_rank, aes(x = Year, y = Average_Ranking), 
            color = "orange", size = 1.2) +
  geom_point(data = avg_rank, aes(x = Year, y = Average_Ranking), 
             color = "orange", size = 2.2) +
  
  # Top recruit label
  geom_text(data = top_recruits, aes(x = Year, y = Ranking, label = label),
            color = "darkgreen", fontface = "bold", size = 2, vjust = -1) +
  
  # # Lowest recruit label
  # geom_text(data = lowest_recruits, aes(x = Year, y = Ranking, label = label),
  #           color = "cadetblue", fontface = "bold", size = 1.4, vjust = -1) +
  # 
  
  # Elite recruit marker line
  geom_hline(yintercept = 90, linetype = "dotted", color = "gray") +
  annotate("text", x = min(all_recruits$Year), y = 91.5,
           label = "Elite Tier (90+)", hjust = 0, color = "gray", size = 2.5) +
  
  # Plot styling
  scale_y_continuous(limits = c(65, 100)) +
  labs(
    title = "UA Football Commits Class Rankings (2010-2026)",
    x = "Recruiting Class Year",
    y = "247Sports Player Ranking",
    subtitle = "Shaded areas show percentile ranges, dashed line shows trend, orange marks yearly average",
    caption = "*Green labels = top recruit each year (excluding transfers)"
  ) +
  theme_minimal()

plotly::ggplotly(plot)





