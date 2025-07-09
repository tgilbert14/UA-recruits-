# Ranked UA recruiting evaluations from 2010 to present (unranked players not included)
# this is also excluding transfers

# Load the necessary library
library(rvest)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)


## set environment path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
app_path<-getwd()


#sport <- "basketball"
sport <- "football"
# example of BIG12 schools
school <- c("arizona","arizona-state","baylor","byu","cincinnati","colorado",
            "houston","iowa-state","kansas","kansas-state","oklahoma-state",
            "tcu","texas-tech","central-florida","utah","west-virginia")

# years to evaluate, rankings started on 247 in 2010
year <- c(2016:2026)

sport <- tolower(sport)
school <- tolower(school)

s=1
# for each school
while (s <= length(school)) {

  i=1
  # get player data for each year and create dataframe with all data
  while (i <= length(year)) {
    call <- paste0("https://247sports.com/college/",school[s],"/season/",year[i],"-",sport,"/commits/")
    page <- read_html(call)
    
    # give time to connect to webpage
    #Sys.sleep(10)
    
    scores <- page %>%
      html_nodes(".ri-page__star-and-score .score , .ri-page__name-link , .wrapper .position , .wrapper .metrics , .posrank , .withDate , .meta , .sttrank , .natrank") %>%
      html_text(trim = TRUE)
    
    # getting rid of commits with no scores
    playerRankings <- scores[scores != "Rating"]
    #playerRankings <- playerRankings[!grepl("^N/A",playerRankings)]
    playerRankings <- playerRankings[!grepl("^Commit",playerRankings)]
    #playerRankings <- playerRankings[!grepl("\\(HS\\)",playerRankings)]
    #playerRankings <- playerRankings[!grepl("\\(T\\)",playerRankings)]
    #playerRankings <- playerRankings[!is.na(playerRankings)]
    
    if (length(playerRankings) > 0) {
      
      # Reshape into a data frame by alternating pairs - if there is data
      name_vec <- playerRankings[seq(1, length(playerRankings), by = 8)]
      rank_vec <- playerRankings[seq(4, length(playerRankings), by = 8)]
      natRank_vec <- playerRankings[seq(5, length(playerRankings), by = 8)]
      posRank_vec <- playerRankings[seq(6, length(playerRankings), by = 8)]
      stateRank_vec <- playerRankings[seq(7, length(playerRankings), by = 8)]
      pos_vec <- playerRankings[seq(8, length(playerRankings), by = 8)]
      # splitting out height and weights
      meta_vec <- playerRankings[seq(3, length(playerRankings), by = 8)]
      meta_vec.clean <- gsub(" ", "", meta_vec)
      split_values <- strsplit(meta_vec.clean, "/")
      split_values.unlist <- unlist(split_values)
      height_vec <- split_values.unlist[seq(1, length(split_values.unlist), by = 2)]
      weight_vec <- split_values.unlist[seq(2, length(split_values.unlist), by = 2)]
      # splitting out state from location
      loc_vec <- playerRankings[seq(2, length(playerRankings), by = 8)]
      loc_vec.clean <- gsub(" ", "", loc_vec)
      split_state <- strsplit(loc_vec.clean, ",")
      split_state.unlist <- unlist(split_state)
      state_vec <- gsub(")","",split_state.unlist[seq(2, length(split_state.unlist), by = 2)])
      
      player_df <- data.frame(
        Name = name_vec,
        Location = loc_vec,
        Height = height_vec,
        Weight = weight_vec,
        Ranking = rank_vec,
        NationalRank = natRank_vec,
        PositionRank = posRank_vec,
        StateRank = stateRank_vec,
        State = state_vec,
        Position = pos_vec,
        stringsAsFactors = FALSE)
      #View(player_df)
      
      # clean up unranked players
      player_df <- player_df[player_df$Ranking != 'NA',]
      # add year
      player_df$Year <- year[i]
      # add school[s]
      player_df$School <- school[s]
      # add recuit type
      player_df$Type <- "Commit"
      
      
      # check for transfers -->
      transfer.meta <- page %>%
        html_nodes(".player .score , .portal-list_itm .position , .player .metrics , .player a") %>%
        html_text(trim = TRUE)
      
      # Find indices of all T-ratings
      t_indices <- which(str_detect(transfer.meta, "\\(T\\)"))
      
      # Extract each recruit’s full info from T rating location
      recruits_clean <- lapply(t_indices, function(i) {
        name <- transfer.meta[i - 2]
        size <- transfer.meta[i - 1]
        rating <- transfer.meta[i]
        position <- transfer.meta[i + 2]
        tibble(Name = name, Size = size, Rating = rating, Position = position)
      })
      
      # Combine into a clean dataframe
      transfer.playerRankings <- bind_rows(recruits_clean)
      #print(transfer.playerRankings)
      
      if(nrow(transfer.playerRankings) > 0) {
        # Reshape into a data frame by alternating pairs - if there is data
        name_vec <- transfer.playerRankings$Name
        pos_vec <- transfer.playerRankings$Position
        
        # clean up scores
        rank_vec.raw <- transfer.playerRankings$Rating
        rank_vec.num <- gsub(" \\(.*\\)", "", rank_vec.raw)
        rank_vec.numeric <- as.numeric(rank_vec.num)
        rank_vec <- round(rank_vec.numeric*100, 0)
        
        # splitting out height and weights
        meta_vec <- transfer.playerRankings$Size
        meta_vec.clean <- gsub(" ", "", meta_vec)
        split_values <- strsplit(meta_vec.clean, "/")
        split_values.unlist <- unlist(split_values)
        height_vec <- split_values.unlist[seq(1, length(split_values.unlist), by = 2)]
        weight_vec <- split_values.unlist[seq(2, length(split_values.unlist), by = 2)]
        
        player_df.transfers <- data.frame(
          Name = name_vec,
          Location = NA,
          Height = height_vec,
          Weight = weight_vec,
          Ranking = rank_vec,
          NationalRank = NA,
          PositionRank = NA,
          StateRank = NA,
          State = NA,
          Position = pos_vec,
          stringsAsFactors = FALSE)
        
        # clean up unranked players
        player_df.transfers <- player_df.transfers[player_df.transfers$Ranking != 'NA',]
        # add year
        player_df.transfers$Year <- year[i]
        # add school
        player_df.transfers$School <- school[s]
        # add recuit type
        player_df.transfers$Type <- "Transfer"
        
        # bind to other commits
        player_df <- rbind(player_df, player_df.transfers)
      }
      
      # create new dataframe or add to it
      if (i==1) {
        rec_data <- player_df
      } else {
        rec_data <-rbind(rec_data, player_df)
      }
    }
    #View(rec_data)
    print(paste0("Year ",year[i]," complete for ",school[s],"..."))
    # move on to next year
    i = i+1
    
    # If last scrap, create plot
    if (i==length(year)+1) {
      
      # Convert Ranking to numeric (if it's a character)

      rec_data <- rec_data %>%
        mutate(
          Ranking = as.numeric(Ranking),
          Weight = as.numeric(Weight),
          NationalRank = readr::parse_number(NationalRank),
          PositionRank = readr::parse_number(PositionRank),
          StateRank = readr::parse_number(StateRank)
        )
      
      # Sort by Year (ascending), then Ranking (descending), then Name (alphabetically)
      ordered_recruits <- rec_data %>%
        arrange(Year, desc(Ranking), Name)
      
      all_recruits <- rec_data %>% 
        dplyr::filter(!is.na(Name))
      
      all_recruits <- unique(all_recruits)
      
      
      # Plot individual rankings with year on x-axis -->
      
      # Compute average by year
      avg_rank <- all_recruits %>%
        group_by(Year) %>%
        summarize(Average_Ranking = mean(Ranking, na.rm = TRUE))
      
      # Find top recruit per year
      top_recruits <- all_recruits %>%
        group_by(Year) %>%
        filter(Ranking == max(Ranking, na.rm = TRUE)) %>%
        slice(1) %>%  # handles ties by taking the first
        ungroup() %>%
        mutate(label = paste0(Name, " (", Ranking, ")"))
      
      ## hmm...
      lowest_recruits <- all_recruits %>%
        group_by(Year) %>%
        filter(Ranking == min(Ranking, na.rm = TRUE)) %>%
        slice(1) %>%  # in case of ties
        ungroup() %>%
        mutate(label = paste0(Name, " (", Ranking, ")"))
      
      # Count recruits per score-year combo
      dot_sizes <- all_recruits %>%
        group_by(Year, Ranking) %>%
        summarize(count = n(), .groups = "drop")
      
      # Merge with main data
      all_recruits <- all_recruits %>%
        left_join(dot_sizes, by = c("Year", "Ranking"))
      
      top_recruits <- all_recruits %>%
        group_by(Year) %>%
        arrange(desc(Ranking)) %>%
        slice_max(order_by = Ranking, n = 1, with_ties = TRUE) %>%
        ungroup() %>%
        mutate(label = paste0(Name, "\n(", Ranking, ")"))
      
      # Add percentiles
      percentile_data <- all_recruits %>%
        group_by(Year) %>%
        summarize(
          p25 = quantile(Ranking, 0.25, na.rm = TRUE),
          p50 = quantile(Ranking, 0.50, na.rm = TRUE),
          p75 = quantile(Ranking, 0.75, na.rm = TRUE)
        )
      
      year_lines <- unique(all_recruits$Year)
      
      
      ggplot(all_recruits, aes(x = Year, y = Ranking, color = Type)) +
        # Percentile ribbon for distribution insight
        geom_ribbon(data = percentile_data, aes(x = Year, ymin = p25, ymax = p75),
                    inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
        
        # Year divider lines
        geom_vline(xintercept = year_lines, color = "gray90", linetype = "solid", size = 0.3) +
        
        # LOESS smoothed trend line with confidence band
        geom_smooth(method = "loess", se = TRUE, color = "firebrick",
                    fill = "pink", linetype = "dashed", alpha = 0.1) +
        
        # Elite tier marker
        geom_hline(yintercept = 90, linetype = "dotted", color = "gray") +
        annotate("text", x = min(all_recruits$Year), y = 91.5,
                 label = "Elite Tier (90+)", hjust = 0, color = "gray", size = 2.5) +
        
        # Customize legend and scales
        scale_color_manual(values = c("Commit" = "blue", "Transfer" = "firebrick")) +
        scale_size_continuous(range = c(2.5, 6), name = "Recruits at Rating") +
        scale_y_continuous(limits = c(min(all_recruits$Ranking)-1, max(all_recruits$Ranking)+1)) +
        scale_x_continuous(expand = expansion(mult = c(0.04, 0.04))) +
        
        # Yearly average line and points
        geom_line(data = avg_rank, aes(x = Year, y = Average_Ranking), 
                  inherit.aes = FALSE, color = "orange", size = 0.6) +
        geom_point(data = avg_rank, aes(x = Year, y = Average_Ranking), 
                   inherit.aes = FALSE, color = "orange", size = 1.2) +
        
        # Recruit dots with jitter
        geom_point(position = position_jitter(width = 0.12), alpha = 0.3) +
        
        # Label every recruit using ggrepel
        geom_text_repel(
          data = top_recruits,
          aes(x = Year, y = Ranking, label = label),
          size = 2,
          fontface = "bold",
          segment.color = "blue",
          box.padding = 0.2,
          point.padding = 0.1,
          max.overlaps = Inf,
          show.legend = FALSE
        ) +
        
        # # Top recruit label
        # geom_text(data = top_recruits, aes(x = Year, y = Ranking, label = label),
        #           color = "darkgreen", fontface = "bold", size = 1.4, vjust = -1) +
        
        # # Lowest recruit label
        # geom_text(data = lowest_recruits, aes(x = Year, y = Ranking, label = label),
        #           color = "gray", fontface = "bold", size = 2, vjust = -1) +
        
        # Final labels and styling
        labs(
          title = paste0(str_to_title(school[s]), " ", str_to_title(sport), " Commits Class Rankings (", min(all_recruits$Year), "–", max(all_recruits$Year), ")"),
          x = "Recruiting Class Year",
          y = "247Sports Player Ranking",
          subtitle = "Gray band = percentile range, Orange = yearly average",
          caption = "*Blue = Top recruit(s) per year • Red = lowest ranked recruit"
        ) +
        theme_minimal()
      # exported as pdf, 5 x 10 or png 1000 width x 500ish
      
      # Save
      file_name <- paste0(school[s],"_",sport,"_classRatings_",Sys.Date(),".png")
      file_png <- paste0(app_path,"/plots/",sport,"/",file_name)
      ggsave(filename = file_png, width = 12, height = 8, dpi = 300, bg = "white")
      # Auto-open the file in your default viewer (Windows)
      shell.exec(file_png)
      
      # Save school[s] data as csv for comparison later...
      school_clean <- tolower(gsub(" ", "_", school[s]))
      var_name <- paste0("all_recruits_", school_clean)
      assign(var_name, all_recruits)
      file_loc <- paste0(app_path,"/recruit_csvs/",sport,"/",var_name,".csv")
      # Save CSV
      write.csv(all_recruits, file = file_loc, row.names = FALSE)
      #shell.exec(file_loc)
      
      print("Complete!")
    }
    
  }
  # move to next school
  s=s+1
  
  if (s == length(school)+1) {
    print("All schools processed")
  }
}




# Compare schools

files_here <- paste0(app_path,"/recruit_csvs/",sport,"/")
files <- dir(files_here)

x=1
while (x <= length(files)) {
  if (x==1) {
    all_data <- read.csv(paste0(files_here,"/",files[x]))
  }else {
    more_data <- read.csv(paste0(files_here,"/",files[x]))
    all_data <-rbind(all_data, more_data)
  }
  x=x+1
}

# colors
colors <- read.csv(paste0(app_path,"/www/teamColors.csv"))

team_colors <- colors %>%
  mutate(TeamColor = str_extract(colors, "#[0-9a-fA-F]{6}"))

color_map <- setNames(team_colors$TeamColor, team_colors$teams)

# filtered data
filtered_schools <- all_data %>%
  dplyr::filter(School %in% c("arizona", "arizona-state"))

avg_scores_filtered <- filtered_schools %>%
  group_by(Year, School) %>%
  summarize(avg_score = round(mean(Ranking, na.rm = TRUE), 1), .groups = "drop")

ggplot(filtered_schools, aes(x = Year, y = Ranking, color = School)) +
  geom_jitter(width = 0.3, alpha = 0.5, size = 2.2) +
  geom_smooth(method = "loess", se = FALSE, linetype = "solid", size = 1) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Recruiting Rankings Comparison",
    subtitle = paste("Commit & Transfer Scores [", str_to_title(sport) , "]"),
    x = "Class Year",
    y = "Player Rating 247sports",
    caption = "LOESS trend line per school • Jittered dots per recruit"
  ) +
  #scale_color_manual(values = color_map) +
  scale_color_manual(values = c("arizona"="blue", "arizona-state"="maroon")) +
  geom_text_repel(data = avg_scores_filtered,
                  aes(x = Year, y = avg_score, label = avg_score, color = School),
                  size = 3,
                  fontface = "bold",
                  max.overlaps = Inf,
                  show.legend = FALSE)

#View(all_data)


## all data
avg_scores <- all_data %>%
  group_by(Year, School) %>%
  summarize(avg_score = round(mean(Ranking, na.rm = TRUE), 1), .groups = "drop")

ggplot(all_data, aes(x = Year, y = Ranking, color = School)) +
  geom_smooth(method = "loess", se = FALSE, linetype = "solid", size = 1.2) +
  scale_color_manual(values = color_map) +
  labs(
    title = "Recruiting Rankings Comparison",
    subtitle = paste("Commit & Transfer Scores [", str_to_title(sport), "]"),
    x = "Class Year",
    y = "Player Rating (247Sports)",
    caption = "LOESS trend line per school • Avg. score labels per class"
  )

## push to get via bash
#git remote add origin https://github.com/tgilbert14/UA-recruits-.git
#git pull origin main  # or git pull origin master, depending on your default branch

