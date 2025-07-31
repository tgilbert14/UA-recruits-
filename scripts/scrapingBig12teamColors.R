# Big 12 color scrape

# get teams
call <- paste0("https://teamcolorcodes.com/ncaa-color-codes/big-12-colors/")
page <- read_html(call)

teams.big10 <- page %>%
  html_nodes(".team-button") %>%
  html_text(trim = TRUE)
teams_big10 <- data.frame("teams"=teams.big10)

teamsColor.big10 <- page %>%
  html_nodes(".team-button") %>%
  html_attr("style")
teamsColor_big10 <- data.frame("colors"=teamsColor.big10)

d1 <- data.frame(teams_big10, teamsColor_big10)

# and pac 12 for new teams
call <- paste0("https://teamcolorcodes.com/ncaa-color-codes/pac-12-colors/")
page <- read_html(call)

teams.p12 <- page %>%
  html_nodes(".team-button") %>%
  html_text(trim = TRUE)
teams_p12 <- data.frame("teams"=teams.p12)

teamsColor.p12 <- page %>%
  html_nodes(".team-button") %>%
  html_attr("style")
teamsColor_p12 <- data.frame("colors"=teamsColor.p12)

d2 <- data.frame(teams_p12, teamsColor_p12)

call <- paste0("https://teamcolorcodes.com/ncaa-color-codes/aac/")
page <- read_html(call)

teams.aac <- page %>%
  html_nodes(".team-button") %>%
  html_text(trim = TRUE)
teams_aac <- data.frame("teams"=teams.aac)

teamsColor.aac <- page %>%
  html_nodes(".team-button") %>%
  html_attr("style")
teamsColor_aac <- data.frame("colors"=teamsColor.aac)

d3 <- data.frame(teams_aac, teamsColor_aac)

call <- paste0("https://teamcolorcodes.com/ncaa-color-codes/west-coast-conference/")
page <- read_html(call)

teams.wc <- page %>%
  html_nodes(".team-button") %>%
  html_text(trim = TRUE)
teams_wc <- data.frame("teams"=teams.wc)

teamsColor.wc <- page %>%
  html_nodes(".team-button") %>%
  html_attr("style")
teamsColor_wc <- data.frame("colors"=teamsColor.wc)

d3_more <- data.frame(teams_wc, teamsColor_wc)

d4 <- rbind(d1,d2)
d5 <- rbind(d4,d3)
d6 <- rbind(d5,d3_more)

d6$teams <- tolower(d6$teams)
d6$teams <- gsub(" ","-",d6$teams)

# school <- c("arizona","arizona-state","baylor","byu","cincinnati","colorado",
#             "houston","iowa-state","kansas","kansas-state","oklahoma-state",
#             "tcu","texas-tech","central-florida","utah","west-virginia")

d6$teams[d6$teams=="central-florida-knights"] <- "central-florida"
#write.csv(d6, file = "www/teamColors.csv", row.names = FALSE)

# had to edit sheet to match scraping
