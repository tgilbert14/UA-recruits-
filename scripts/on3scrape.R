
#on3 starts transfer data in 2019 -> 
xml_child
library(XML)
html_children(page)

# years to evaluate, rankings started on 247 in 2010
year <- c(2019:2026)
i=1
sport <- "basketball"
#sport <- "Football"
school <- "arizona-wildcats"

call <- paste0("https://www.on3.com/college/",school,"/",sport,"/",year[i],"/industry-comparison-commits/")
page <- read_html(call)

# give time to connect to webpage
#Sys.sleep(10)

scores <- page %>%
  html_nodes(".IndustryComparisonPlayerItem_playerDetailsContainer__h_6qQ0 .MuiTypography-root") %>%
  html_text(trim = TRUE)
scores

View(page)
scores <- page %>%
  html_nodes(".MuiTypography-root MuiTypography-h5 MuiLink-root MuiLink-underlineNone css-1bsn81r a") %>%
  html_text(trim = TRUE)
scores




scores <- page %>%
  html_nodes(".StarRating_rating__0y5XS , .CommitListItem_homeContainer__PUwbq dd , .CommitListItem_vitalsContainer__oOjjB dd , .CommitListItem_homeContainer__PUwbq a , .CommitListItem_nameAndSport__f6aWJ a") %>%
  html_text(trim = TRUE)

scores
