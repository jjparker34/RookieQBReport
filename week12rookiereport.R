library(vip)
library(caret)
library(nflfastR)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggimage)

week12QB <- read.csv('FPDweek12.csv')
#filter Fantasy points data for rookies only
rookieQBs <- week12QB %>% 
  filter(Name == "Drake Maye" | Name == "Caleb Williams" | Name == "Bo Nix" | Name == "Jayden Daniels" | Name == "Michael Penix")
#find rookie pff grades
rookiePFFGrades <- read.csv('PFFweek12Rookies.csv')
#filter PFF data for rookies only
rookiePFFGrades <- rookiePFFGrades %>% 
  filter(player == "Drake Maye" | player == "Caleb Williams" | player == "Bo Nix" | player == "Jayden Daniels" | player == "Michael Penix Jr.") %>% 
  select(player, player_id, grades_offense) %>% 
  mutate(player = ifelse(player == "Michael Penix Jr.", "Michael Penix", player))


#combine the two datasets
rookiePFFGrades <- rookiePFFGrades %>% 
  rename(Name = player)
  
rookieQBs <- left_join(rookieQBs,rookiePFFGrades, by = 'Name')

rookieQBs <- rookieQBs %>%
  mutate(
    headshot_url = case_when(
      Name == "Drake Maye" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4431452.png&w=350&h=254",
      Name == "Caleb Williams" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4431611.png",
      Name == "Bo Nix" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4426338.png",
      Name == "Jayden Daniels" ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/4426348.png",
      Name == "Michael Penix" ~ "https://a.espncdn.com/i/headshots/nfl/players/full/4360423.png",
      TRUE ~ NA_character_
    )
  )

rookieQBdata <- rookieQBs %>% 
  select(Name,headshot_url, DB, YDS, YDS.G, TD, INT,CPOE, aDOT,ACC..,OFF..,HERO..,TWT..,PRESS.SK..,FP.G,grades_offense)

rookieQBdata %>%
  gt(rowname_col = "Name") %>%
  text_transform(
    locations = cells_body(columns = headshot_url),
    fn = function(x) {
      web_image(url = x, height = 50) # Adjust height as needed
    }
  ) %>%
  tab_header(
    title = "Rookie Quarterback Report",
    subtitle = "Week 1-12"
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = c(grades_offense),
      rows = grades_offense > 90
    )
  ) %>%
  cols_label(
    Name = "Player",
    headshot_url = " ",
    YDS.G = "YDS/G",
    ACC.. = "ACC%",
    OFF.. = "OFF%",
    HERO.. = "HERO%",
    TWT.. = "TWT%",
    PRESS.SK.. = "P2S%",
    FP.G = "FP/G",
    grades_offense = "PFF"
  ) %>% 
  data_color(
    columns = c(`grades_offense`),
    palette = c('purple4','orange4','green4'),
    domain = NULL
  ) 

