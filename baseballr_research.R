# install.packages("devtools")
devtools::install_github("BillPetti/baseballr")

viz_gb_on_period("2019-03-29", "2019-07-05", "AL West")

require(baseballr)
require(tidyverse)
#####everything in this section gets the game id for mlb games, choose the date
?get_game_pks_mlb
games <- get_game_pks_mlb(date = '2019-06-01',
                          level_ids = c(1))
games

games %>%
  select(game_pk, gameDate, teams.away.team.name, teams.home.team.name) %>%
  slice(1:10)
#####
####
payload <- get_pbp_mlb(566401)
bb_palette <- c('Single' = "#006BA4",
                'Double' = "#A2CEEC", 
                'Triple'= "#FFBC79", 
                'Home Run'= "#C85200", 
                'Out/Other' = "#595959")

ggspraychart(payload, 
             x_value = 'hitData.coordinates.coordX', 
             y_value = '-hitData.coordinates.coordY', 
             fill_value = 'batted.ball.result', 
             fill_palette = bb_palette, 
             point_size = 3) +
  labs(title = 'Batted Balls: Seattle Mariners versus LA Angels', 
       subtitle = '2019-06-01')
