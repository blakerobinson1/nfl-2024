library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(dplyr)

pbp <- load_pbp(1999:2024)

#Creating a map of the receivers to their player_ids
wr_names_ids <- pbp |> 
  filter(receiver %in% c('T.Holt', 'R.Wayne', 'S.Smith', 'B.Marshall', 'A.Johnson', 'M.Evans')) |> 
  filter(receiver_player_id %in% c('00-0007681', '00-0020498', '00-0020337', '00-0024334', '00-0022044', '00-0031408')) |> 
  distinct(receiver, receiver_player_id)

#Giving Mike Evans a photo
player_photos <- data.frame(
  player = c('Mike Evans'),
  photo = c('https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/16737.png')
)

#Sourced hex values from teams_colors_logos but set my own colors because all 5 blues overlapped 
mod_tcl <- data.frame(
  player = c('Torry Holt', 'Reggie Wayne', 'Steve Smith', 'Brandon Marshall', 'Andre Johnson', 'Mike Evans'),
  team_color = c('#FFD100', '#002C5F', '#0085CA', '#C83803', '#A71930', 'white')
)

#Color Mapping used for scale_fill_manual
color_mapping <- setNames(epa_by_season_all$team_color, epa_by_season_all$player)

teams_colors_logos |> 
  filter(team_abbr %in% c('LA', 'IND', 'CAR', 'CHI', 'HOU'))
  
#Getting EPA data for completed passes to our receivers
wr_epa_play <- pbp |> 
  filter(pass == 1 & receiver_player_id %in% c('00-0007681', '00-0020498', '00-0020337', '00-0024334', '00-0022044', '00-0031408')) |>
  filter(ifelse(season < 2021, week <= 17, week <= 18)) |> 
  filter(complete_pass == 1)

#For each WR, create total epa and divide that by number of plays by year, 
#Add readable player name, organize it by season instead of year
holt_epa_play <- wr_epa_play |> 
  filter(receiver_player_id == '00-0007681') |> 
  group_by(season) |> 
  summarize(total_epa = sum(epa, na.rm = T),
            total_plays = n()) |> 
  mutate(epa_per_play = total_epa / total_plays,
         player = 'Torry Holt',
         year = row_number())

smith_epa_play <- wr_epa_play |> 
  filter(receiver_player_id == '00-0020337') |> 
  group_by(season) |> 
  summarize(total_epa = sum(epa, na.rm = T),
            total_plays = n()) |> 
  mutate(epa_per_play = total_epa / total_plays,
         player = 'Steve Smith',
         year = row_number())

wayne_epa_play <- wr_epa_play |> 
  filter(receiver_player_id == '00-0020498') |> 
  group_by(season) |> 
  summarize(total_epa = sum(epa, na.rm = T),
            total_plays = n()) |> 
  mutate(epa_per_play = total_epa / total_plays,
         player = 'Reggie Wayne',
         year = row_number())

johnson_epa_play <- wr_epa_play |> 
  filter(receiver_player_id == '00-0022044') |> 
  group_by(season) |> 
  summarize(total_epa = sum(epa, na.rm = T),
            total_plays = n()) |> 
  mutate(epa_per_play = total_epa / total_plays,
         player = 'Andre Johnson',
         year = row_number())

marshall_epa_play <- wr_epa_play |> 
  filter(receiver_player_id == '00-0024334') |> 
  group_by(season) |> 
  summarize(total_epa = sum(epa, na.rm = T),
            total_plays = n()) |> 
  mutate(epa_per_play = total_epa / total_plays,
         player = 'Brandon Marshall',
         year = row_number())

evans_epa_play <- wr_epa_play |> 
  filter(receiver_player_id == '00-0031408') |> 
  group_by(season) |> 
  summarize(total_epa = sum(epa, na.rm = T),
            total_plays = n()) |> 
  mutate(epa_per_play = total_epa / total_plays,
         player = 'Mike Evans',
         year = row_number())


#Bind_rows to combine the 6 datasets, join the picture and colors
epa_by_season_all <- bind_rows(holt_epa_play, wayne_epa_play, smith_epa_play, johnson_epa_play, marshall_epa_play, evans_epa_play) |>
  filter(year <= 11) |> 
  left_join(player_photos, by = c('player')) |> 
  left_join(mod_tcl, by = c('player'))

epa_by_season_all |>
  ggplot(aes(x = year, y = epa_per_play)) +
  geom_line(data = epa_by_season_all |> filter(player == "Mike Evans"),
            aes(group = player), 
            color = "black",  
            size = 0.5,
            show.legend = FALSE) +  # Creates the Mike Evans trend line, removes from the final graph
  geom_point(aes(fill = player), shape = 21, size = 6.5, alpha = 0.9, stroke = NA) +  # Fill and color by player
  geom_image(aes(image = photo), size = 0.05, asp = 16/9) + # Mike Evans picture
  scale_fill_manual(values = color_mapping, name = "Player") +  # Use color mapping for fill
  scale_color_identity(guide = "none") + # Remove second legend 
  theme_bw() +
  labs(x = "Season",
       y = "EPA/reception",
       title = "EPA/reception by season for notable Hall of Fame hopeful receivers",
       subtitle = "Through first 11 seasons of players' careers",
       caption = "Blake Robinson | @BlakeRbsn | data: nflfastR | Inspired by @arjunmenon100"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + # continuous fixes the graph scale
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        legend.position = "right", 
        panel.grid.minor.x = element_blank()) + # Removes extra gridlines
  guides(
    fill = guide_legend(override.aes = list(size = 5))  # Customize point size in legend
  )
ggsave('Evans_EPA_reception_vs_field.png', width = 14, height = 10, dpi = "retina")
