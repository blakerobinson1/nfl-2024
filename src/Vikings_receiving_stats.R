library(tidyverse)
library(nflreadr)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(ggthemes)
library(dplyr)
library(nflplotR)

pbp <- load_pbp(2024)
ftn <- load_ftn_charting(2024)
ngs <- load_nextgen_stats(2024, "receiving")
#fastR <- load_pbp()

names(pbp)

pbp$univ_id <- paste(pbp$game_id, pbp$play_id, sep="_")
ftn$univ_id <- paste(ftn$nflverse_game_id, ftn$nflverse_play_id, sep="_")

vikings_receivers <- c(
  'J.Jefferson',
  'A.Jones',
  'J.Addison',
  'J.Oliver',
  'J.Nailor',
  'J.Mundt',
  'T.Hockenson'
)

mn_receivers <- pbp |> 
  filter(receiver_player_name %in% vikings_receivers, pass == 1, penalty == 0, week <= 11) |> 
  inner_join(ftn, by = c('univ_id' = 'univ_id'))

mn_receivers_table <- mn_receivers |> 
  group_by(receiver_player_name, receiver_player_id) |> 
  summarize(
            player_gsis = first(receiver_player_id),
            targets = n(),
            catches = sum(complete_pass == 1, na.rm = TRUE),
            catches_per = round((catches / targets) * 100, 1),
            catchable_targets = sum(is_catchable_ball == 1, na.rm = TRUE),
            ct_per = round((catchable_targets / targets) * 100, 1),
            catchable_catches = round((catches / catchable_targets) * 100, 1),
            cc_per = round((catches / catchable_targets) * 100, 1),
            completion_probability = round(mean(cp, na.rm = TRUE) * 100, 1),
            adot = round(mean(air_yards, na.rm = TRUE), 2),
            drops = sum(is_drop == 1, na.rm = TRUE)
            ) |> 
  ungroup() |> 
  arrange(-targets)

mn_receivers_table <- mn_receivers_table |> 
  mutate(
    custom_cc_string = paste0(catches, " / ", catchable_targets)
  )

mn_receivers_table |> 
  mutate(
    completion_probability = paste0(round(completion_probability, 1), "%")  # Adjust column name as needed
  ) |> 
  select(player_gsis, receiver_player_name, targets, catches, catches_per, catchable_targets, ct_per, cc_per, custom_cc_string, completion_probability, adot, drops) |> 
  gt() |> 
  tab_header(
    title = "Vikings Receiving Stats",
    subtitle = "2024 | Weeks 1-11 | 10+ Targets"
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_title()
  ) |> 
  gt_nfl_headshots("player_gsis") |> 
  cols_merge(
    columns = c(catches_per, catches),
    pattern = "<div style='text-align: center;'>
                 <span style='font-size: 14px; font-weight: bold;'>{1}%</span><br>
                 <span style='font-size: 12px; color: black;'>{2}</span>
               </div>"
  ) |> 
  cols_merge(
    columns = c(ct_per, catchable_targets),
    pattern = "<div style='text-align: center;'>
                 <span style='font-size: 14px; font-weight: bold;'>{1}%</span><br>
                 <span style='font-size: 12px; color: black;'>{2}</span>
               </div>"
  ) |> 
  cols_merge(
    columns = c(cc_per, custom_cc_string),
    pattern = "<div style='text-align: center;'>
                 <span style='font-size: 14px; font-weight: bold;'>{1}%</span><br>
                 <span style='font-size: 12px; color: black;'>{2}</span>
               </div>"
  ) |> 
  cols_label(
    receiver_player_name = "Player",
    targets = "Targets",
    catches = "Catch Stats"
  ) |> 
  fmt_number(
    columns = catches_per,
    decimals = 1
  ) |> 
  opt_all_caps(FALSE) |> 
  tab_options(table.font.size = px(14)) |> 
  cols_align(align = "center") |> 
  cols_label(player_gsis = " ",
             receiver_player_name = 'Player',
             targets = 'Targets',
             catches_per = 'Catches',
             catchable_targets = 'Catchable Targets',
             ct_per = 'Catchable Targets',
             cc_per = 'Catchable Catches',
             completion_probability = 'Completion Probability',
             adot = 'ADoT',
             drops = 'Drops') |> 
  tab_footnote(
    footnote = "Blake Robinson | @blakerbsn.bsky.social | nflverse | Inspired by Joseph Hefner."
  ) |> 
  gtExtras::gt_theme_espn()







