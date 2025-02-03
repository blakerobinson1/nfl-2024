library(tidyverse)
library(nflreadr)
library(nflfastR)
library(ggimage)
library(gt)
library(gtExtras)
library(ggthemes)
library(dplyr)
library(nflplotR)
library(ggplot2)

pbp <- load_pbp(2024)
pbp23 <- load_pbp(2023)

# EPA/Play of coordinator's new team from last year, vs old unit from last year
# With league average

# New Hires:
# CHI: Ben Johnson (from DET)
# JAX: Liam Coen (from TB)
# NYJ: Aaron Glenn (from DET)
# PAT: Mike Vrabel (from TEN 2023)
# DAL: Brian Schottenheimer (from DAL)
# SEA: Pete Carroll (from SEA)
# NO: Kellen Moore (from PHI)

df <- data.frame(
  coach = c("Ben Johnson", "Aaron Glenn", "Liam Coen", "Brian Schottenheimer", "Kellen Moore"),   
  previous_team = c("DET", "DET", "TB", "DAL", "PHI"),
  new_team = c("CHI", "NYJ", "JAX", "DAL", "NO"),
  unit = c("OFF", "DEF", "OFF", "OFF", "OFF"),
  stringsAsFactors = FALSE  # To keep the strings as character type
)

df23 <- data.frame(
  coach = c("Mike Vrabel", "Pete Carroll"),
  previous_team = c("TEN", "SEA"),
  new_team = c("NE", "LV"),
  unit = c("DEF", "DEF"),
  stringsAsFactors = FALSE  # To keep the strings as character type
)

off_sums <- pbp |> 
  group_by(posteam) |> 
  summarize(
    off_epa_play = (sum(epa, na.rm = TRUE) / n())
  ) 

def_sums <- pbp |> 
  group_by(defteam) |> 
  summarize(
    def_epa_play = (sum(epa, na.rm = TRUE) / n())
  ) 

def_sums23 <- pbp23 |> 
  group_by(defteam) |> 
  summarize(
    def_epa_play = (sum(epa, na.rm = TRUE) / n())
  ) 

off_mean <- off_sums |> 
  summarize(
    mean_epa_play_off = mean(off_epa_play)
  )

def_mean <- def_sums |> 
  summarize(
    mean_epa_play_def = mean(def_epa_play)
  )

def_mean23 <- def_sums23 |> 
  summarize(
    mean_epa_play_def = mean(def_epa_play)
  )

# Join data for 2024 using off_sums (offensive data for 2024) and def_sums (defensive data for 2024)
df_24_joined <- df |> 
  left_join(off_sums, by = c("previous_team" = "posteam")) |> 
  rename(epa_play_off_prev = off_epa_play) |> 
  left_join(def_sums, by = c("previous_team" = "defteam")) |> 
  rename(epa_play_def_prev = def_epa_play) |> 
  left_join(off_sums, by = c("new_team" = "posteam")) |> 
  rename(epa_play_off_new = off_epa_play) |> 
  left_join(def_sums, by = c("new_team" = "defteam")) |> 
  rename(epa_play_def_new = def_epa_play) |> 
  mutate(
    # Combine the previous and new epa values
    epa_old = case_when(
      unit == "OFF" ~ epa_play_off_prev,
      unit == "DEF" ~ epa_play_def_prev,
      TRUE ~ NA_real_
    ),
    epa_new = case_when(
      unit == "OFF" ~ epa_play_off_new,
      unit == "DEF" ~ epa_play_def_new,
      TRUE ~ NA_real_
    )
  ) |> 
  select(-epa_play_off_prev, -epa_play_def_prev, -epa_play_off_new, -epa_play_def_new)

df_23_joined <- df23 |> 
  left_join(def_sums23, by = c("previous_team" = "defteam")) |> 
  rename(epa_play_def_prev = def_epa_play) |> 
  left_join(def_sums, by = c("new_team" = "defteam")) |> 
  rename(epa_play_def_new = def_epa_play) |> 
  mutate(
    # Combine the previous and new epa values
    epa_old = case_when(
      unit == "DEF" ~ epa_play_def_prev,
      TRUE ~ NA_real_
    ),
    epa_new = case_when(
      unit == "DEF" ~ epa_play_def_new,
      TRUE ~ NA_real_
    )
  ) |> 
  select(-epa_play_def_prev, -epa_play_def_new)

df_union <- rbind(df_24_joined, df_23_joined)

mean_epa_play_off <- 0.0103
mean_epa_play_def <- 0.0135

# Add the new column based on the unit column
df_union <- df_union |> 
  mutate(mean_epa = if_else(unit == "OFF", mean_epa_play_off, mean_epa_play_def))

df_union <- df_union |> 
  left_join(teams_colors_logos |> select(team_abbr, team_logo_wikipedia), by = c("new_team" = "team_abbr")) |> 
  rename(new_team_logo = team_logo_wikipedia)  

df_union <- df_union |> 
  left_join(teams_colors_logos |> select(team_abbr, team_logo_wikipedia), by = c("previous_team" = "team_abbr")) |> 
  rename(old_team_logo = team_logo_wikipedia)

off_df <- subset(df_union, unit == "OFF")

off_df$coach <- gsub(" ", "\n", off_df$coach)  # Replace spaces with new lines

def_df <- subset(df_union, unit == "DEF")

ggplot(off_df, aes(y = coach)) +
  geom_vline(xintercept = 0.0103, linetype = "dashed", color = "black", size = .5) +  # Dashed vertical line at avg EPA
  geom_segment(aes(x = epa_old, xend = epa_new), color = "black", size = 1.2) +  # The line connecting the points
  geom_image(aes(x = epa_old, image = old_team_logo), size = 0.11) +  # The logos for epa_old
  geom_image(aes(x = epa_new, image = new_team_logo), size = 0.11) +  # The logos for epa_new
  scale_x_continuous(
    limits = c(-0.075, 0.15),            # Adjust these limits to cover your data range
    breaks = seq(-0.075, 0.15, by = 0.025),  # Set breaks at every 0.25 increment
    labels = scales::label_number(accuracy = 0.001)  # Ensures normal decimal format
  ) +
  theme(axis.text.y = element_text(size = 5),  # Adjust text size for coach names
        plot.title = element_text(hjust = 5, size = 20),   # Center the title
        plot.subtitle = element_text(hjust = 10)) +  # Center the subtitle
  labs(title = "Offensive EPA per Play on New Coaches' Most Recent Offense vs New Team's Offense",
       subtitle = "Larger numbers are better. Dashed line represents league average offensive EPA per play",
       x = "EPA",
       y = "Coach",
       caption = "Blake Robinson | @blakerbsn | nflverse") +
  theme_fivethirtyeight() +
  theme(
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 50, 10, 10)  # Expand right-side margin
  )

ggplot(def_df, aes(y = coach)) +
  geom_vline(xintercept = 0.0103, linetype = "dashed", color = "black", size = .5) +  # Dashed vertical line at avg EPA
  geom_segment(aes(x = epa_old, xend = epa_new), color = "black", size = 1.2) +  # The line connecting the points
  geom_image(aes(x = epa_old, image = old_team_logo), size = 0.11) +  # The logos for epa_old
  geom_image(aes(x = epa_new, image = new_team_logo), size = 0.11) +  # The logos for epa_new
  scale_x_continuous(
    limits = c(-0.025, 0.075),            # Adjust these limits to cover your data range
    breaks = seq(-0.025, 0.075, by = 0.01),  # Set breaks at every 0.25 increment
    labels = scales::label_number(accuracy = 0.001)  # Ensures normal decimal format
  ) +
  theme(axis.text.y = element_text(size = 5),  # Adjust text size for coach names
        plot.title = element_text(hjust = 5, size = 20),   # Center the title
        plot.subtitle = element_text(hjust = 10)) +  # Center the subtitle
  labs(title = "Defensive EPA per Play on New Coaches' Most Recent Defense vs New Team's Defense",
       subtitle = "Smaller numbers are better. Dashed line represents league average offensive EPA per play",
       x = "EPA",
       y = "Coach",
       caption = "Blake Robinson | @blakerbsn | nflverse") +
  theme_fivethirtyeight() +
  theme(
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 50, 10, 10)  # Expand right-side margin
  )

