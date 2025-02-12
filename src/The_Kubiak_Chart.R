<<<<<<< HEAD
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(dplyr)

pbp21 <- load_pbp(2021)

pbp21$game_date <- as.Date(pbp21$game_date)

pbp22 <- load_pbp(2022)

pbp22$game_date <- as.Date(pbp22$game_date)

pbp22_filtered <- pbp22 |> 
  filter(game_date >= as.Date('2022-11-20'))

pbp24 <- load_pbp(2024)

pbp24$game_date <- as.Date(pbp24$game_date)


pbp_combined <- rbind(pbp21, pbp22_filtered, pbp24)

pbp_relevant <- pbp_combined[, c('season', 'week', 'game_date', 'posteam', 'epa')]

pbp_relevant$game_date <- as.Date(pbp_relevant$game_date)

pbp_relevant$month <- format(pbp_relevant$game_date, "%b")

pbp_relevant$year <- format(pbp_relevant$game_date, "%Y")

pbp_output <- pbp_relevant |> 
  filter((posteam == 'MIN' & season == 2021) | (posteam == 'DEN' & season == 2022) | (posteam == 'NO' & season == 2024)) |> 
  group_by(season, week, posteam, game_date, year, month) |> 
  summarize(
    total_epa = sum(epa, na.rm = TRUE)
  )

final <- pbp_output |> 
  mutate(
    year = as.numeric(year),
    month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  ) |> 
  group_by(year, month) |> 
  summarize(
    final_epa = sum(total_epa, na.rm = TRUE)
  ) |> 
  arrange(year, month)  

final$month <- factor(final$month, levels = month.abb)

final <- final |> 
  ungroup() |> 
  mutate(index = 1:nrow(final))

final$label <- paste(final$month, final$year)

final <- final |> 
  arrange(year, month)

ggplot(final, aes(x = index, y = final_epa)) +
geom_line() +
geom_point() +
scale_x_continuous(
  breaks = final$index,
  labels = final$label
) + 
scale_y_continuous(
  breaks = seq(-80, 80, by = 20),  
  labels = seq(-80, 80, by = 20)   
) +
geom_hline(yintercept = 0, linetype = "dashed") + 
labs(x = "Month",
     y = "Total EPA",
     title = "Total EPA by Month for Klint Kubiak-led Offenses",
     subtitle = "2021/22 with the Vikings, 2024 with the Saints",
     caption = "Blake Robinson | @blakerbsn.bsky.social | data: nflfastR"
) + 
theme_bw() +
theme(panel.grid.minor.x = element_blank()) 

  








=======
library(gt)

# Example data frame
df <- data.frame(
  ID = 1,
  Targets = 10,
  Catches = 7,
  Catch_Percentage = 0.7
)

# Create a GT table
gt_table <- df %>%
  gt() %>%
  cols_merge(
    columns = c(Catches, Catch_Percentage),
    # Use HTML for styling
    pattern = "<div style='text-align: center;'>
                 <span style='font-size: 16px; font-weight: bold;'>{1}</span><br>
                 <span style='font-size: 12px; color: gray;'>{2}%</span>
               </div>"
  ) %>%
  # Rename the merged column
  cols_label(
    Catches = "Catch Stats"
  ) %>%
  # Format the Catch Percentage
  fmt_number(
    columns = Catch_Percentage,
    decimals = 1
  ) %>%
  # Enable HTML rendering for styling
  opt_all_caps(FALSE) %>%
  tab_options(table.font.size = px(14))

# Display the table
gt_table
>>>>>>> 22fb904 (Creating Vikings receiving stats table + fixing Kubiak Chart location)
