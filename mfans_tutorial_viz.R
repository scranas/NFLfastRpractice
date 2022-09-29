library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(ggimage)
library(gt)
library(gtExtras)
library(nflfastR)
library(ggrepel)
library(ggthemes)

obo <- load_pbp(2022)
#getting qb epa play, filtering down only pass or rush plays, filtering empty epa rows,
# grouping by player/id, getting mean epa, plays they were in for, and
# getting combining paff plays into pass attempts
# finally adding a column for pass rate to see how many pass plays teams run

# issue? : when na.rm = TRUE is removed, 6 qbs drop out of the dataset
# shouldnt happen, takes out sacks that dont count/ have na in its column

qb_epa_play <- obo |>
  filter(pass == 1 | rush == 1) |>
  filter(!is.na(epa)) |>
  group_by(id) |>
  summarize(name = last(name),
            team = last(posteam),
            plays = n(),
            epa_play = mean(epa),
            pass_attempts = sum(incomplete_pass + complete_pass, na.rm = TRUE)) |> #any na remove when aggregating
  filter(plays >= 50, pass_attempts >= 10) |>
  mutate(pass_rate = pass_attempts/plays) |>
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) 
  
# plot

qb_epa_play |>
  ggplot(aes(x = pass_rate, y = epa_play)) + 
  geom_point(aes(fill = team_color, color = team_color2, size = plays),
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = name)) +
  theme_bw() +
  geom_hline(yintercept = mean(qb_epa_play$epa_play), linetype = "dashed") +
  geom_vline(xintercept = mean(qb_epa_play$pass_rate), linetype = "dashed") +
  labs(x = "Pass Rate",
       y = "EPA/Play",
       title = "EPA/Play and Pass Rate, 2022",
       subtitle = "Through Week 3, minimum 50 plays and 20 pass attempts",
       caption = " by SebastianK | Data by NFLFastR") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 16, hjust = 0.5))
ggsave('epa-pass-rate.png', width = 14, height = 10, dpi = "retina")

#bar chart

qb_epa_play |>
  ggplot(aes(x = epa_play, y = fct_reorder(name, epa_play))) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.9) +
  geom_vline(aes(xintercept = mean(epa_play))) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_bw() +
  geom_image(aes(image = team_logo_espn,
                 x = ifelse(epa_play > 0, epa_play + 0.01, epa_play - 0.01)),
             asp = 4/3, size = 0.035) +
  labs(x = "EPA/Play",
      y = "Quarterback",
      title = "Each QBs EPA/Play 2022",
      caption = "by SebastianK | Data by NFLFastR") +
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5)) 
ggsave('bar-epa.png', width = 14, height = 10, dpi = "retina")

#table  

qb_gt <- qb_epa_play |>
  arrange(-epa_play) |>
  mutate(rank = row_number()) |>
  dplyr::select(rank, name, team_wordmark, pass_attempts, plays, pass_rate, epa_play) |>
  mutate(pass_rate = 100*round(pass_rate, 3),
         epa_play = round(epa_play,2)) |>
  gt() |>
  cols_align(align = "center") |>
  gtExtras::gt_img_rows(team_wordmark) |>
  cols_label(rank = "Rank",
             name = "Quarterback",
             team_wordmark = "",
             pass_attempts = "Pass Attempts",
             pass_rate = "Pass Rate",
             epa_play = "EPA per Play") |>
  gtExtras::gt_theme_espn() |>
  gtExtras::gt_hulk_col_numeric(epa_play)
gtsave(qb_gt, "qb_gt.png")
  
