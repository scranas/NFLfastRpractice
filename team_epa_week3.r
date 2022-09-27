#script to show offensive and defensive epa for the 2022 season

library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(ggimage)
library(gt)
library(gtExtras)
library(nflfastR)

week = "3"
sub_title = "Through week 3, besides NYG and DAL"

options(scipen = 9999)

pbp <- load_pbp(2022)

pbp_rp <- pbp |> 
  filter(pass == 1 | rush == 1) |>
  filter(!is.na(epa))

off_efficiency <- pbp_rp |>
  filter(rush == 1 | pass == 1) |>
  group_by(posteam) |>
  summarize(plays = n(),
            off_epa = mean(epa))
def_efficiency <- pbp_rp |>
  filter(rush == 1 | pass == 1) |>
  group_by(defteam) |>
  summarize(plays = n(),
            def_epa = mean(epa))
#rename defteam to posteam for the join to work
names(def_efficiency)[1] <- "posteam"

total_efficiency <- off_efficiency |>
  left_join(def_efficiency, by = "posteam")

total_efficiency <- total_efficiency |>
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

total_efficiency |>
  ggplot(aes(x = off_epa, y = def_epa)) +
  scale_y_reverse() +
  geom_hline(yintercept = mean(total_efficiency$def_epa), linetype = "dashed", colour = "red") +
  geom_vline(xintercept = mean(total_efficiency$off_epa), linetype = "dashed", colour = "red") +
  #geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp= 16/9) +
  theme_bw() +
  labs(x = "Offensive EPA",
       y = "Defensive EPA",
       title = "Offensive and Defensive EPA in 2022",
       subtitle = sub_title ,
       caption = "by SebastianK | NFLFastR")
ggsave('off-def-epa-week3.png', width = 14, height = 10, dpi = "retina")