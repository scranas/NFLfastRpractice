source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(RColorBrewer)
library(ggimage)

this_week <- 6

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

pbp <- load_pbp(2022)
logos <- teams_colors_logos


hm <- load_schedules(2022) |>
  filter(week == this_week) |>
  select(home_team, away_team) |>
  rename("opponent" = "away_team")

hm2 <- rbind(hm, setNames(rev(hm), names(hm))) |>
  left_join(logos, by = c("home_team" = "team_abbr")) |>
  left_join(logos, by = c("opponent" = "team_abbr")) 


pbp_rp <- pbp |>
  filter(pass == 1 | rush == 1) |>
  filter(!is.na(epa))

dpass_efficiency <- pbp_rp |>
  filter(pass == 1) |>
  group_by(defteam) |>
  summarize(dpasses = n(),
            dpass_epa = -mean(epa)) #-mean(epa) bc smaller dpass is better

opass_efficiency <- pbp_rp |>
  filter(pass == 1) |>
  group_by(posteam) |>
  summarize(opasses = n(),
            opass_epa = mean(epa))

df <- hm2 |>
  left_join(dpass_efficiency, by = c("opponent" = "defteam")) |>
  left_join(opass_efficiency, by = c("home_team" = "posteam")) |>
  mutate(game = 1:n(),
         diff = opass_epa - dpass_epa)

p <- df |>
  ggplot(aes(x = reorder(game,diff), y = diff)) +
  geom_bar(aes(color = diff, fill = diff), stat = "identity", alpha = 0.9, show.legend = FALSE) +
  scale_fill_continuous_sequential(palette = "Viridis", trans = "reverse") +
  geom_image(aes(image = team_logo_espn.x, 
                 y = ifelse(diff > 0, diff + 0.03, diff - 0.03)), size = 0.03, asp= 16/9) +
  geom_image(aes(image = team_logo_espn.y,
                 y = ifelse(diff < 0, 0 + 0.03, 0 - 0.03)), image_fun = transparent, size = 0.03, asp= 16/9) +
  theme_bw() +
  labs(x = "",
       y = "Difference in OFF. EPA/Play and DEF EPA/Pass",
       title = "Difference in Offensive EPA/Pass and Defensive EPA/Pass in Week ", this_week,
       subtitle = "Bar above 0 has an offensive advantage, team logo below 0 has a passing disadvantage",
       caption = "by SebastianK | NFLFastR")

ggsave("passEpa_v_dpassEpa.png", height = 10, width = 14, dpi = "retina")
