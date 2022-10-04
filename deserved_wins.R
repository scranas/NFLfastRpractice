# net success rate in non-garbage time
# chart that displays net win % , winner on top
# identify winning 
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(ggimage)
library(gt)
library(gtExtras)
library(nflfastR)

options(scipen = 9999)
this_week <- 4

pbp <- load_pbp(2022)  |>
  filter(week == this_week, !is.na(posteam), !is.na(success))

logos <- teams_colors_logos |>
  select(team_abbr, team_color, team_logo_wikipedia)
# #checking if field goal has epa
# pbp |>
#   select(field_goal_attempt, epa, game_id, success) |>
#   filter(field_goal_attempt == 1)
# 
# pbp |>
#   select(epa, success, extra_point_attempt) |>
#   filter(extra_point_attempt == 1)

net_wp <- pbp |>
  select(game_id, posteam, home_wp, away_wp, wp, success)|>
  filter(!wp > 0.95, !wp< 0.05) |>
  group_by(posteam) |>
  mutate(suc_plays = as.numeric((success == 1))) |>
  summarize( plays = n(),
             succ_plays = sum(suc_plays),
             success_rate = sum(succ_plays/plays))
             # net = success_rate)|>
  
game_stats <- pbp |>
  left_join(net_wp, by = c("posteam" = "posteam")) |>
  distinct(posteam, .keep_all = TRUE) |>
  select(game_id, success_rate, posteam, home_team, away_team, home_score, away_score)
  

check <- game_stats |>
  filter(posteam == home_team) |>
  rename("home_rate" = "success_rate") |>
  select(game_id, home_rate, home_team, home_score)

check2 <- game_stats |>
  filter(posteam == away_team) |>
  rename("away_rate" = "success_rate")|>
  select(game_id, away_rate, away_team, away_score) |>
  left_join(check, by = c("game_id" = "game_id")) |>
  mutate(net_rate = home_rate - away_rate,
         final_score = home_score - away_score,
         winner = final_score > 0) |> 
  arrange(-winner, net_rate) |> ## issue: rate is off b/c of home/away. see ifelse statement below for fix 
  left_join(logos, by = c("away_team" = "team_abbr")) |>
  left_join(logos, by = c("home_team" = "team_abbr")) |>
  rename("logo_home" = "team_logo_wikipedia.x",
         "logo_away" = "team_logo_wikipedia.y",
         "color_home" = "team_color.x",
         "color_away" = "team_color.y") |>
  arrange(net_rate)

test <-
  ifelse(check2$home_score > check2$away_score, (check2$net_rate = check2$home_rate - check2$away_rate),
    (check2$net_rate = check2$away_rate - check2$home_rate))

check2$calc <- test 

#fill color should be winning team, use another ifelse statement to create a winner column for logo

check2$loser_logo <- 
  ifelse(check2$winner == TRUE, check2$logo_home, check2$logo_away)
check2$winner_logo <-
  ifelse(check2$winner == FALSE, check2$logo_home, check2$logo_away)

check2$fill_color <-
  ifelse(check2$winner == FALSE, check2$color_home, check2$color_away)

check2 |>
  ggplot(aes(x = reorder(game_id,calc), y = calc)) +
  geom_bar(aes(fill = fill_color, color = fill_color), stat = "identity", alpha = 0.9) +
  scale_color_identity(aesthetic = c("fill", "color")) +
  geom_image(aes(image = winner_logo, 
                 y = ifelse(calc > 0, calc + 0.01, calc - 0.01)), size = 0.05, asp= 16/9) +
  geom_image(aes(image = loser_logo, y = 0), size = 0.05, asp= 16/9) +
  scale_y_continuous(breaks = seq(-0.1, 0.2, by = 0.05)) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  theme_bw() +
  labs(x = "Games",
       y = "Net Success Rate",
       title = "Did we deserve that win?",
       subtitle = "Week 4 Net Succes Rates in non-garbage time (win prob > 5% & <95%)",
       caption = "by SebastianK | NFLFastR")
ggsave('deserved_wins_week4.png', width = 14, height = 10, dpi = "retina")
