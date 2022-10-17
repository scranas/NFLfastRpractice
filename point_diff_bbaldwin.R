library(nflplotR)
sched <- nflreadr::load_schedules(2022)


h <- sched %>%
  select(game_id, team = home_team, result)

a <- sched %>%
  mutate(result = -result) %>%
  select(game_id, team = away_team, result)

fig <- bind_rows(h, a) %>%
  arrange(game_id) %>%
  group_by(team) %>%
  summarise(diff = sum(result, na.rm = T)) %>%
  ungroup() %>%
  left_join(nflreadr::load_teams(), by = c("team" = "team_abbr")) %>%
  arrange(-diff) %>%
  ggplot(aes(reorder(team, -diff), diff)) +
  geom_col(aes(color = team, fill = team), width = 0.75) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(alpha = 0.4) +
  ggthemes::theme_fivethirtyeight() +
  labs(x = "Week",
       y = "Point Differential",
       caption = "@benbbaldwin",
       title = glue::glue("Point Differential Through Week 5")) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, face="bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_nfl_logos(aes(team_abbr = team, alpha = .9), width = 0.045)