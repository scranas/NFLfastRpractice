# similar graph AND process as target air yards distributions gri
# but for running backs
# where we see the distribution of usage depending on win percentage
# i.e which RBs see more usage in gargabe time/when the game is on the line

# need to figure

source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(RColorBrewer)

pbp <- load_pbp(2022) |>
  decode_player_ids() |>
  filter(!week == 6) |>
  mutate(wp = wp*100) |>
  #filter(is.na(passer_id)) |>
  filter(!is.na(rusher_id) | !is.na(receiver_id) , !is.na(rush_attempt) , !is.na(pass_attempt)) |>
  mutate(player_id = ifelse(is.na(rusher_id), receiver_id, rusher_id))#create column for player_id instead of having rush and rec sepparately 

pbp$wp <- as.numeric(pbp$wp)

roster_df <- fast_scraper_roster(2022) |>
  select(-week)

# bit convulted but essentially create opportunites column, create team ranking for opps, 
# join add. player data nd filter filter out QB from subset data 
# player_df will later be joined on pbp for the plot, usage is to get team rank

player_df <- pbp |> 
  group_by(season, posteam, player_id) |>
  mutate(opportunities = sum(rush_attempt) + sum(pass_attempt))|>
  arrange(-opportunities) |>
  summarise(n = n(), opportunities = opportunities) |>
  mutate(team_opp_rank = row_number()) |>
  filter(opportunities >= 10) |>
  arrange(posteam, -opportunities, player_id) |>
  distinct(player_id, .keep_all = TRUE) |>
  #select(name, posteam, opportunities) |>
  ungroup() |>
  left_join(roster_df, by = c('season', 'player_id' = 'gsis_id')) |>
  filter(position == "RB") |> 
  mutate(jersey_number = NULL) |>
  group_by(posteam) |>
  mutate(team_opp_rank = row_number()) |>
  ungroup()

tm_grob_df <- data.frame(posteam = .tm_div_order, team_opp_rank = 2, full_name = NA, headshot_url = wordmark_url(.tm_div_order), wp = 50, vp.height = 1, alpha = 0.4)

grob_df <- player_df %>% 
  select(posteam, team_opp_rank, full_name, headshot_url) %>% 
  mutate(wp = 50, vp.height = 0.4, alpha = 0, team_opp_rank = team_opp_rank) %>% 
  mutate(posteam = factor(posteam, .tm_div_order))

my_week <- 5

pt <- pbp %>% 
  right_join(player_df) %>%
  filter(position == "RB") |>
  mutate(posteam = factor(posteam, .tm_div_order)) %>% 
  ggplot(aes(group = team_opp_rank, x = wp, y = team_opp_rank, color = "black", fill = stat(x))) +
  facet_wrap(.~posteam, nrow = 4, scales = 'free_x') +
  #geom_image(data = tm_grob_df, aes(image = headshot_url), size = 0.7, asp = 1.375) +
  #geom_image(data = tm_grob_df, aes(image = headshot_url), size = 0.7, color = 'white', alpha = 0.8, asp = 1.375) +
  geom_vline(xintercept = seq(0,100,25), color = 'grey85', size = 0.3) +
  #geom_grob(data = grob_df, aes(x = air_yards, y = team_air_rank - 0.4, label = grob, vp.height = vp.height)) +
  #geom_hline(data = grob_df, aes(yintercept = ifelse(is.na(full_name), NA, team_opp_rank), color = posteam), size = 0.6, show.legend = F) + #bottom line und player distributionfor better visibility
  geom_density_ridges_gradient(scale = 2, bandwidth = 8, panel_scaling = F, show.legend = F, size = 0.4) +
  #ylim(1,4) +
  geom_shadowtext(data = grob_df, aes(label = full_name, x = wp - 20 , y = team_opp_rank - 0.5), color = 'darkblue', bg.color = 'grey95', family = font_SB, bg.r = 0.2, hjust = 1, size = 1.5) + #name labels
  scale_y_reverse(expand = expansion(add = c(0.5,0.5))) +
  scale_x_continuous(limits = c(0,100), expand = expansion(mult = 0)) +
  scale_color_manual(values = "black") +
  scale_fill_viridis_c(guide = "none") +
  labs(title = '2022 Rb Opportunies distribution by win percentage',
       subtitle = paste0('Opportunities for RB on each team through Week ', my_week, ', >= 10 opportunities'),
       x = NULL,
       y = NULL) +
  theme_classic() +
  theme(panel.background = element_blank(),
        legend.position = "none") 


ggsave('rushing_distribution_by_wp.png', width = 14, height = 10, dpi = "retina")

