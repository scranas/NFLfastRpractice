# linear regression for expected air yards over expectations

library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(ggimage)
library(gt)
library(gtExtras)
library(nflfastR)

options(scipen = 9999)


pbp <- load_pbp(2022)

# filtering dtaset down to pass plays

pass_plays <- pbp |>
  filter(pass == 1) |>
  filter(!is.na(air_yards), !is.na(down), !is.na(score_differential),
          !is.na(ydstogo), !is.na(half_seconds_remaining))|>
  mutate(down = as.factor(down))

pass_play_model_data <- pass_plays |>
  select(air_yards, down, score_differential, ydstogo, half_seconds_remaining) |>
  mutate(down = as.factor(down)) # down shouldnt be seen as a nummerical value in context

str(pass_play_model_data)
colSums(is.na(pass_play_model_data))

air_yards_lm <- lm(air_yards ~ down + score_differential + ydstogo + half_seconds_remaining,
                   data = pass_play_model_data) #air_yards as dependent variable

summary(air_yards_lm) 
#r^2 at 0.01, not really predicting much. not surprising considering only few variables nad few observations

vip(air_yards_lm, num_features = 7)

air_yard_preds <- data.frame(predict.lm(air_yards_lm, newdata = pass_plays)) |>
  rename(exp_air_yards = predict.lm.air_yards_lm..newdata...pass_plays.)

air_yard_projs <- cbind(pass_plays, air_yard_preds)

air_yards_projs <- air_yard_projs |>
  mutate(ayoe = air_yards - exp_air_yards) |>
  group_by(passer) |>
  summarise(passes = n(),
            avg_ayoe = mean(ayoe)) |>
  filter(passes >= 20)

