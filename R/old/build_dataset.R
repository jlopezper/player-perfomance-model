all_leagues <- c('11','37','43','49','72')
matches <- 
  map(c('37'),
      function(i) {
        files <- list.files(here("data", "matches", i), pattern = ".json") # get file names
        matches <-
          map_df(
            here("data", "matches", i, files),
            ~ read_json(., simplifyVector = TRUE) %>%
              mutate(
                home_team = .$home_team$home_team_id,
                away_team = .$away_team$away_team_id,
                season = .$season$season_name,
                match_date = lubridate::as_date(.$match_date)
              ) %>%
              select(match_id, match_date, home_score, away_score, home_team, away_team, season) %>%
              as_tibble() 
          )}) %>% 
  bind_rows()



files <- list.files(here("data", "events"), pattern = ".json") # get file names
files <- files[sub(pattern = "\\.json$", "", files) %in% matches$match_id]
tst <-
  map(here("data", "events", files), ~ read_json(., simplifyVector = TRUE)) %>%
  set_names(gsub(pattern = "\\.json$", "", files))



matches %<>%
  pivot_longer(c("home_team", "away_team"), names_to = "team_type", values_to = "team_id") %>%
  mutate(result = case_when(
    team_type == "home_team" & home_score > away_score ~ 1,
    team_type == "away_team" & home_score < away_score ~ 1,
    TRUE ~ 0
  ))




# Passes
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  complete_passes = get_pass_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Complete'),
                  incomplete_passes = get_pass_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Incomplete'),
                  out_passes = get_pass_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Out'),
                  defected_pass = get_pass_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'deflected'),
                  cross_pass = get_pass_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'cross'),
                  switch_pass = get_pass_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'switch')
  ))


# Cut back
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  cut_back = get_pass_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'cut_back')
  ))


# Under pressure events
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  under_pressure = get_under_pressure_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))

# Assist
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  shot_assist = get_shot_assist_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id)),
                  goal_assist = get_goal_assist_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))


# Cards
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  yellow_card = get_cards_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = "Yellow Card"),
                  red_card = get_cards_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = "Red Card")
  ))


# Ball receipts
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  ball_receipts = get_ball_receipt_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))


# Ball recovery
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  ball_recovery = get_ball_recovery_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))


# Blocks
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  blocks = get_block_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))


# Air clearance
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  air_clearance = get_clearance_aerial_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))


# Clearance
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  clearance = get_clearance_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))



# Dribble
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  dribble_complete = get_dribble_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Complete'),
                  dribble_incomplete = get_dribble_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Incomplete'),
  ))


# Duel
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  duel_won = get_duel_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Won'),
                  duel_lost_play = get_duel_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Lost In Play'),
                  duel_lost_out = get_duel_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Lost Out'),
                  duel_success_play = get_duel_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Success In Play'),
                  duel_success_out = get_duel_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Success Out')
  ))


# Fouls committed
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  foul_6_seconds = get_foul_commited_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = '6 Seconds'),
                  foul_backpass_pick = get_foul_commited_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'Backpass Pick'),
                  foul_dangerous_play = get_foul_commited_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'Dangerous Play'),
                  foul_dive = get_foul_commited_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'Dive'),
                  foul_out = get_foul_commited_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'Foul Out'),
                  foul_handball = get_foul_commited_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'Handball')
  ))


# Foul won
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  foul_won_defensive = get_foul_won_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'defensive'),
                  foul_won_advantage = get_foul_won_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'advantage'),
                  foul_won_penalty = get_foul_won_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), type = 'penalty'),
  ))


# Goalkepper
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  goalkeeper_saves = get_goalkeeper_saves_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id))
  ))



# Interceptions
matches %<>%
  group_split(match_id, team_id) %>%
  map_df(~ mutate(.,
                  interception_lost_play = get_interception_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Lost In Play'),
                  interception_lost_out = get_interception_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Lost Out'),
                  interception_success_play = get_interception_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Success In Play'),
                  interception_success_out = get_interception_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Success Out'),
                  interception_won = get_interception_n(data = filter(tst[[as.character(.$match_id)]], team$id == .$team_id), outcome = 'Won')
  ))


