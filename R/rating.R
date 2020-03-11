Sys.setlocale("LC_ALL", "C")


tst4 <-
  map(tst, function(x) {
    x %>%
      group_split(player$id, player$name) %>%
      map(~ mutate(.,
        complete_passes = get_pass_n(data = ., outcome = 'Complete'),
        incomplete_passes = get_pass_n(data = ., outcome = 'Incomplete'),
        out_passes = get_pass_n(data = ., outcome = 'Out'),
        defected_pass = get_pass_n(data = ., pass_type = 'deflected'),
        cross_pass = get_pass_n(data = ., pass_type = 'cross'),
        switch_pass = get_pass_n(data = ., pass_type = 'switch'),
        shot_assist = get_pass_n(data = ., pass_type = 'shot_assist'),
        cut_back = get_pass_n(data = ., pass_type = 'cut_back'),
        under_pressure = get_under_pressure_n(data = .),
        yellow_card = get_cards_n(data = ., card_type = "Yellow Card"),
        red_card = get_cards_n(data = ., card_type = "Red Card"),
        ball_receipts = get_ball_receipt_n(data = .),
        ball_recovery = get_ball_recovery_n(data = .),
        blocks = get_block_n(data = .),
        air_clearance = get_clearance_aerial_n(data = .),
        clearance = get_clearance_n(data = .),
        dribble_complete = get_dribble_n(data = ., outcome = "Complete"),
        dribble_incomplete = get_dribble_n(data = ., outcome = "Incomplete"),
        duel_won = get_duel_n(data = ., outcome = "Won"),
        duel_lost_play = get_duel_n(data = ., outcome = "Lost In Play"),
        duel_lost_out = get_duel_n(data = ., outcome = "Lost Out"),
        duel_success_play = get_duel_n(data = ., outcome = "Success In Play"),
        duel_success_out = get_duel_n(data = ., outcome = "Success Out"),
        #foul_6_seconds = get_foul_commited_n(data = ., type = "6 Seconds"),
        foul_backpass_pick = get_foul_commited_n(data = ., type = "Backpass Pick"),
        foul_dangerous_play = get_foul_commited_n(data = ., type = "Dangerous Play"),
        foul_dive = get_foul_commited_n(data = ., type = "Dive"),
        foul_out = get_foul_commited_n(data = ., type = "Foul Out"),
        foul_handball = get_foul_commited_n(data = ., type = "Handball"),
        foul_won_defensive = get_foul_won_n(data = ., type = "defensive"),
        foul_won_advantage = get_foul_won_n(data = ., type = "advantage"),
        foul_won_penalty = get_foul_won_n(data = ., type = "penalty"),
        goalkeeper_saves = get_goalkeeper_saves_n(data = .),
        interception_lost_play = get_interception_n(data = ., outcome = "Lost In Play"),
        interception_lost_out = get_interception_n(data = ., outcome = "Lost Out"),
        interception_success_play = get_interception_n(data = ., outcome = "Success In Play"),
        interception_success_out = get_interception_n(data = ., outcome = "Success Out"),
        interception_won = get_interception_n(data = ., outcome = "Won")
      ) %>%
        distinct(
          player = `player$id`,
          player_name = `player$name`,
          complete_passes,
          incomplete_passes,
          out_passes,
          defected_pass,
          cross_pass,
          switch_pass,
          shot_assist,
          cut_back,
          under_pressure,
          yellow_card,
          red_card,
          ball_receipts,
          ball_recovery,
          blocks,
          air_clearance,
          clearance,
          dribble_complete,
          dribble_incomplete,
          duel_won,
          duel_lost_play,
          duel_lost_out,
          duel_success_play,
          duel_success_out,
          #foul_6_seconds,
          foul_backpass_pick,
          foul_dangerous_play,
          foul_dive,
          foul_out,
          foul_handball,
          foul_won_defensive,
          foul_won_advantage,
          foul_won_penalty,
          goalkeeper_saves,
          interception_lost_play,
          interception_lost_out,
          interception_success_play,
          interception_success_out,
          interception_won
        ))
  }) %>%
  map(., bind_rows) %>%
  map(., ~ filter(., !is.na(player)))




for (i in coefs$term[1:length(coefs$term)]) {
  tst4 %<>% map(., function(x) x %>% mutate(!!i := x[[i]] * coefs[which(coefs$term == i), ]$estimate))
}


tst5 <- 
  map(tst4, ~ mutate(., r_match = apply(.[!names(.) %in% c("player", "player_name")], 1, sum))) %>% 
  bind_rows(.id = 'match') %>% 
  mutate(match = as.integer(match)) %>% 
  #filter(match %in% matches[matches$season %in% c("2017/2018","2018/2019"),]$match_id) %>% 
  left_join(distinct(matches, match_id, match_date, season), by = c('match' = 'match_id'))


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 2 * IQR(x) | x > quantile(x, 0.75) + .5 * IQR(x))
}


tst6 <- 
  tst5 %>% 
  #group_by(match) %>% 
  mutate(r_norm = normalize(r_match) * 100) #%>% 
  #ungroup()


tst7 <-
  tst6 %>% 
  group_by(player, player_name) %>% 
  mutate(n = n()) %>%
  filter(n > 1) %>% 
  summarise(r_norm_med = weighted.mean(r_norm, w = n)) %>% 
  ungroup()



players_position <-
  map_df(tst, ~tibble(player = .$player$id, position = .$position$name)) %>% 
  group_by(player,position) %>% 
  summarise(n = n()) %>%
  group_by(player) %>% 
  filter(n == max(n) & !is.na(position)) %>% 
  select(player, position)


tst8 <-
  tst7 %>% 
  left_join(players_position, by = 'player') %>% 
  mutate(position_custom = case_when(
    position %in% c('Goalkeeper','Substitute') ~ 'Goalkeeper',
    position %in% c('Left Back', 'Left Wing Back') ~ 'Left Back',
    position %in% c('Left Center Back', 'Center Back', 'Right Center Back') ~ 'Center Back',
    position %in% c('Right Back', 'Right Wing Back') ~ 'Right Back',
    position %in% c('Left Defensive Midfield', 'Center Defensive Midfield', 'Right Defensive Midfield') ~ 'Defensive Midfield',
    position %in% c('Right Center Midfield', 'Center Midfield', 'Left Center Midfield') ~ 'Midfield',
    position %in% c('Left Midfield') ~ 'Left Midfield/Wing',
    position %in% c('Right Midfield') ~ 'Right Midfield/Wing',
    position %in% c('Left Wing') ~ 'Left Midfield/Wing',
    position %in% c('Right Wing') ~ 'Right Midfield/Wing',
    position %in% c('Right Attacking Midfield','Center Attacking Midfield','Left Attacking Midfield') ~ 'Attacking Midfield',
    position %in% c('Striker', 'Center Forward','Right Center Forward','Left Center Forward','Secondary Striker') ~ 'Forward',
    TRUE ~ NA_character_
  )) %>% 
  group_by(position_custom) %>% 
  mutate(outlier = ifelse(is_outlier(r_norm_med), str_trunc(player_name, width = 15), NA)) %>% 
  ungroup()
  



ggplot(tst8, aes(x=r_norm_med)) +
  geom_histogram() +
  theme_minimal() +
  facet_wrap(~position_custom)



ggplot(tst8, aes(x=position_custom, y=r_norm_med)) +
  #geom_boxplot() +
  geom_violin(fill='lightgrey') +
  geom_jitter(shape=16, position=position_jitter(0.1), color = 'black') +
  ggrepel::geom_text_repel(aes(label = outlier), na.rm = TRUE, hjust = -0.05, ) +
  theme_minimal() +
  labs(x = 'Position', y = 'Player rating (0-100)', title = 'Player perfomance rating') + 
  ggsave(filename = here('analyses', 'plots', 'perfomance_rating.png'), width = 18, height = 8)


ggplot(tst8, aes(x = r_norm_med, y = position_custom)) +
  ggridges::geom_density_ridges(alpha = 0.5) +
  theme_minimal()  +
  labs(x = 'Player rating (0-100)', y = 'Position', title = 'Player perfomance rating') +
  ggsave(filename = here('analyses', 'plots', 'ridges_perfomance_rating.png'), width = 14, height = 10)



top_players <- 
  tst8 %>% 
  group_by(position_custom) %>% 
  top_n(n = 1, wt = r_norm_med) %>% 
  pull(player)
  

tst6 %>% 
  left_join(tst8[c('player','position_custom')], by = 'player') %>% 
  filter(player %in% top_players) %>% 
  ggplot(aes(x = match_date, y = r_norm, group=player_name)) +
  geom_line(aes(color=player_name)) +
  #geom_point(aes(color=player_name)) +
  ylim(0,100) +
  theme_minimal() +
  facet_wrap(season~position_custom,  scales = "free_x") +
  labs(color = 'Player name', title = 'Player rating progress', subtitle = 'By position and season') +
  theme(legend.position="bottom", strip.text.x = element_text(face = 'bold')) +
  viridis::scale_color_viridis(discrete = TRUE) + 
  ggsave(filename = here('analyses', 'plots', 'rating_season_position.png'), width = 14, height = 10)





tst6 %>%
  left_join(tst8[c('player','position_custom')], by = 'player') %>% 
  filter(player %in% top_players  & position_custom != 'Goalkeeper') %>% 
  group_by(player) %>% 
  arrange(match_date) %>% 
  mutate(gap = cumsum(c(0, diff(match_date) > 90))) %>% 
  ungroup() %>% 
  ggplot(aes(x = match_date, y = r_norm, group = gap)) +
  geom_line(aes(color=player_name), size = 0.7) +
  #geom_point(aes(color=player_name)) +
  ylim(0,100) +
  scale_x_date(date_labels = "%b-%y") +
  theme_minimal() +
  facet_wrap(~position_custom,  scales = "free_x") +
  labs(color = 'Player name', 
       title = 'Top players performance rating',
       subtitle = expression('By position and season' ~bold('2018/2019 - 2019/2020')~''), 
       x = 'Date', y = 'Rating (0-100)') +
  theme(strip.text.x = element_text(face = 'italic'), legend.position = "bottom") +
  viridis::scale_color_viridis(discrete = TRUE) +
  ggsave(filename = here('analyses', 'plots', 'rating_season_position.png'), width = 14, height = 10)

