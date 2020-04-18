
matches_copy <- matches[1:100, ]




matches_copy <- test(matches_copy, events, "complete_passes", func = "get_pass_n", outcome = "Complete")
matches_copy <- test(matches_copy, events, "incomplete_passes", func = "get_pass_n", outcome = "Incomplete")
matches_copy <- test(matches_copy, events, "out_passes", func = "get_pass_n", outcome = "Out")
matches_copy <- test(matches_copy, events, "defected_pass", func = "get_pass_n", type = "deflected")
matches_copy <- test(matches_copy, events, "cross_pass", func = "get_pass_n", type = "cross")
matches_copy <- test(matches_copy, events, "switch_pass", func = "get_pass_n", type = "switch")
matches_copy <- test(matches_copy, events, "cut_back", func = "get_pass_n", type = "cut_back")
matches_copy <- test(matches_copy, events, "under_pressure", func = "get_under_pressure_n")
matches_copy <- test(matches_copy, events, "shot_assist", func = "get_shot_assist_n")
matches_copy <- test(matches_copy, events, "goal_assist", func = "get_goal_assist_n")
matches_copy <- test(matches_copy, events, "yellow_card", func = "get_cards_n", type = "Yellow Card")
matches_copy <- test(matches_copy, events, "red_card", func = "get_cards_n", type = "Red Card")
matches_copy <- test(matches_copy, events, "ball_receipts", func = "get_ball_receipt_n")
matches_copy <- test(matches_copy, events, "ball_recovery", func = "get_ball_recovery_n")
matches_copy <- test(matches_copy, events, "blocks", func = "get_block_n")
matches_copy <- test(matches_copy, events, "air_clearance", func = "get_clearance_aerial_n")
matches_copy <- test(matches_copy, events, "clearance", func = "get_clearance_n")
matches_copy <- test(matches_copy, events, "dribble_complete", func = "get_dribble_n", outcome = "Complete")
matches_copy <- test(matches_copy, events, "dribble_incomplete", func = "get_dribble_n", outcome = "Incomplete")
matches_copy <- test(matches_copy, events, "duel_won", func = "get_duel_n", outcome = "Won")
matches_copy <- test(matches_copy, events, "duel_lost_play", func = "get_duel_n", outcome = "Lost In Play")
matches_copy <- test(matches_copy, events, "duel_lost_out", func = "get_duel_n", outcome = "Lost Out")
matches_copy <- test(matches_copy, events, "duel_success_play", func = "get_duel_n", outcome = "Success In Play")
matches_copy <- test(matches_copy, events, "duel_success_out", func = "get_duel_n", outcome = "Success Out")
matches_copy <- test(matches_copy, events, "foul_6_seconds", func = "get_foul_commited_n", type = "6 Seconds")
matches_copy <- test(matches_copy, events, "foul_backpass_pick", func = "get_foul_commited_n", type = "Backpass Pick")
matches_copy <- test(matches_copy, events, "foul_dangerous_play", func = "get_foul_commited_n", type = "Dangerous Play")
matches_copy <- test(matches_copy, events, "foul_dive", func = "get_foul_commited_n", type = "Dive")
matches_copy <- test(matches_copy, events, "foul_out", func = "get_foul_commited_n", type = "Foul Out")
matches_copy <- test(matches_copy, events, "foul_handball", func = "get_foul_commited_n", type = "Handball")
matches_copy <- test(matches_copy, events, "foul_won_defensive", func = "get_foul_won_n", type = "defensive")
matches_copy <- test(matches_copy, events, "foul_won_advantage", func = "get_foul_won_n", type = "advantage")
matches_copy <- test(matches_copy, events, "foul_won_penalty", func = "get_foul_won_n", type = "penalty")
matches_copy <- test(matches_copy, events, "goalkeeper_saves", func = "get_goalkeeper_saves_n")
matches_copy <- test(matches_copy, events, "interception_lost_play", func = "get_interception_n", outcome = "Lost In Play")
matches_copy <- test(matches_copy, events, "interception_lost_out", func = "get_interception_n", outcome = "Lost Out")
matches_copy <- test(matches_copy, events, "interception_success_play", func = "get_interception_n", outcome = "Success In Play")
matches_copy <- test(matches_copy, events, "interception_success_out", func = "get_interception_n", outcome = "Success Out")
matches_copy <- test(matches_copy, events, "interception_won", func = "get_interception_n", outcome = "Won")


type <- c(
  rep(NA_character_, 3), "deflected", "cross", "switch", "cut_back", rep(NA_character_, 3), "Yellow Card",
  "Red Card", rep(NA_character_, 12), "6 Seconds", "Backpass Pick", "Dangerous Play", "Dive", "Foul Out",
  "Handball", "defensive", "advantage", "penalty", rep(NA_character_, 6)
)

variables <- c(
  "complete_passes", "incomplete_passes", "out_passes", "defected_pass", "cross_pass","switch_pass", "cut_back",
  "under_pressure", "shot_assist", "goal_assist", "yellow_card", "red_card", "ball_receipts", "ball_recovery",
  "blocks", "air_clearance", "clearance", "dribble_complete", "dribble_incomplete", "duel_won", "duel_lost_play",
  "duel_lost_out", "duel_success_play", "duel_success_out", "foul_6_seconds", "foul_backpass_pick", "foul_dangerous_play", 
  "foul_dive", "foul_out", "foul_handball", "foul_won_defensive", "foul_won_advantage", "foul_won_penalty", 
  "goalkeeper_saves", "interception_lost_play", "interception_lost_out", "interception_success_play", 
  "interception_success_out", "interception_won"
)

func <- c(
  rep("get_pass_n", 7),
  "get_under_pressure_n", "get_shot_assist_n", "get_goal_assist_n", rep("get_cards_n", 2),
  "get_ball_receipt_n", "get_ball_recovery_n", "get_block_n", "get_clearance_aerial_n", "get_clearance_n",
  rep("get_dribble_n", 2), rep("get_duel_n", 5),
  rep("get_foul_commited_n", 6), rep("get_foul_won_n", 3), "get_goalkeeper_saves_n",
  rep("get_interception_n", 5)
)

outcome <- c(
  "Complete", "Incomplete", "Out", rep(NA_character_, 14), "Complete", "Incomplete",
  "Won", "Lost In Play", "Lost Out", "Success In Play", "Success Out", rep(NA_character_, 10),
  "Lost In Play", "Lost Out", "Success In Play", "Success Out", "Won"
)



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 2 * IQR(x) | x > quantile(x, 0.75) + .5 * IQR(x))
}




create_model_variables <-
  function(matches, events, variable, func, outcome = NA, type = NA) {
    
    create_vars <- 
      function(matches, events, variable, func, outcome = NA, type = NA) {
      args_list <-
        list(
          data = quote(filter(events[[as.character(.$match_id)]], team$id == .$team_id)),
          outcome = outcome,
          type = type
        )
      
      # args_list <- args_list[!sapply(args_list, is.null)]
      args_list <- suppressWarnings(Filter(function(a) any(!is.na(a)), args_list))
      
      matches %<>%
        group_split(match_id, team_id) %>%
        map_df(~ mutate(
          ., !!variable := do.call(func, args_list)
        ))
      matches
    }
    
    df <-
      tibble(
        variables = variables,
        func = func,
        outcome = outcome,
        type = type
      )
    
    
    for (i in 1:nrow(df)) {
      row <- df[i, ]
      matches <- create_vars(
        matches = matches,
        events = events,
        variable = row$variables,
        func = row$func,
        outcome = row$outcome,
        type = row$type
      )
    }
    
    matches
    
  }
  
matches_copy <- create_model_variables(matches = matches, events = events, variable = variables, func = func, outcome = outcome, type = type)



function(matches, events, variable, func, outcome = NA, type = NA) {
  args_list <-
    list(
      data = quote(filter(events[[as.character(.$match_id)]], team$id == .$team_id)),
      outcome = outcome,
      type = type
    )
  
  # args_list <- args_list[!sapply(args_list, is.null)]
  args_list <- suppressWarnings(Filter(function(a) any(!is.na(a)), args_list))
  
  matches %<>%
    group_split(match_id, team_id) %>%
    map_df(~ mutate(
      ., !!variable := do.call(func, args_list)
    ))
  matches
  
  
}




df <-
  tibble(
    variables = variables,
    func = func,
    outcome = outcome,
    type = type
  )


for (i in 1:nrow(df)) {
  row <- df[i, ]
  matches_copy <- test(
    matches = matches_copy,
    events = events,
    variable = row$variables,
    func = row$func,
    outcome = row$outcome,
    type = row$type
  )
}
