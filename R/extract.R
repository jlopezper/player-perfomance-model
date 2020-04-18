pkgs <- c("spatstat", "tidyverse", "jsonlite", "here", "caret", "recipes", "precrec", "pROC", "patchwork","styler")
invisible(lapply(pkgs, require, character.only = TRUE))


get_pass_n <- function(data, type = NULL, outcome = NULL) {
  # returns count of pass type
  if (!is.null(type)) {
    if (is.null(data$pass[[type]])) return(0)
    return(sum(data$pass[[type]], na.rm = TRUE))
  }
  
  # if type and outcome are nulls, then it returns total passes
  if (is.null(c(outcome, type))) return(sum(data$type$name == 'Pass'))
  
  # if outcome = Complete, calculate total complete passes
  if (outcome == 'Complete') {
    not_complete_pass <- sum(!is.na(data$pass$outcome$name))
    total_pass <- sum(data$type$name == 'Pass') 
    complete_pass <- total_pass - not_complete_pass
    return(complete_pass)
  }
  
  # if outcome is selected, filter and return outcome passes
  sum(data$pass$outcome$name == outcome, na.rm = TRUE)
} 


get_deflected_n <- function(data) {
  if (is.null(data$pass$deflected)) return(0)
  sum(data$pass$deflected, na.rm = TRUE)
} 


get_cross_n <- function(data) {
  if (is.null(data$pass$cross)) return(0)
  sum(data$pass$cross, na.rm = TRUE)
} 


get_cut_back_n <- function(data) {
  if (is.null(data$pass$cut_back)) return(0)
  sum(data$pass$cut_back, na.rm = TRUE)
} 

get_switch_n <- function(data) {
  if (is.null(data$pass$switch)) return(0)
  sum(data$pass$switch, na.rm = TRUE)
} 


get_shot_assist_n <- function(data) {
  if (is.null(data$pass$shot_assist)) return(0)
  sum(data$pass$shot_assist, na.rm = TRUE)
}


get_goal_assist_n <- function(data) {
  if (is.null(data$pass$goal_assist)) return(0)
  sum(data$pass$goal_assist, na.rm = TRUE)
} 


get_cards_n <- function(data, type) {
  stopifnot(type %in% c("Yellow Card", "Second Yellow", "Red Card"))
  if (sum(data$type$name == 'Bad Behaviour')  == 0 ) return(0)
  sum(data$bad_behaviour$card$name == type, na.rm = TRUE)
}


get_ball_receipt_n <- function(data) {
  if (sum(data$type$name == 'Ball Receipt*')  == 0) return(0)
  sum(!is.na(data$ball_receipt$outcome$name))
}


get_ball_recovery_n <- function(data) {
  sapply(c('offensive', 'recovery_failure'), 
         function(x) sum(data$ball_recovery[[x]], na.rm = TRUE)) %>% 
    sum()
}

get_block_n <- function(data) {
  sapply(c('save_block', 'deflection', 'offensive', 'counterpress'), 
         function(x) sum(data$block[[x]], na.rm = TRUE)) %>% 
    sum()
}


get_clearance_aerial_n <- function(data) {
  if (sum(data$type$name == 'Clearance')  == 0) return(0)
  sum(data$clearance$aerial_won, na.rm = TRUE)
}


get_clearance_n <- function(data) {
  if (sum(data$type$name == 'Clearance')  == 0) return(0)
  sum(data$type$name == 'Clearance', na.rm = TRUE)
}
  

get_dribble_n <- function(data,outcome) {
  stopifnot(outcome %in% c('Complete', 'Incomplete'))
  if (sum(data$type$name == 'Dribble')  == 0) return(0)
  sum(data$dribble$outcome$name == outcome, na.rm = TRUE)
}


get_duel_n <- function(data, outcome) {
  stopifnot(outcome %in% c('Won', 'Lost In Play', 'Lost Out',
                        'Success', 'Success In Play', 'Success Out'))
  if (sum(data$type$name == 'Duel')  == 0) return(0)
  sum(data$duel$outcome$name == outcome, na.rm = TRUE)
}


get_foul_commited_n <- function(data, type) {
  stopifnot(type %in% c('6 Seconds', 'Backpass Pick', 'Dangerous Play',
                        'Dive', 'Foul Out', 'Handball'))
  if (sum(data$type$name == 'Foul Committed')  == 0) return(0)
  sum(data$foul_committed$type$name == type, na.rm = TRUE)
}


get_foul_won_n <- function(data, type) {
  stopifnot(type %in% c('defensive', 'advantage', 'penalty'))
  if (sum(data$type$name == 'Foul Won')  == 0) return(0)
  sum(data$foul_won[[type]], na.rm = TRUE)
}


get_goalkeeper_saves_n <- function(data) {
  sum(!data$goalkeeper$outcome$name %in% c('No Touch', 'Touched In') 
      & !is.na(data$goalkeeper$outcome$name))
}


get_interception_n <- function(data, outcome) {
  stopifnot(outcome %in% c('Lost', 'Lost In Play', 'Lost Out', 'Success',
                        'Success In Play','Success Out', 'Won'))
  if (sum(data$type$name == 'Interception')  == 0) return(0)
  sum(data$interception$outcome == outcome, na.rm = TRUE)
}


get_under_pressure_n <- function(data) {
  sum(data$under_pressure, na.rm = TRUE)
} 

