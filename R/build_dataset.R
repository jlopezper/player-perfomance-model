get_matches <- function(leagues) {
  if (any(leagues %in% 'all')) leagues <- c('11','37','43','49','72')
  
  matches <- 
    map(leagues,
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
  
  matches <-
    matches %>% 
    pivot_longer(c("home_team", "away_team"), names_to = "team_type", values_to = "team_id") %>%
    mutate(result = case_when(
      team_type == "home_team" & home_score > away_score ~ 1,
      team_type == "away_team" & home_score < away_score ~ 1,
      TRUE ~ 0
    ))
  
  matches
  
}

matches <- get_matches(leagues = '37')


get_events <- function(matches) {
  files <- list.files(here("data", "events"), pattern = ".json") # get file names
  files <- files[sub(pattern = "\\.json$", "", files) %in% matches$match_id]
  events <-
    map(here("data", "events", files), ~ read_json(., simplifyVector = TRUE)) %>%
    set_names(gsub(pattern = "\\.json$", "", files))
  
  events
}

events <- get_events(matches = matches)



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

matches <- create_model_variables(matches = matches, events = events, variable = variables, func = func, outcome = outcome, type = type)
