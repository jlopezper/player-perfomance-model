Sys.setlocale("LC_ALL", "C")
load(here('model', 'model_image.Rda'))


players_position <-
  map_df(events, ~tibble(player = .$player$id, position = .$position$name)) %>% 
  group_by(player,position) %>% 
  summarise(n = n()) %>%
  group_by(player) %>% 
  filter(n == max(n) & !is.na(position)) %>% 
  select(player, position)



create_vars_players <-
  function(events, variable, func, outcome = NA, type = NA) {
    events <- 
      map(events, function(x) {
        x %>% group_split(player$id, player$name)
      }) 
    
    
    create_vars <- 
      function(events, variable, func, outcome = NA, type = NA) {
        args_list <-
          list(
            data = quote(.),
            outcome = outcome,
            type = type
          )
        
        
        # args_list <- args_list[!sapply(args_list, is.null)]
        args_list <- suppressWarnings(Filter(function(a) any(!is.na(a)), args_list))
        
        events <- 
          map(events, function(x) {
            x %>%
              map(~ mutate(., !! variable := do.call(func, args_list))
                  %>% distinct)
          })
        
        events
      }
    
    
    df <-
      tibble(
        variables = variables,
        func = func,
        outcome = outcome,
        type = type
      ) %>% filter(!variables %in% c('foul_6_seconds', 'goal_assist'))
    
    
    
    for (i in 1:nrow(df)) {
      # cumulative variable vector
      row <- df[i, ]
      
      events <- create_vars(
        events = events,
        variable = row$variables,
        func = row$func,
        outcome = row$outcome,
        type = row$type
      )
    }
    
    
    events <- 
      map(events, function(x) {
        x %>%
          map(~select(.,
                      player = `player$id`,
                      player_name = `player$name`,
                      variables[!variables %in% c("goal_assist", "foul_6_seconds")]
          ) %>% distinct)
      }) %>%   
      map(., bind_rows) %>%
      map(., ~ filter(., !is.na(player)))
    
    
    events
    
    
  }


events <- create_vars_players(events = events, 
                              variable = variables, 
                              func =  func, 
                              outcome = outcome, 
                              type = type)
  


build_r_value <- function(events) {
  # weighted variables
  for (i in coefs$term[1:length(coefs$term)]) {
    events <-
      events %>% 
      map(., function(x) x %>% mutate(!!i := x[[i]] * coefs[which(coefs$term == i), ]$estimate))
  }
  
  events <- 
    map(events, ~ mutate(., r_match = apply(.[!names(.) %in% c("player", "player_name")], 1, sum))) %>% 
    bind_rows(.id = 'match') %>% 
    mutate(match = as.integer(match)) %>% 
    left_join(distinct(matches, match_id, match_date, season), by = c('match' = 'match_id')) %>% 
    mutate(r_norm = normalize(r_match) * 100)
  
  events
}

events <- build_r_value(events)
rating <- events


build_r_weighted_value <- function(events) {
  events <-
    events %>% 
    group_by(player, player_name) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>%
    # summarise(r_norm_med = weighted.mean(r_norm, w = n)) %>%
    summarise(r_norm_med = mean(r_norm)) %>%
    ungroup()
  
  events
  
}

events <- build_r_weighted_value(events)


add_position <- function(events, players_position) {
  events <- 
    events %>% 
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
  
  events
}

events <- add_position(events = events, players_position = players_position)


plot_results <- function(events, rating) {
  ggplot(events, aes(x = position_custom, y = r_norm_med)) +
    geom_violin(fill = 'lightblue', alpha = .6) +
    geom_jitter(shape = 16, position=position_jitter(0.1), color = 'black') +
    ggrepel::geom_text_repel(aes(label = outlier), size = 5, na.rm = TRUE, hjust = -0.05) +
    theme_minimal() +
    labs(x = 'Position', y = 'Player rating (0-100)', title = 'Player perfomance rating') + 
    theme(axis.text.x = element_text(angle = 90, hjust=.98,vjust = 0.2),
          plot.title = element_text(size=20), 
          plot.subtitle = element_text(size = 17),
          axis.text =  element_text(size=14),
          axis.title = element_text(size=14),
          legend.title = element_text(size=14),
          legend.text = element_text(size=13)) +
    ggsave(filename = here('analyses', 'plots', 'perfomance_rating.png'), width = 17, height = 10)
  
  
  ggplot(events, aes(x = r_norm_med, y = position_custom)) +
    ggridges::geom_density_ridges(alpha = 0.5, fill = 'lightblue') +
    theme_minimal()  +
    theme(axis.text.x = element_text(angle = 90, hjust=.98,vjust = 0.2),
          plot.title = element_text(size=20), 
          plot.subtitle = element_text(size = 17),
          axis.text =  element_text(size=14),
          axis.title = element_text(size=14),
          legend.title = element_text(size=14),
          legend.text = element_text(size=13)) +
    labs(x = 'Player rating (0-100)', y = 'Position', title = 'Player perfomance rating') +
    ggsave(filename = here('analyses', 'plots', 'ridges_perfomance_rating.png'), width = 14, height = 10)
  
  
  
  top_players <- 
    events %>% 
    group_by(position_custom) %>% 
    top_n(n = 1, wt = r_norm_med) %>% 
    pull(player)
  
  
  rating %>% 
    left_join(events[c('player','position_custom')], by = 'player') %>% 
    filter(player %in% top_players) %>% 
    ggplot(aes(x = match_date, y = r_norm, group=player_name)) +
    geom_line(aes(color=player_name), size=.75) +
    ylim(0, 100) +
    theme_minimal() +
    facet_wrap(season~position_custom,  scales = "free_x") +
    labs(color = 'Player name', title = 'Player rating progress', subtitle = 'By position and season') +
    theme(legend.position="bottom", strip.text.x = element_text(face = 'bold'),
          axis.text.x = element_text(angle = 90, hjust=.98,vjust = 0.2),
          plot.title = element_text(size=20), 
          plot.subtitle = element_text(size = 17),
          axis.text =  element_text(size=14),
          axis.title = element_text(size=14),
          legend.title = element_text(size=14),
          legend.text = element_text(size=13),
          strip.text = element_text(size = 12)) +
    scale_color_brewer(type = 'div', palette = 'Spectral', direction = 1) + 
    ggsave(filename = here('analyses', 'plots', 'rating_season_position.png'), width = 14, height = 10)
}

plot_results(events, rating)
