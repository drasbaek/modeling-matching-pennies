# script for creating a game and simulating data for prior and posterior predictive checks
pacman::p_load(tidyverse, here)
source(here::here("src/agents.R")) # for sigmoid and RL agent
source(here::here("src/simulate_for_param_recov.R")) # source play_game_RL function

set.seed(42)

# init games df
games_df <- data.frame()

# define the parameters we want to test c(alpha, tau)
low_low <- c(0.2, 0.4)
low_high <- c(0.2, 2)
high_low <- c(0.8, 0.4)
high_high <- c(0.8, 2)
scenarios <- list(low_low, low_high, high_low, high_high)
scenarios_labels <- c("low_low", "low_high", "high_low", "high_high")


# define the number of trials
n_trials <- 120

for (i in 1:4){

  # play the game with predefined alpha and tau
  game_df <- play_game_RL(n_trials, 
                          alpha_hider=0.2, 
                          alpha_picker=scenarios[[i]][1], 
                          tau_hider=0.4, 
                          tau_picker=scenarios[[i]][2])
  
  # add agent id
  game_df["scenario"] <- scenarios_labels[i]
  
  # add to main games df
  games_df <- bind_rows(games_df, game_df)
}

# save the games df
file_path <- here::here("data", "games_for_pred_check.csv")
write_csv(games_df, file_path)