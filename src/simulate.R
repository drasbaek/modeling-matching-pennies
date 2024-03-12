# script for creating a game and simulating data for predictive checks and parameter recovery
pacman::p_load(tidyverse, here)
source(here::here("src/game_functions.R")) # for agents and simulation functions
set.seed(42)

n_trials_list <- c(60, 120, 300)
n_games <- 100

##### FOR PARAMETER RECOVERY #####
for (i in 1:length(n_trials_list)){
  n_trials <- n_trials_list[i]
  
  # simulate the games
  games_df <- simulate_games(n_trials, n_games)
  
  # save the games df
  file_path <- here::here("data", paste0(n_trials, "_trials.csv"))
  
  write_csv(games_df, file_path)
}
##### FOR PREDICTIVE CHECKS #####

# init games df
games_df <- data.frame()

# define the parameters we want to test c(alpha, tau)
LowLearning_Stochastic <- c(0.1, 0.1)
LowLearning_Deterministic <- c(0.1, 5)
Learning_Stochastic <- c(0.9, 0.1)
Learning_Deterministic <- c(0.9, 5) 

scenarios <- list(LowLearning_Stochastic, LowLearning_Deterministic, Learning_Stochastic, Learning_Deterministic)
scenarios_labels <- c("LowLearning_Stochastic", "LowLearning_Deterministic", "Learning_Stochastic", "Learning_Deterministic")

# get the choices from the simple agent  
set.seed(42)
n_trials <- 120
hider_choices <- SIMPLE_Agent(n_trials, 0.8)

for (i in 1:4){
  # play the game with predefined alpha and tau
  game_df <- play_game_RL(n_trials, 
                          alpha_picker=scenarios[[i]][1], 
                          tau_picker=scenarios[[i]][2],
                          hider_choices=hider_choices)
  
  # add agent id
  game_df["scenario"] <- scenarios_labels[i]
  
  # add to main games df
  games_df <- bind_rows(games_df, game_df)
}

# save the games df
file_path <- here::here("data", "games_for_pred_check.csv")
write_csv(games_df, file_path)