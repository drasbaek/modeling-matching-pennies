# script for creating a game and simulating
pacman::p_load(tidyverse, here)
source(here::here("src/agents.R")) # for sigmoid and RL agent

#' get_initial_choice 
get_initial_choice <- function(tau){
    # define inital values as 50/50 right and left 
    initial_value <- c(0.5, 0.5)

    # convert to probability
    initial_difference <- initial_value[2] - initial_value[1] # will be 0
    initial_p <- sigmoid(initial_difference, tau)

    # make choice
    initial_choice <- rbinom(1,1,initial_p)

    return(initial_choice)
}

#' play_game_RL
#' Plays a game of matching pennies between two agents:
#' - hider: an RL agent that has FIXED learning rate and tau (just to enable the game)
#' - picker: an RL agent that has varying learning rate and tau. This agents' choices and feedback are stored for parameter recovery
play_game_RL <- function(n_trials, alpha_picker=0.2, tau_picker=0.3) {
    # init arrays to store values, choices and feedback for the agents
    value1_hider <- array(NA, n_trials)
    value2_hider <- array(NA, n_trials)
    choices_hider <- array(NA, n_trials)
    feedback_hider <- array(NA, n_trials)

    value1_picker <- array(NA, n_trials)
    value2_picker <- array(NA, n_trials)
    choices_picker <- array(NA, n_trials)
    feedback_picker <- array(NA, n_trials)

    # set initial value for both agents
    value1_hider[1] <- 0.5
    value2_hider[1] <- 0.5

    value1_picker[1] <- 0.5
    value2_picker[1] <- 0.5

    # init first trial vals for both agents
    choices_hider[1] <- get_initial_choice(tau=0.4)
    choices_picker[1] <- get_initial_choice(tau=tau_picker)

    # calculate feedback for first trial for both agents
    feedback_hider[1] <- ifelse(choices_hider[1] != choices_picker[1], 1, 0)
    feedback_picker[1] <- ifelse(choices_hider[1] == choices_picker[1], 1, 0)
    
    for (i in 2:n_trials){
        # set the hider as an agent with FIXED learning rate and tau
        hider <- REINFORCEMENT_Agent(previous_choice = choices_hider[i-1], previous_value = c(value1_hider[i-1], value2_hider[i-1]), feedback = feedback_hider[i-1], alpha = 0.2, tau = 0.4)
        
        # set the picker as an agent with varying learning rate and tau based on the function input
        picker <- REINFORCEMENT_Agent(previous_choice = choices_picker[i-1], previous_value = c(value1_picker[i-1], value2_picker[i-1]), feedback = feedback_picker[i-1], alpha = alpha_picker, tau = tau_picker)

        # get choices for hider and picker
        choices_hider[i] <- hider[[1]]
        choices_picker[i] <- picker[[1]]

        # get values
        value1_hider[i] <- hider[[2]][1]
        value2_hider[i] <- hider[[2]][2]
        value1_picker[i] <- picker[[2]][1]
        value2_picker[i] <- picker[[2]][2]

        # get feedback
        feedback_hider[i] <- ifelse(choices_hider[i] != choices_picker[i], 1, 0)
        feedback_picker[i] <- ifelse(choices_hider[i] == choices_picker[i], 1, 0)
    }
    # bind the information to dataframes
    hider_df <- data.frame("choices" = choices_hider, "feedback" = feedback_hider, "value1" = value1_hider, "value2" = value2_hider)
    picker_df <- data.frame("choices" = choices_picker, "feedback" = feedback_picker, "value1" = value1_picker, "value2" = value2_picker)

    # add role to hider_df and picker_df 
    hider_df["role"] <- "hider"
    picker_df["role"] <- "picker"

    # add trial columns
    hider_df["trial"] <- 1:n_trials
    picker_df["trial"] <- 1:n_trials

    # add alpha and tau
    picker_df["alpha"] <- alpha_picker
    picker_df["tau"] <- tau_picker

    return(picker_df)
}

#' simulate games 
simulate_games <- function(n_trials, n_games){
    # init games df
    games_df <- data.frame()

    # define the learning rates we want to test
    for (i in 1:n_games){
        print(i)
        # sample tau from uniform and alpha constrained between 0 and 1
        alpha <- runif(1, 0, 1)
        tau <- runif(1, 0, 5)

        # play the game with the sampled alpha and tau
        df <- play_game_RL(n_trials, alpha_picker=alpha, tau_picker=tau)

        # add agent id
        df["agent_id"] <- i

        # add to main games df
        games_df <- bind_rows(games_df, df)
    }

    return(games_df)
}

n_trials <- 120
games_df <- simulate_games(n_trials, 100)

# save the games df
file_path <- here::here("data", paste0(n_trials, "_trials.csv"))

write_csv(games_df, file_path)