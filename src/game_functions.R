# script with code for agents and simulation (adapted from portfolio 1)
pacman::p_load(tidyverse)

#' sigmoid
#' x: input value
#' tau: temperature parameter
sigmoid <- function(x, tau) {
    outcome = 1 / (1 + exp(-tau * x))
    return(outcome)
}

#' REINFORCEMENT_Agent
#' Define a reinforcement learning agent
#' - previous_choice: either 1 (right) or 0 (left)
#' - previous_values: vector of previous values as (right hand, left hand) e.g., c(0.8, 0.2)
#' - previous_feedback: 1 if previous choice was correct, 0 if previous choice was incorrect
#' - alpha: learning rate
#' - tau: temperature parameter (controls randomness of choice, tau at 0 -> stochastic)
REINFORCEMENT_Agent <- function(previous_choice, previous_values, previous_feedback, alpha, tau){

    # update values
    v1 = previous_values[1] + alpha * previous_choice * (previous_feedback - previous_values[1])
    v2 = previous_values[2] + alpha * (1 - previous_choice) * (previous_feedback - previous_values[2])
  
    new_values = c(v1, v2)

    # convert difference in values (right minus left) to probability (of choosing right)
    val_diff = new_values[1] - new_values[2]
    p = sigmoid(val_diff, tau)

    # make choice based on value
    choice = rbinom(1, 1, p)

    # return choice and value
    return(list(choice, new_values))
}

#' SIMPLE_Agent
#' Define a simple agent that makes choices based on a fixed rate
#' - rate: the probability of choosing right
SIMPLE_Agent <- function(n_trials, rate){
    set.seed(42)
    choices = rbinom(n_trials, 1, rate)
    return(choices)
}

#' get_initial_choice of RL agent
get_initial_choice <- function(tau){
    # define initial values as 50/50 right and left 
    initial_value <- c(0.5, 0.5)

    # convert to probability
    initial_difference <- initial_value[2] - initial_value[1] # will be 0
    initial_p <- sigmoid(initial_difference, tau) # will be 0.5

    # make choice
    initial_choice <- rbinom(1,1,initial_p)

    return(initial_choice)
}


#' play_game_RL
#' Plays a game of matching pennies between two agents:
#' - hider: a simple agent with a fixed rate
#' - picker: an RL agent that has varying learning rate and tau. This agents' choices and feedback are stored for parameter recovery
play_game_RL <- function(n_trials, alpha_picker, tau_picker, hider_choices) {
    
    value1_picker <- array(NA, n_trials)
    value2_picker <- array(NA, n_trials)
    choices_picker <- array(NA, n_trials)
    feedback_picker <- array(NA, n_trials)

    value1_picker[1] <- 0.5
    value2_picker[1] <- 0.5

    # init first trial vals for both agents
    choices_picker[1] <- get_initial_choice(tau=tau_picker)

    # calculate feedback for first trial for both agents
    feedback_picker[1] <- ifelse(hider_choices[1] == choices_picker[1], 1, 0)
    
    for (i in 2:n_trials){        
        # set the picker as an agent with varying learning rate and tau based on the function input
        picker <- REINFORCEMENT_Agent(previous_choice = choices_picker[i-1], 
                                      previous_value = c(value1_picker[i-1], value2_picker[i-1]), 
                                      previous_feedback = feedback_picker[i-1], 
                                      alpha = alpha_picker, 
                                      tau = tau_picker)

        # get choices, values and feedback for picker
        choices_picker[i] <- picker[[1]]
        value1_picker[i] <- picker[[2]][1]
        value2_picker[i] <- picker[[2]][2]
        feedback_picker[i] <- ifelse(hider_choices[i] == choices_picker[i], 1, 0)
    }
    # bind the information to data frames
    picker_df <- data.frame("choices" = choices_picker, "feedback" = feedback_picker, "value1" = value1_picker, "value2" = value2_picker)

    # add role to hider_df and picker_df 
    picker_df["role"] <- "picker"

    # add trial columns
    picker_df["trial"] <- 1:n_trials
    
    picker_df["alpha"] <- alpha_picker
    picker_df["tau"] <- tau_picker

    # add column for hider choices
    picker_df["hider_choices"] <- hider_choices
    
    return(picker_df)
}


#' simulate games 
simulate_games <- function(n_trials, n_games){
    # init games df
    games_df <- data.frame()
    
    # get hider choices
    hider_choices <- SIMPLE_Agent(n_trials = n_trials, rate = 0.8)

    # define the learning rates we want to test
    for (i in 1:n_games){
        # sample tau from uniform and alpha constrained between 0 and 1
        alpha <- runif(1, 0, 1)
        tau <- runif(1, 0, 5)

        # play the game with the sampled alpha and tau
        game_df <- play_game_RL(n_trials, alpha_picker=alpha, tau_picker=tau, hider_choices)

        # add agent id
        game_df["agent_id"] <- i

        # add to main games df
        games_df <- bind_rows(games_df, game_df)
    }

    return(games_df)
}