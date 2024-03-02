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