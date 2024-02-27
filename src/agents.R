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
#' - previous_choice: vector of previous choice e.g., c(1,0) meaning that right hand was chosen or c(0,1) meaning that the left hand was chosen
#' - previous_values: vector of previous values as (right hand, left hand) e.g., c(0.8, 0.2)
#' - feedback: 1 if previous choice was correct, 0 if previous choice was incorrect
#' - alpha: learning rate
#' - tau: temperature parameter (controls randomness of choice). Tau at 0 -> stochastic
REINFORCEMENT_Agent <- function(previous_choice, previous_values, feedback, alpha, tau){
    
    # compute previous correct choice from feeddback and previous choice (if feedback is 1, previous correct choice is previous choice, otherwise it is the opposite)
    if (feedback == 1) {previous_correct_choice = previous_choice}
    else {previous_correct_choice = rev(previous_choice)}

    # update value
    new_values = (1-alpha) * previous_values + alpha * previous_correct_choice

    # convert difference in values (right minus left) to propability (of choosing right)
    val_diff = new_values[1] - new_values[2]
    p = sigmoid(val_diff, tau)

    # make choice based on value
    choice = rbinom(1, 1, p)

    # vectorize choice (if choice == 1 then the vector should )
    if (choice == 1) {choice = c(1,0)}
    else {choice = c(0,1)}

    # return choice and value
    return(list(choice, new_values))
}