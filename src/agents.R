# script with code for agents (adapted from portfolio 1)

pacman::p_load(tidyverse)

sigmoid <- function(x, tau) {
    outcome = 1 / (1 + exp(-tau * x))
    return(outcome)
}

# REINFORCEMENT LEARNING AGENT

#' REINFORCEMENT_Agent
#' previous_choice: vector of previous choice ( c(1,0) or c(0,1) )
#' previous_values: vector of previous values (between 0 and 1)
#' feedback: 1 if previous choice was correct, 0 if previous choice was incorrect
#' alpha: learning rate
#' tau: temperature parameter (controls randomness of choice)
REINFORCEMENT_Agent <- function(previous_choice, previous_values, feedback, alpha, tau){
    
    # compute previous correct choice from feeddback and previous choice (if feedback is 1, previous correct choice is previous choice, otherwise it is the opposite)
    if (feedback == 1) {previous_correct_choice = previous_choice}
    else {previous_correct_choice = rev(previous_choice)}

    # update value
    new_values = (1-alpha) * previous_values + alpha * previous_correct_choice

    # convert to propability
    val_diff = new_values[2] - new_values[1]
    p = sigmoid(val_diff, tau)

    # make choice based on value
    choice = rbinom(1, 1, p)

    # return choice and value
    return(list(choice, new_values))
}

result <- REINFORCEMENT_Agent(previous_choice = c(1,0),
                    previous_values = c(0.8, 0.2),
                    feedback = 1,
                    alpha = 0.1,
                    tau = 1)
print(result)
