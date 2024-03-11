# script for running parameter recovery (contains both functions and the main script)
pacman::p_load(tidyverse, here, cmdstanr, pbapply, posterior)

## DEFINE FUNCTIONS ## 

# 'fit_model' function
# 'df' is the dataframe with the data
# 'stan_filepath' is the path to the stan model
# 'onlyprior' is a flag to fit the model with only the prior (1) or with the prior and likelihood (0)
fit_model <- function(df, stan_filepath, onlyprior = 0, priorTypealpha=1, priorSdTau=1){
    # prepare the data
    data <- list("trials" = length(df$trial), 
                 "choice"= df$choices, 
                 "feedback" = df$feedback, #ifelse(df$feedback == 0, -1, 1), 
                 "onlyprior" = onlyprior, 
                 "priorTypealpha" = priorTypealpha, 
                 "priorSdTau" = priorSdTau
                 )
    
    # compile the model
    model <- cmdstan_model(stan_filepath, cpp_options = list(stan_threads = TRUE))
    
    # fit the model
    samples <- model$sample(
        data = data,
        seed = 420,
        threads_per_chain = 4,
        iter_warmup = 1000,
        iter_sampling = 2000,
        refresh = 500,
        max_treedepth = 10,
        adapt_delta = 0.99
    )
    
    return(samples)
}

MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

load_dfs <- function(n_trials) {
    df_filepath = here::here("data", paste0(n_trials, "_trials.csv"))
    df <- read_csv(df_filepath)
    dfs <- split(df, f = df$agent_id)
    return(dfs)
}

process_for_param_recov_plot_MPD <- function(dfs, param_MPD) {
    param_df <- data.frame()
    
    # get the true parameters based on dfs
    for (i in 1:length(dfs)) {
        agent_id <- dfs[[i]]$agent_id[1]
        true_alpha <- dfs[[i]]$alpha[1]
        true_tau <- dfs[[i]]$tau[1]
      
        MPD_alpha <- param_MPD[[i]][[1]]
        MPD_tau <- param_MPD[[i]][[2]]

        param_df <- rbind(param_df, data.frame(agent_id, 
                                               true_alpha, 
                                               MPD_alpha, 
                                               true_tau, 
                                               MPD_tau
                                               ))
    }
    return (param_df)
}

# function for extracting all estimates (i.e., based on n_iterations). NB not used atm!
process_for_param_recov_plot_post <- function(dfs) {
    param_df <- data.frame()
    
    # get the true parameters based on dfs
    for (i in 1:length(dfs)) {
        agent_id <- rep( dfs[[i]]$agent_id[1] , 8000 )
        true_alpha <- rep( dfs[[i]]$alpha[1] , 8000 )
        true_tau <- rep( dfs[[i]]$tau[1] , 8000 )

        # only extract 
        draws_df <- as_draws_df(samples_list[[i]]$draws(variables=c("alpha", "tau")))

        post_alpha <- draws_df$alpha
        post_tau <- draws_df$tau

        param_df <- rbind(param_df, data.frame("agent_id" = agent_id, 
                                               "true_alpha" = true_alpha,
                                               "post_alpha" = post_alpha,  
                                               "true_tau" = true_tau,
                                               "post_tau" = post_tau 
                                               ))
    }


    return (param_df)
}

run_parameter_recovery <- function(stan_filepath, n_trials, priorTypealpha, priorSdTau, filename) {
    # read data
    dfs <- load_dfs(n_trials = n_trials)

    # fit models (consider making it parallel with https://stackoverflow.com/questions/62916053/r-asynchronous-parallel-lapply)
    samples_list = pblapply(dfs, function(df) { # progress bar list apply
        fit_model(df, stan_filepath, onlyprior = 0, priorTypealpha=priorTypealpha, priorSdTau=priorSdTau)
    })

    # extract median of parameter samples
    param_MPD <- pblapply(samples_list, function(samples){
        alpha_MPD <- MPD(as_draws_matrix(samples$draws(variables = "alpha")))
        tau_MPD <- MPD(as_draws_matrix(samples$draws(variables = "tau")))

        return(list(alpha_MPD, tau_MPD))
    })

    # create df 
    param_df <- process_for_param_recov_plot_MPD(dfs, param_MPD)

    # save the data
    save_filepath <- here::here("data", "recovery", filename)
    write_csv(param_df, save_filepath)

    return(list(samples_list, param_df))
}

## RUN PARAMETER RECOVERY ##
stan_filepath = here::here("stan", "RL.stan")

# set trial combinations
n_trials_list <- c(60, 120, 300)

# run parameter recovery for all trial combinations
for (n_trials in n_trials_list) {
    print(paste0("Running parameter recovery for ", n_trials, " trials"))
    # run baseline recovery with alpha as a uniform prior (0, 1) and tau as a lognormal prior (0, 1)
    print("Running with BASELINE priors")
    baseline = run_parameter_recovery(stan_filepath, n_trials, priorTypealpha=0, priorSdTau=1, filename = paste0(n_trials, "_trials", "_baseline.csv"))
    
    # run with same tau but changing alpha to a beta(2,2)
    print("Running with alpha as a beta(2,2)")
    diff_alpha = run_parameter_recovery(stan_filepath, n_trials, priorTypealpha=1, priorSdTau=1, filename = paste0(n_trials, "_trials", "_diffAlpha.csv"))
    
    # run with the original alpha but changing tau's sd to 0.2
    print("Running with tau as (0,0.2)")
    diff_tau = run_parameter_recovery(stan_filepath, n_trials, priorTypealpha=0, priorSdTau=0.2, filename = paste0(n_trials, "_trials", "_diffTau.csv"))
    }