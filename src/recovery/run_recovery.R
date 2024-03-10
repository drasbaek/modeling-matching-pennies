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

# function for extracting all estimates (i.e., based on n_iterations) 
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

## RUN PARAMETER RECOVERY ##
stan_filepath = here::here("stan", "RL.stan")

# arguments for parameter recovery
n_trials = 120
priorTypealpha = 1 # bool: 0 uniform, 1 beta
priorSdTau = 1 # actual value of sd for prior

# read data
dfs <- load_dfs(n_trials = n_trials)

# fit models (consider making it parallel with https://stackoverflow.com/questions/62916053/r-asynchronous-parallel-lapply)
samples_list = pblapply(dfs, function(df) { # progress bar list apply
    fit_model(df, stan_filepath, onlyprior = 0, priorTypealpha=priorTypealpha, priorSdTau=priorSdTau)
})

samples_list

# extract median of parameter samples
param_MPD <- pblapply(samples_list, function(samples){
    alpha_MPD <- MPD(as_draws_matrix(samples$draws(variables = "alpha")))
    tau_MPD <- MPD(as_draws_matrix(samples$draws(variables = "tau")))
    
    return(list(alpha_MPD, tau_MPD))
})

param_df <- process_for_param_recov_plot_MPD(dfs, param_MPD)
#param_df <- process_for_param_recov_plot_post(dfs)

# save the data
save_filepath <- here::here("data", "recovery", paste0(n_trials, "_trials_alpha", priorTypealpha, "_tau", priorSdTau, "_recovery.csv"))
write_csv(param_df, save_filepath)

## TEMP TEST CHUNK ## 
# test that it actually works with beta distribution as prior for alpha
draws_df <- as_draws_df(samples_list[[1]]$draws())

param_col = "alpha"
max_x = ifelse(param_col == "alpha", 1, 5)
param_true = 2
posterior_samples_name = "test_beta_prior"

plot <- ggplot(draws_df) +
  geom_density(aes(alpha), fill = "blue", alpha = 0.3) +
  geom_density(aes(alpha_prior), fill = "red", alpha = 0.3) +
  xlim(0, max_x) +
  xlab("Learning Rate") +
  ylab("Posterior Density") +
  labs(title = "Test Plot") + 
  theme_bw()

# save as test
ggsave(here::here("plots", "posterior_updates", paste0(param_col, "_", posterior_samples_name, ".jpg")), plot = plot, width = 10, height = 6)
