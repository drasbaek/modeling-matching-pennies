# model fitting 
pacman::p_load(tidyverse, here, cmdstanr, pbapply)

stan_filepath = here::here("stan", "RL.stan")

# 'fit_model' function
# 'df' is the dataframe with the data
# 'stan_filepath' is the path to the stan model
# 'onlyprior' is a flag to fit the model with only the prior (1) or with the prior and likelihood (0)
fit_model <- function(df, stan_filepath, onlyprior = 0){
    # prepare the data
    data <- list("trials" = length(df$trial), 
                 "choice"= df$choices, 
                 "feedback" = df$feedback, #ifelse(df$feedback == 0, -1, 1), 
                 "onlyprior" = onlyprior)
    
    # compile the model
    model <- cmdstan_model(stan_filepath, cpp_options = list(stan_threads = TRUE), verbose = FALSE)
    
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

n_trials <- 120
df_filepath = here::here("data", paste0(n_trials, "_trials.csv"))
df <- read_csv(df_filepath)
dfs <- split(df, f = df$agent_id)

# fit models
samples_list = pblapply(dfs, function(df) { # progress bar list apply
    fit_model(df, stan_filepath, onlyprior = 0)
})

# extract median of parameter samples
param_MPD <- pblapply(samples_list, function(samples){
    alpha_MPD <- MPD(as_draws_matrix(samples$draws(variables = "alpha")))
    tau_MPD <- MPD(as_draws_matrix(samples$draws(variables = "tau")))
    
    return(list(alpha_MPD, tau_MPD))
})

process_for_param_recov_plot <- function(dfs, param_MPD) {
    param_df <- data.frame()
    
    # get the true parameters based on dfs
    for (i in 1:length(dfs)) {
        agent_id <- dfs[[i]]$agent_id[1]
        true_alpha <- dfs[[i]]$alpha[1]
        true_tau <- dfs[[i]]$tau[1]
      
        MPD_alpha <- param_MPD[[i]][[1]]
        MPD_tau <- param_MPD[[i]][[2]]

        param_df <- rbind(param_df, data.frame(agent_id, true_alpha, MPD_alpha, true_tau, MPD_tau))
    }
    return (param_df)
}

param_df <- process_for_param_recov_plot(dfs, param_MPD)

# plot MPDs versus true values
recovery_plot <- function(param_df, parameter){
    plot <- param_df %>%
        ggplot(aes(x = !!sym(paste0("MPD_", parameter)), y = !!sym(paste0("true_", parameter)))) + 
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        labs(title = paste0("True ", parameter, " vs MPD ", parameter),
             y = paste0("True ", parameter),
             x = paste0("MPD ", parameter)) +
        theme_minimal()

    ggsave(here::here("plots", "recovery", paste0(parameter, "_recovery.jpg")), plot)
}

for (parameter in c("alpha", "tau")) {
    recovery_plot(param_df, parameter)
}

