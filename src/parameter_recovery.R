# model fitting 
pacman::p_load(tidyverse, here, cmdstanr)

stan_filepath = here::here("stan", "RL.stan")

# fitting function
fit_model <- function(df, onlyprior = 1){
    
    # prepare the data
    data <- list("trials" = length(df$trial), 
                 "choice"= df$choices, 
                 "feedback" = df$feedback, 
                 "onlyprior" = onlyprior)
    
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

# read data
n_trials = 120
df_filepath = here::here("data", paste0(n_trials, "_trials.csv"))
df <- read_csv(df_filepath)

# split the df based on agent_id
dfs <- split(df, f = df$agent_id)

# sampling based on priors only
prior_samples <- lapply(dfs, function(df){ # a list of samples objects, one per agent (n_games in total) 
    fit_model(data, onlyprior = 1)
})

# sampling based on priors and likelihood
posterior_samples <- lapply(dfs, function(df){
    fit_model(data, onlyprior = 0)
})

# extract choice predictions
prior_predict_samples <- lapply(prior_samples, function(samples){
    extract(samples)$choice_pred # dim: 8000 samples x n_trials
})

posterior_predict_samples <- lapply(posterior_samples, function(samples){
    extract(samples)$choice_pred
})

# calculate proportion of right-hand choices (rowwise proportion of 1s)
prior_predict_right <- lapply(prior_predict_samples, function(samples){
    rowMeans(samples)
})

posterior_predict_right <- lapply(posterior_predict_samples, function(samples){
    rowMeans(samples)
})

# prior predictive check plot from the first agent
prior_predictive_check <- prior_predict_right[[1]] %>%
    as_tibble() %>%
    ggplot(aes(x = value)) +
    geom_histogram(binwidth = 0.01) +
    labs(title = "Prior predictive check",
         x = "Proportion of right-hand choices",
         y = "Frequency")

ggsave(here::here("plots", "prior_predictive_check.png"), plot = prior_predictive_check)

# posterior predictive check plot from the first agent
posterior_predictive_check <- posterior_predict_right[[1]] %>%
    as_tibble() %>%
    ggplot(aes(x = value)) +
    geom_histogram(binwidth = 0.01) +
    labs(title = "Posterior predictive check",
         x = "Proportion of right-hand choices",
         y = "Frequency")

ggsave(here::here("plots", "posterior_predictive_check.png"), plot = posterior_predictive_check)