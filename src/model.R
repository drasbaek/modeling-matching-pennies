# model fitting 
pacman::p_load(tidyverse, here, cmdstanr)

stan_filepath = here::here("stan", "RL.stan")

# read data
n_trials = 120
df_filepath = here::here("data", paste0(n_trials, "_trials.csv"))
df <- read_csv(df_filepath)

# split the df based on agent_id
dfs <- split(df, f = df$agent_id)

# iterate over dfs
for (i in 1:length(dfs)){
    # get the data correct
    df <- dfs[[i]]
    data <- list("trials" = length(df$trial), "choice"= df$choices, "feedback" = df$feedback)

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

    # save samples
    save(samples, file = here::here("samples", paste0(n_trials, "_trials"), paste0("samples_", i, ".RData")))
    }