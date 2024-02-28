# model fitting 
pacman::p_load(tidyverse, here, cmdstanr)

stan_filepath = here::here("src", "RL.stan")

# read data
df_filepath = here::here("data", "120_trials.csv")
df <- read_csv(df_filepath)

# split the df based on agent_id
dfs <- split(df, f = df$agent_id)

# iterate over dfs
for (i in 1:length(dfs)){
    # get the data correct
    df <- dfs[[i]]
    data <- list("trials" = length(df$trial)/100, "choice"= df$choices, "feedback" = df$feedback)

    # compile the model
    model <- cmdstan_model(stan_filepath, cpp_options = list(stan_threads = TRUE))
    
    # fit the model
    fit <- model$sample(
        data = data,
        seed = 420,
        iter_warmup = 1000,
        iter_sampling = 2000,
        refresh = 500,
        max_treedepth = 10,
        adopt_delta = 0.99
    )
    }
