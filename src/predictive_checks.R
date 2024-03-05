# A script for producing plots with prior and posterior predictive checks
pacman::p_load(tidyverse, here, cmdstanr, rstan, posterior)

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
df_filepath = here::here("data", "games_for_pred_check.csv")
df <- read_csv(df_filepath)
n_trials <- max(df$trial)

# split the df based on scenario
dfs <- split(df, f = df$scenario)

#' sampling based on priors only
#' arbitrary choice of fitting in scenario 1, because the model never sees the data anywas when onlyPrior=1
prior_samples <- fit_model(dfs[[1]], onlyprior = 1)


# sampling based on priors and likelihood
#' creates a list of samples objects, one per agent (4 in total)
posterior_samples <- lapply(dfs, function(df){
    fit_model(df, onlyprior = 0)
})

# extract values
prior_predict_samples <- list(value1 = as_draws_matrix(prior_samples$draws(variables= "value1")), 
                              value2 = as_draws_matrix(prior_samples$draws(variables= "value2"))) # each a matrix with dimensions 8000 samples x n_trials

posterior_predict_samples <- lapply(posterior_samples, function(samples){
    list(value1 = as_draws_matrix(samples$draws(variables= "value1")), 
         value2 = as_draws_matrix(samples$draws(variables= "value2"))) 
})

# prepare data for prior predictive check
prior_predict_df <- data.frame(trial = 1:n_trials, 
                              value1_median = apply(prior_predict_samples$value1, 2, median), 
                              value2_median = apply(prior_predict_samples$value2, 2, median), 
                              value1_3rd_quartile = apply(prior_predict_samples$value1, 2, quantile, probs = 0.75), 
                              value2_3rd_quartile = apply(prior_predict_samples$value2, 2, quantile, probs = 0.75), 
                              value1_1st_quartile = apply(prior_predict_samples$value1, 2, quantile, probs = 0.25), 
                              value2_1st_quartile = apply(prior_predict_samples$value2, 2, quantile, probs = 0.25))
    

# add hider's choice to the data
prior_predict_df$hider_choice <- dfs[[1]]$choices

# prepare data for posterior predictive check
posterior_predict_dfs <- lapply(posterior_predict_samples, function(values_list){
    value1_median <- apply(values_list$value1, 2, median)
    value2_median <- apply(values_list$value2, 2, median)
    value1_3rd_quartile <- apply(values_list$value1, 2, quantile, probs = 0.75)
    value2_3rd_quartile <- apply(values_list$value2, 2, quantile, probs = 0.75)
    value1_1st_quartile <- apply(values_list$value1, 2, quantile, probs = 0.25)
    value2_1st_quartile <- apply(values_list$value2, 2, quantile, probs = 0.25)
    
    return(data.frame(trial = 1:n_trials, 
                      value1_median = value1_median, 
                      value2_median = value2_median, 
                      value1_3rd_quartile = value1_3rd_quartile, 
                      value2_3rd_quartile = value2_3rd_quartile, 
                      value1_1st_quartile = value1_1st_quartile, 
                      value2_1st_quartile = value2_1st_quartile))
    
})

# add hider's choice and alpha and tau to the data
for (i in 1:length(posterior_predict_dfs)){
    posterior_predict_dfs[[i]]$hider_choice <- dfs[[i]]$choices
    posterior_predict_dfs[[i]]$alpha <- dfs[[i]]$alpha[1]
    posterior_predict_dfs[[i]]$tau <- dfs[[i]]$tau[1]
}


# predictive check plot function
predictive_check_plot <- function(predict_df, title){
    plot <- ggplot(predict_df, aes(x = trial)) +
        geom_ribbon(aes(ymin = value1_1st_quartile, ymax = value1_3rd_quartile), fill = "lightblue", alpha = 0.5) +
        geom_line(aes(y = value1_median), color = "blue") +
        geom_ribbon(aes(ymin = value2_1st_quartile, ymax = value2_3rd_quartile), fill = "lightgreen", alpha = 0.5) +
        geom_line(aes(y = value2_median), color = "green") +
        geom_line(aes(y = hider_choice), color = "red") +
        labs(title = title, x = "Trial", y = "Value") +
        theme_bw()
  
    # make save_title by formatting title
    save_title <- gsub(", ", "_", title)
    save_title <- gsub(" ", "_", save_title)
    save_title <- gsub("=", "", save_title)
    ggsave(here::here("plots", paste0(save_title,".jpg")), plot = plot, width = 20, height = 6)
}


# plot prior predictive check
predictive_check_plot(prior_predict_df, "Prior Predictive Check")

# plot posterior predictive check
for (i in 1:length(posterior_predict_dfs)){
    title <- paste0("Posterior Predictive Check, alpha=", dfs[[i]]$alpha[1], ", tau=", dfs[[i]]$tau[1])
    predictive_check_plot(posterior_predict_dfs[[i]], title)
}
