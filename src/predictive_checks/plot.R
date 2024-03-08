pacman::p_load(tidyverse, here, cmdstanr, posterior)

#' predictive_check_plot
predictive_check_plot <- function(predict_df, title){
    plot <- ggplot(predict_df, aes(x = trial)) +
        geom_ribbon(aes(ymin = value1_1st_quartile, ymax = value1_3rd_quartile), fill = "lightblue", alpha = 0.5) +
        geom_line(aes(y = value1_median), color = "blue") +
        geom_ribbon(aes(ymin = value2_1st_quartile, ymax = value2_3rd_quartile), fill = "lightgreen", alpha = 0.5) +
        geom_line(aes(y = value2_median), color = "darkgreen") +
        geom_line(aes(y = hider_choice), color = "red") +
        labs(title = title, x = "Trial", y = "Value") +
        theme_bw()
  
    # make save_title by formatting title
    save_title <- title
    ggsave(here::here("plots", "predictive_checks", paste0(save_title,".jpg")), plot = plot, width = 20, height = 6)
}

#' posterior_update_plot
#' @param draws_df a dataframe of draws from the posterior (from RData obj)
#' @param param_name the name of the parameter to plot
#' @param param_true the true value of the parameter
#' @param param_col the color of the parameter
#' @param title the title of the plot
posterior_update_plot <- function(posterior_samples_list, posterior_samples_name, param_true, param_col, title){
    draws_df <- as_draws_df(posterior_samples_list$draws())
    
    # set max on x-axis based on param
    max_x = ifelse(param_col == "alpha", 1, 5)
    
    plot <- ggplot(draws_df) +
        geom_density(aes(!!sym(param_col)), fill = "blue", alpha = 0.3) +
        geom_density(aes(!!sym(paste0(param_col, "_prior"))), fill = "red", alpha = 0.3) +
        geom_vline(xintercept = param_true, color = "black", linetype = "dashed") +
        xlim(0, max_x) +
        xlab("Learning Rate") +
        ylab("Posterior Density") +
        labs(title=title) +
        theme_bw()

    ggsave(here::here("plots", "posterior_updates", paste0(param_col, "_", posterior_samples_name, ".jpg")), plot = plot, width = 10, height = 6)
}

# load the data from the predictive checks
prior_predict <- read_csv(here::here("data", "predictive_checks", "prior.csv"))
posterior_high_high_predict <- read_csv(here::here("data", "predictive_checks", "posterior_high_high.csv"))
posterior_high_low_predict <- read_csv(here::here("data", "predictive_checks", "posterior_high_low.csv"))
posterior_low_high_predict <- read_csv(here::here("data", "predictive_checks", "posterior_low_high.csv"))
posterior_low_low_predict <- read_csv(here::here("data", "predictive_checks", "posterior_low_low.csv"))

# create dictionary to use for plotting
param_dict <- c(
  "high_high"=c(posterior_high_high_predict[1,"alpha"], posterior_high_high_predict[1,"tau"]),
  "high_low"=c(posterior_high_low_predict[1,"alpha"], posterior_high_low_predict[1,"tau"]),
  "low_high"=c(posterior_low_high_predict[1,"alpha"], posterior_low_high_predict[1,"tau"]),
  "low_low"=c(posterior_low_low_predict[1,"alpha"], posterior_low_low_predict[1,"tau"])
)

# load all the posterior samples from rdata
samples_high_high <- readRDS(here::here("data", "predictive_checks", "posterior_high_high.rds"))
samples_high_low <- readRDS(here::here("data", "predictive_checks", "posterior_high_low.rds"))
samples_low_high <- readRDS(here::here("data", "predictive_checks", "posterior_low_high.rds"))
samples_low_low <- readRDS(here::here("data", "predictive_checks", "posterior_low_low.rds"))

posterior_samples <- list(
  "high_high"=samples_high_high,
  "high_low"=samples_high_low,
  "low_high"=samples_low_high,
  "low_low"=samples_low_low
)

# plot the predictive checks
predictive_check_plot(prior_predict, "Prior Predictive Check")
predictive_check_plot(posterior_high_high_predict, "Posterior Predictive Check: High High")
predictive_check_plot(posterior_high_low_predict, "Posterior Predictive Check: High Low")
predictive_check_plot(posterior_low_high_predict, "Posterior Predictive Check: Low High")
predictive_check_plot(posterior_low_low_predict, "Posterior Predictive Check: Low Low")

# for loop to plot all the posterior
names <- names(posterior_samples)
for (i in 1:length(posterior_samples)){
  # get the name of the posterior
  name <- names[i]

  # index the param_dict to get the alpha and tau
  true_alpha <- param_dict[[paste(name, ".alpha", sep = "")]]
  true_tau <- param_dict[[paste(name, ".tau", sep = "")]]

  title_alpha <- paste0("Posterior update for: alpha (alpha = ", true_alpha, ", tau = ", true_tau, ")")
  title_tau <- paste0("Posterior update for: tau (alpha = ", true_alpha, ", tau = ", true_tau, ")")

  # plot the posterior
  posterior_update_plot(posterior_samples[[name]], name, true_alpha, "alpha", title_alpha)
  posterior_update_plot(posterior_samples[[name]], name, true_tau, "tau", title_tau)
}



