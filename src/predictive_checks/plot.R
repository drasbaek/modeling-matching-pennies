pacman::p_load(tidyverse, here, cmdstanr, posterior)

#' predictive_check_plot
predictive_check_plot <- function(predict_df, title, save_title){
  plot <- ggplot(predict_df, aes(x = trial)) +
        # value 1 
        geom_ribbon(aes(ymin = value1_1st_quartile, ymax = value1_3rd_quartile, fill = "Value 1 (Right Hand)"), color = NA, alpha = 0.5, show.legend=FALSE) +
        geom_line(aes(y = value1_median, color = "Value 1 (Right Hand)")) +
        
        # value 0
        geom_ribbon(aes(ymin = value2_1st_quartile, ymax = value2_3rd_quartile, fill = "Value 0 (Left Hand)"), color = NA, alpha = 0.5, show.legend=FALSE) +
        geom_line(aes(y = value2_median, color = "Value 0 (Left Hand)")) +
        
        # add hider choice 
        geom_line(aes(y = hider_choice, color = "Hider Choice")) +

        # color and fill of the lines 
        scale_color_manual(values = c("Value 1 (Right Hand)" = "blue", "Value 0 (Left Hand)" = "darkgreen", "Hider Choice" = "red")) +
        scale_fill_manual(name = "Legend",
                          values = c("Value 1 (Right Hand)" = "lightblue", "Value 0 (Left Hand)" = "lightgreen"),
                          labels = c("Value 1 (Right Hand)", "Value 0 (Left Hand)")) +

        # theme, labels, legend, font sizes
        labs(title = title, x = "Trial", y = "Value") +
        theme_bw()+
        theme(legend.position="bottom", 
              legend.title=element_blank(), 
              legend.key.size = unit(1, 'cm'), 
              legend.text = element_text(size = 12),
              legend.box.spacing = unit(0, "pt"),
              axis.text=element_text(size=12), 
              axis.title=element_text(size=14), 
              plot.title = element_text(size = 16)
              )
  
    # make save_title by formatting title
    ggsave(here::here("plots", "predictive_checks", paste0(save_title, ".jpg")), plot = plot, width = 20, height = 6)
}

#' posterior_update_plot
#' @param draws_df a dataframe of draws from the posterior (from RData obj)
#' @param param_name the name of the parameter to plot
#' @param param_true the true value of the parameter
#' @param param_col the color of the parameter
#' @param title the title of the plot
posterior_update_plot <- function(posterior_samples_list, posterior_samples_name, param_true, param_col, title){
    draws_df <- as_draws_df(posterior_samples_list$draws())
    
    # set max on x-axis based on param, limit to 1 for alpha, 8 for tau
    max_x = ifelse(param_col == "alpha", 1, 7)
    
    plot <- ggplot(draws_df) +
      # posterior
      geom_density(aes(!!sym(param_col), fill = "Posterior"), alpha = 0.4) +
      
      # prior 
      geom_density(aes(!!sym(paste0(param_col, "_prior")), fill = "Prior"), alpha = 0.4) +
      
      # true value
      geom_vline(aes(xintercept = param_true, color = "True Value"), linetype = "dashed") +
      
      xlim(0, max_x) +
      xlab(param_col) +
      ylab("Posterior Density") +
      labs(title=title) +
      scale_fill_manual(name = "Distribution",
                        values = c("Posterior" = "#005DFF", "Prior" = "#FF0000"),
                        labels = c("Posterior", "Prior")) +
      scale_color_manual(name = "",
                        values = "black",
                        labels = "True Value") +
      theme_bw() +
      theme(legend.position="bottom", 
            legend.title=element_blank(), 
            legend.key.size = unit(0.4, 'cm'), 
            legend.text = element_text(size = 10),
            legend.box.spacing = unit(5, "pt"),
            axis.text=element_text(size=10), 
            axis.title=element_text(size=12), 
            plot.title = element_text(size = 14)
            )

    ggsave(here::here("plots", "posterior_updates", paste0(param_col, "_", posterior_samples_name, ".jpg")), plot = plot, width = 10, height = 6)
}

# load the data from the predictive checks
prior_predict <- read_csv(here::here("data", "predictive_checks", "prior.csv"))
posterior_Learning_Deterministic_predict <- read_csv(here::here("data", "predictive_checks", "posterior_Learning_Deterministic.csv"))
posterior_Learning_Stochastic_predict <- read_csv(here::here("data", "predictive_checks", "posterior_Learning_Stochastic.csv"))
posterior_LowLearning_Deterministic_predict <- read_csv(here::here("data", "predictive_checks", "posterior_LowLearning_Deterministic.csv"))
posterior_LowLearning_Stochastic_predict <- read_csv(here::here("data", "predictive_checks", "posterior_LowLearning_Stochastic.csv"))

# create dictionary to use for plotting
param_dict <- c(
  "Learning_Deterministic"=c(posterior_Learning_Deterministic_predict[1,"alpha"], posterior_Learning_Deterministic_predict[1,"tau"]),
  "Learning_Stochastic"=c(posterior_Learning_Stochastic_predict[1,"alpha"], posterior_Learning_Stochastic_predict[1,"tau"]),
  "LowLearning_Deterministic"=c(posterior_LowLearning_Deterministic_predict[1,"alpha"], posterior_LowLearning_Deterministic_predict[1,"tau"]),
  "LowLearning_Stochastic"=c(posterior_LowLearning_Stochastic_predict[1,"alpha"], posterior_LowLearning_Stochastic_predict[1,"tau"])
)

# load all the posterior samples from rdata
samples_Learning_Deterministic <- readRDS(here::here("data", "predictive_checks", "posterior_Learning_Deterministic.rds"))
samples_Learning_Stochastic <- readRDS(here::here("data", "predictive_checks", "posterior_Learning_Stochastic.rds"))
samples_LowLearning_Deterministic <- readRDS(here::here("data", "predictive_checks", "posterior_LowLearning_Deterministic.rds"))
samples_LowLearning_Stochastic <- readRDS(here::here("data", "predictive_checks", "posterior_LowLearning_Stochastic.rds"))

posterior_samples <- list(
  "Learning_Deterministic"=samples_Learning_Deterministic,
  "Learning_Stochastic"=samples_Learning_Stochastic,
  "LowLearning_Deterministic"=samples_LowLearning_Deterministic,
  "LowLearning_Stochastic"=samples_LowLearning_Stochastic
)

# plot the predictive checks
predictive_check_plot(prior_predict, "Prior Predictive Check", "prior_check")
predictive_check_plot(posterior_Learning_Deterministic_predict, "Posterior Predictive Check: Learning-Deterministic", "posterior_check_Learning_Deterministic")
predictive_check_plot(posterior_Learning_Stochastic_predict, "Posterior Predictive Check: Learning-Stochastic", "posterior_check_Learning_Stochastic")
predictive_check_plot(posterior_LowLearning_Deterministic_predict, "Posterior Predictive Check: LowLearning-Deterministic", "posterior_check_LowLearning_Deterministic")
predictive_check_plot(posterior_LowLearning_Stochastic_predict, "Posterior Predictive Check: LowLearning-Stochastic", "posterior_check_LowLearning_Stochastic")

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



