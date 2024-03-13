pacman::p_load(tidyverse, here, cmdstanr, posterior, ggpubr)

#' predictive_check_plot
predictive_check_plot <- function(predict_df, title){
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
              legend.key.size = unit(2, 'cm'), 
              legend.key.width = unit(3, 'cm'),
              legend.text = element_text(size = 14),
              legend.box.spacing = unit(0, "pt"),
              axis.text=element_text(size=14), 
              axis.title=element_text(size=16), 
              plot.title = element_text(size = 18)
              )

  return(plot)
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

    return(plot)
}

draw_trace_plots <- function(posterior_samples_list, param_col, title){
  draws_df <- as_draws_df(posterior_samples_list$draws())
  
  plot <- ggplot(draws_df, aes(.iteration, !!sym(param_col), group = .chain, color = .chain)) +
    geom_line() +
    labs(title = title, x = "Iteration", y = "Value") +
    theme_bw()+
    theme(legend.position="bottom", 
          legend.title=element_blank(), 
          legend.key.size = unit(0.4, 'cm'), 
          legend.text = element_text(size = 10),
          legend.box.spacing = unit(5, "pt"),
          axis.text=element_text(size=10), 
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 14)
          )

  return(plot)
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

# define lists with data to use for plotting (names become very relevant to plot in for loop with correct titles)
posterior_samples <- list(
  "Learning_Deterministic"=samples_Learning_Deterministic,
  "Learning_Stochastic"=samples_Learning_Stochastic,
  "LowLearning_Deterministic"=samples_LowLearning_Deterministic,
  "LowLearning_Stochastic"=samples_LowLearning_Stochastic
)

posterior_predict <- list(
  "Learning_Deterministic"=posterior_Learning_Deterministic_predict,
  "Learning_Stochastic"=posterior_Learning_Stochastic_predict,
  "LowLearning_Deterministic"=posterior_LowLearning_Deterministic_predict,
  "LowLearning_Stochastic"=posterior_LowLearning_Stochastic_predict
)

# plot predictive checks
prior_plot <- predictive_check_plot(prior_predict, " ")
prior_plot <- prior_plot + theme(plot.margin = margin(0.5, 0, 0, 0, "cm"))
prior_plot <- annotate_figure(prior_plot, top = text_grob("Prior Predictive Checks", vjust=1.6, size=20))
ggsave(here::here("plots", "predictive_checks", "prior_check.jpg"), prior_plot, width = 20, height = 6)


names <- names(posterior_predict)
posterior_plots <- c()
for (i in 1:length(posterior_predict)){
  # get the name of the posterior
  name <- names[i]
  pretty_name <- gsub("_", "-", name)

  # plot the predictive check, append directly to list
  posterior_plots[[i]] <- predictive_check_plot(posterior_predict[[name]], pretty_name)
}

# plot them all in one
posterior_final_plot <- ggarrange(plotlist = posterior_plots, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom")
posterior_final_plot <- posterior_final_plot + theme(plot.margin = margin(1.5, 0, 0, 0, "cm"))
posterior_final_plot <- annotate_figure(posterior_final_plot, top = text_grob("Posterior Predictive Checks", vjust=1.6, size=30))
ggsave(here::here("plots", "predictive_checks", "posterior_check.jpg"), posterior_final_plot, width = 20, height = 24)


names <- names(posterior_samples)
alpha_traceplots <- c()
tau_traceplots <- c()

for (i in 1:length(posterior_samples)){
  # get the name of the posterior
  name <- names[i]
  pretty_name <- gsub("_", "-", name)

  # plot the predictive check, append directly to list
  alpha_traceplots[[i]] <- draw_trace_plots(posterior_samples[[name]], "alpha", pretty_name)
  tau_traceplots[[i]] <- draw_trace_plots(posterior_samples[[name]], "tau", pretty_name)
}

# plot them all in one
alpha_trace_final <- ggarrange(plotlist = alpha_traceplots, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
alpha_trace_final <- alpha_trace_final + theme(plot.margin = margin(1.5, 0, 0, 0, "cm"))
alpha_trace_final <- annotate_figure(alpha_trace_final, top = text_grob("Traceplots for alpha", vjust=1.6, size=30))
ggsave(here::here("plots", "traceplots", "alpha_traceplots.jpg"), alpha_trace_final, width = 15, height = 10)

tau_trace_final <- ggarrange(plotlist = tau_traceplots, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
tau_trace_final <- tau_trace_final + theme(plot.margin = margin(1.5, 0, 0, 0, "cm"))
tau_trace_final <- annotate_figure(tau_trace_final, top = text_grob("Traceplots for tau", vjust=1.6, size=30))
ggsave(here::here("plots", "traceplots", "tau_traceplots.jpg"), tau_trace_final, width = 15, height = 10)

# for loop to plot all the posterior
names <- names(posterior_samples)

alpha_plots <- c()
tau_plots <- c()

for (i in 1:length(posterior_samples)){
  # get the name of the posterior
  name <- names[i]
  pretty_name <- gsub("_", "-", name)

  # index the param_dict to get the alpha and tau
  true_alpha <- param_dict[[paste(name, ".alpha", sep = "")]]
  true_tau <- param_dict[[paste(name, ".tau", sep = "")]]

  title_alpha <- paste0(pretty_name, " (alpha = ", true_alpha, ", tau = ", true_tau, ")")
  title_tau <- paste0(pretty_name, " (alpha = ", true_alpha, ", tau = ", true_tau, ")")

  # plot the posterior update, append to list
  alpha_plots[[i]] <- posterior_update_plot(posterior_samples[[name]], name, true_alpha, "alpha", title_alpha)
  tau_plots[[i]] <- posterior_update_plot(posterior_samples[[name]], name, true_tau, "tau", title_tau)
}

### ALPHA ### 
# plot them all in one
alpha_final_plot <- ggarrange(plotlist = alpha_plots, ncol = 2, nrow = 2)

# add margin for main title (written with annotate figure)
alpha_final_plot <- alpha_final_plot + theme(plot.margin = margin(1.5, 0, 0, 0, "cm"))
alpha_final_plot <- annotate_figure(alpha_final_plot, top = text_grob("Posterior Updates for alpha", vjust=1.6, size=20))

# save 
ggsave(here::here("plots", "posterior_updates", "alpha_posterior_update.jpg"), alpha_final_plot, width = 20, height = 12)

### TAU ### 
tau_final_plot <- ggarrange(plotlist = tau_plots, ncol = 2, nrow = 2)

# add margin for main title (written with annotate figure)
tau_final_plot <- tau_final_plot + theme(plot.margin = margin(1.5, 0, 0, 0, "cm"))
tau_final_plot <- annotate_figure(tau_final_plot, top = text_grob("Posterior Updates for tau", vjust=1.6, size=20))

#save
ggsave(here::here("plots", "posterior_updates", "tau_posterior_update.jpg"), tau_final_plot, width = 20, height = 12)
