pacman::p_load(tidyverse, here)

# load param_df csv
param_df <- read_csv(here::here("data", "recovery_param_df.csv"))

param_df

# plot estimated MPD versus true values
recovery_plot_MPD <- function(param_df, parameter, color){
    plot <- param_df %>%
        ggplot(aes(x = !!sym(paste0("post_", parameter)), y = !!sym(paste0("true_", parameter)), color=!!sym(paste0("true_", color)))) + 
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        labs(title = paste0("True ", parameter, " vs MPD ", parameter),
             y = paste0("True ", parameter),
             x = paste0("MPD ", parameter)) +
        theme_minimal()

    ggsave(here::here("plots", "recovery", paste0(parameter, "_MPD_recovery.jpg")), plot)
}

# plot estimated posterior versus true values
recovery_plot_post <- function(param_df, parameter, color){
    plot <- param_df %>%
        ggplot(aes(x = !!sym(paste0("true_", parameter)), y = !!sym(paste0("post_", parameter)), color=!!sym(paste0("true_", color)), alpha=0.001)) + 
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        labs(title = paste0("Estimated ", parameter, " vs True ", parameter),
             y = paste0("post ", parameter),
             x = paste0("true ", parameter)) +
        theme_minimal()

    ggsave(here::here("plots", "recovery", paste0(parameter, "_post_recovery.jpg")), plot)
}

parameters <- c("alpha", "tau")
colors <- c("tau", "alpha")

for (i in 1:2) {
    recovery_plot_post(param_df, parameters[i], colors[i])
}


