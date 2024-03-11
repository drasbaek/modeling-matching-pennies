pacman::p_load(tidyverse, here)

# plot estimated MPD versus true values
recovery_plot_MPD <- function(param_df, parameter, color, n_trials){
    plot <- param_df %>%
        ggplot(aes(x = !!sym(paste0("true_", parameter)), y = !!sym(paste0("MPD_", parameter)), color=!!sym(paste0("true_", color)))) + 
        geom_point() +
        scale_color_gradient(low = "#FFD580", high = "darkblue")  +
        geom_abline(intercept = 0, slope = 1, color = "black") +
        labs(title = paste0("Estimated ", parameter, " (MPD)", " vs True ", parameter, " (", n_trials, " trials)"),
             y = paste0("True ", parameter),
             x = paste0("Estimated ", parameter, " (MPD)")) +
        theme_bw()

    return(plot)
}

n_trials_list <- c(120, 300)
for (n_trials in n_trials_list){
    # load param_df csv
    data_file_path <- here::here("data", "recovery", paste0(n_trials, "_trials_recovery.csv"))
    param_df <- read_csv(data_file_path)

    parameters <- c("alpha", "tau")
    colors <- c("tau", "alpha")

    for (j in 1:2) {
        plot <- recovery_plot_MPD(param_df, parameters[j], colors[j], n_trials)
        ggsave(here::here("plots", "recovery", paste0(parameters[j], "_", n_trials, "_MPD_recovery.jpg")), plot)
    }
}

# list all the files in the recovery folder
recovery_files <- list.files(here::here("data", "recovery"), full.names = TRUE)

# iterate through the files and plot the MPD recovery
for (file in recovery_files){
    param_df <- read_csv(file)
    n_trials <- str_extract(file, "\\d+")
    parameters <- c("alpha", "tau")
    colors <- c("tau", "alpha")

    for (j in 1:2) {
        plot <- recovery_plot_MPD(param_df, parameters[j], colors[j], n_trials)
        ggsave(here::here("plots", "recovery", paste0(parameters[j], "_", n_trials, "_MPD_recovery.jpg")), plot)
    }
}