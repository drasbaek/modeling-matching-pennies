pacman::p_load(tidyverse, here)

# plot estimated MPD versus true values
recovery_plot_MPD <- function(param_df, parameter, color, n_trials){
    plot <- param_df %>%
        ggplot(aes(x = !!sym(paste0("true_", parameter)), y = !!sym(paste0("MPD_", parameter)), color=!!sym(paste0("true_", color)))) + 
        geom_point(aes(size=2)) +
        scale_color_gradient(low = "#FFD580", high = "darkblue")  +
        geom_abline(intercept = 0, slope = 1, color = "black", linewidth=1.5) +
        labs(title = paste0("Estimated ", parameter, " (MPD)", " vs True ", parameter, " (", n_trials, " trials)"),
             y = paste0("Estimated ", parameter),
             x = paste0("True ", parameter, " (MPD)")) +
        theme_bw()+
        theme(legend.key.size = unit(1, 'cm'), 
              legend.text = element_text(size = 12),
              legend.box.spacing = unit(0, "pt"),
              axis.text=element_text(size=12), 
              axis.title=element_text(size=14), 
              plot.title = element_text(size = 16)) +
        guides(size=FALSE)

    return(plot)
}

get_prior_type <- function(filename) {
  if (grepl("diffTau", filename)) {
    return("diffTau")
  } else if (grepl("diffAlpha", filename)) {
    return("diffAlpha")
  } else if (grepl("baseline", filename)) {
    return("baseline")
  } else {
    return(NA)
  }
}

# list all the files in the recovery folder
all_recovery_files <- list.files(here::here("data", "recovery"), full.names = TRUE)
baseline_files <- subset(recovery_files, sapply(recovery_files, function(x) get_trial_type(x) %in% c("baseline")))
tau_files <- subset(recovery_files, sapply(recovery_files, function(x) get_trial_type(x) %in% c("diffTau")))
alpha_files <- subset(recovery_files, sapply(recovery_files, function(x) get_trial_type(x) %in% c("diffAlpha")))

# iterate through the files and plot the MPD recovery
for (filepath in baseline_files){
    param_df <- read_csv(filepath)
    
    # get the BASIC filena
    filename <- basename(filepath)
    splitted_filename <- strsplit(filename, "_")

    n_trials <- splitted_filename[[1]][1]
    prior_name <- strsplit(splitted_filename[[1]][[3]], ".", fixed = TRUE)[[1]][[1]]

    parameters <- c("alpha", "tau")
    colors <- c("tau", "alpha")

    for (j in 1:2) {
        plot <- recovery_plot_MPD(param_df, parameters[j], colors[j], n_trials)
        ggsave(here::here("plots", "recovery", paste0(parameters[j], "_", n_trials, "_", prior_name, "_recovery.jpg")), plot)
    }
}

# iterate through the files and plot the MPD recovery
for (filepath in tau_files){
    param_df <- read_csv(filepath)
    
    # get the BASIC filena
    filename <- basename(filepath)
    splitted_filename <- strsplit(filename, "_")

    n_trials <- splitted_filename[[1]][1]
    prior_name <- strsplit(splitted_filename[[1]][[3]], ".", fixed = TRUE)[[1]][[1]]

    plot <- recovery_plot_MPD(param_df, "tau", "alpha", n_trials)
    ggsave(here::here("plots", "recovery", paste0("tau_", n_trials, "_", prior_name, "_recovery.jpg")), plot)
}

for (filepath in alpha_files){
    param_df <- read_csv(filepath)
    
    # get the BASIC filena
    filename <- basename(filepath)
    splitted_filename <- strsplit(filename, "_")

    n_trials <- splitted_filename[[1]][1]
    prior_name <- strsplit(splitted_filename[[1]][[3]], ".", fixed = TRUE)[[1]][[1]]

    plot <- recovery_plot_MPD(param_df, "alpha", "tau", n_trials)
    ggsave(here::here("plots", "recovery", paste0("alpha_", n_trials, "_", prior_name, "_recovery.jpg")), plot)
}

