pacman::p_load(tidyverse, here, ggpubr)

# plot estimated MPD versus true values
recovery_plot_MPD <- function(param_df, parameter, color, n_trials, prior_name){
    plot <- param_df %>%
        ggplot(aes(x = !!sym(paste0("true_", parameter)), y = !!sym(paste0("MPD_", parameter)), color=!!sym(paste0("true_", color)))) + 
        geom_point(aes(size=2)) +
        scale_color_gradient(low = "#FFD580", high = "darkblue")  +
        geom_abline(intercept = 0, slope = 1, color = "black", linewidth=1.5) +
        labs(y = paste0("Estimated ", parameter),
             x = paste0("True ", parameter, " (MPD)")) +
        #ggtitle(paste0("Estimated ", parameter, " (MPD)", " vs True ", parameter, " (", n_trials, " trials)")) +
        ggtitle(paste0(n_trials, ": ", prior_name)) + 
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
baseline_files <- subset(all_recovery_files, sapply(all_recovery_files, function(x) get_prior_type(x) %in% c("baseline")))
tau_files <- subset(all_recovery_files, sapply(all_recovery_files, function(x) get_prior_type(x) %in% c("diffTau")))
alpha_files <- subset(all_recovery_files, sapply(all_recovery_files, function(x) get_prior_type(x) %in% c("diffAlpha")))

# iterate through the files and plot the MPD recovery
# Create a list to store plots
plot_list <- list()

for (filepath in baseline_files){
    param_df <- read_csv(filepath)
    
    # get the BASIC filename
    filename <- basename(filepath)
    splitted_filename <- strsplit(filename, "_")

    n_trials <- splitted_filename[[1]][1]
    prior_name <- strsplit(splitted_filename[[1]][[3]], ".", fixed = TRUE)[[1]][[1]]

    parameters <- c("alpha", "tau")
    colors <- c("tau", "alpha")

    for (j in 1:2) {
        plot <- recovery_plot_MPD(param_df, parameters[j], colors[j], n_trials, prior_name)
        # append plot to the list with a name
        plot_list[[paste0(parameters[j], "_", n_trials, "_", prior_name)]] <- plot
        # save the plot
        #ggsave(here::here("plots", "recovery", paste0(parameters[j], "_", n_trials, "_", prior_name, "_recovery.jpg")), plot)
    }
}

# Iterate through the files and plot the MPD recovery for tau
for (filepath in tau_files){
    param_df <- read_csv(filepath)
    
    # get the BASIC filename
    filename <- basename(filepath)
    splitted_filename <- strsplit(filename, "_")

    n_trials <- splitted_filename[[1]][1]
    prior_name <- strsplit(splitted_filename[[1]][[3]], ".", fixed = TRUE)[[1]][[1]]

    plot <- recovery_plot_MPD(param_df, "tau", "alpha", n_trials, prior_name)
    # append plot to the list with a name
    plot_list[[paste0("tau_", n_trials, "_", prior_name)]] <- plot
    # save the plot
    #ggsave(here::here("plots", "recovery", paste0("tau_", n_trials, "_", prior_name, "_recovery.jpg")), plot)
}

# Iterate through the files and plot the MPD recovery for alpha
for (filepath in alpha_files){
    param_df <- read_csv(filepath)
    
    # get the BASIC filename
    filename <- basename(filepath)
    splitted_filename <- strsplit(filename, "_")

    n_trials <- splitted_filename[[1]][1]
    prior_name <- strsplit(splitted_filename[[1]][[3]], ".", fixed = TRUE)[[1]][[1]]

    plot <- recovery_plot_MPD(param_df, "alpha", "tau", n_trials, prior_name)
    # append plot to the list with a name
    plot_list[[paste0("alpha_", n_trials, "_", prior_name)]] <- plot
    # save the plot
    #ggsave(here::here("plots", "recovery", paste0("alpha_", n_trials, "_", prior_name, "_recovery.jpg")), plot)
}

# plot all 6 alpha plots in grid
alpha_order <- c("alpha_60_baseline", "alpha_120_baseline", "alpha_300_baseline", "alpha_60_diffAlpha", "alpha_120_diffAlpha", "alpha_300_diffAlpha")
alpha_plots <- plot_list[alpha_order]
final_alpha_plot <- ggarrange(plotlist = alpha_plots, ncol = 3, nrow = 2)
ggsave(here::here("plots", "recovery", "alpha_recovery_big.jpg"), final_alpha_plot, width = 20, height = 15)

# plot all 6 tau plots in grid
tau_order <- c("tau_60_baseline", "tau_120_baseline", "tau_300_baseline", "tau_60_diffTau", "tau_120_diffTau", "tau_300_diffTau")
tau_plots <- plot_list[tau_order]
final_tau_plot <-ggarrange(plotlist = tau_plots, ncol = 3, nrow = 2)
ggsave(here::here("plots", "recovery", "tau_recovery_big.jpg"), final_tau_plot, width = 20, height = 15)