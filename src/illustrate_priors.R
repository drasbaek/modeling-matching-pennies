# script for plotting sigmoid curve +  priors
pacman::p_load(tidyverse, ggpubr)

plot_sigmoid_curves <- function(x_values, tau_values, colors, n_col, n_rows, filename) {
  plots <- list()
  
  for (i in 1:length(tau_values)) {
    tau <- tau_values[i]
    y_values <- 1 / (1 + exp(-x_values * tau))
    df <- data.frame(x = x_values, y = y_values)
    p <- ggplot(df, aes(x, y)) +
      geom_line(color = colors[i], aes(linewidth = 0.5)) + 
      labs(title = paste("Sigmoid curve with tau =", tau), x = "x", y = "y") +
      theme_bw()
      # remove legend
      p <- p + theme(legend.position="none")
    plots[[i]] <- p
  }
  final_plot <- ggarrange(plotlist = plots, ncol = )
  ggsave(filename, final_plot)

  return(final_plot)
}

# define x_values and tau_values
x_values <- seq(-10, 10, length=100)
tau_values <- c(0.1, 0.5, 1, 2, 3, 4) # Example tau values
colors <- c('#C9D4F6','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

# plot three values of tau beside each other
filepath <- file.path("plots", "tau_sigmoid_curves.jpg")
plot <- plot_sigmoid_curves(x_values, tau_values, colors, n_col = 3, n_rows = 2, filepath)

plot_tau_priors <- function(mean1, sd1, mean2, sd2) {
  # generate data for the first prior distribution
  logtaus1 <- rnorm(1000, mean1, sd1)
  taus1 <- exp(logtaus1)
  data1 <- data.frame(taus = taus1, prior = paste("Lognormal (", mean1, ", ", sd1, ")", sep = ""))
  
  # generate data for the second prior distribution
  logtaus2 <- rnorm(1000, mean2, sd2)
  taus2 <- exp(logtaus2)
  data2 <- data.frame(taus = taus2, prior = paste("Lognormal (", mean2, ", ", sd2, ")", sep = ""))
  
  # combine the data
  combined_data <- rbind(data1, data2)
  
  # plot
  plot <- ggplot(combined_data, aes(x = taus, fill = prior)) +
    geom_density(alpha = 0.5) +
    labs(title = "Prior distribution of tau",
         x = "tau",
         y = "Density") +
    xlim(0, 7) +
    scale_fill_manual(values = c("darkgrey", "#FF0000")) +
    theme_bw()+
    theme(legend.position="bottom",
          legend.title=element_blank(), 
          legend.key.size = unit(0.4, 'cm'), 
          legend.text = element_text(size = 10),
          legend.box.spacing = unit(5, "pt"),
          axis.text=element_text(size=10), 
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 14))

  
  return(plot)
}

# plot and save
plot <- plot_tau_priors(0, 0.2, 0, 1)
ggsave(file.path("plots", "tau_priors.jpg"), plot, width = 10, height = 6)

