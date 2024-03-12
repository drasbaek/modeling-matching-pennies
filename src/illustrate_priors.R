# script for plotting sigmoid curve +  priors
pacman::p_load(tidyverse, ggpubr)

# set seed to draw the same random numbers to not have to commit new plots all the time
set.seed(43)

plot_sigmoid_curves <- function(x_values, tau_values, colors, n_col, n_rows) {
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
  final_plot <- ggarrange(plotlist = plots, ncol = n_col, nrow = n_rows)

  return(final_plot)
}

# define x_values and tau_values
x_values <- seq(-10, 10, length=100)
tau_values <- c(0.1, 0.5, 1, 2, 3, 4) # Example tau values
colors <- c('#C9D4F6','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

# plot three values of tau beside each other
filepath <- file.path("plots", "illustrate_priors", "tau_sigmoid_curves.jpg")
tau_sigmoid_curves <- plot_sigmoid_curves(x_values, tau_values, colors, n_col = 3, n_rows = 2)
ggsave(filepath, tau_sigmoid_curves, width = 10, height = 6)

plot_tau_priors <- function(mean1, sd1, mean2, sd2) {
  # generate data for the first prior distribution
  logtaus1 <- rnorm(8000, mean1, sd1)
  taus1 <- exp(logtaus1)
  data1 <- data.frame(taus = taus1, prior = paste("Lognormal (", mean1, ", ", sd1, ")", sep = ""))
  
  # generate data for the second prior distribution
  logtaus2 <- rnorm(8000, mean2, sd2)
  taus2 <- exp(logtaus2)
  data2 <- data.frame(taus = taus2, prior = paste("Lognormal (", mean2, ", ", sd2, ")", sep = ""))
  
  # combine the data
  combined_data <- rbind(data1, data2)
  
  # plot
  plot <- ggplot(combined_data, aes(x = taus, fill = prior)) +
    geom_density(alpha = 0.4) +
    labs(title = "Prior distribution of tau",
         x = "tau",
         y = "Density") +
    xlim(0, 7) +
    scale_fill_manual(values = c("#0BDA51", "#FF0000")) +
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
tau_plot <- plot_tau_priors(0, 0.2, 0, 1)
ggsave(file.path("plots", "illustrate_priors", "tau_priors.jpg"), tau_plot, width = 10, height = 6)

plot_alpha_priors <- function() {
  # generate data for a uniform prior distribution
  alphas1 <- runif(8000, 0, 1)
  data1 <- data.frame(alphas = alphas1, prior = "Uniform (0, 1)")

  # generate data for a beta prior distribution
  alphas2 <- rbeta(8000, 2, 2)
  data2 <- data.frame(alphas = alphas2, prior = "Beta (2, 2)")
  
  # combine the data
  combined_data <- rbind(data1, data2)
  
  # plot
  plot <- ggplot(combined_data, aes(x = alphas, fill = prior)) +
    geom_density(alpha = 0.4) +
    labs(title = "Prior distribution of alpha",
         x = "alpha",
         y = "Density") +
    xlim(0, 1) +
    ylim(0, 2) +
    scale_fill_manual(values = c("#0BDA51", "#FF0000")) +
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

alpha_plot <- plot_alpha_priors()
ggsave(file.path("plots", "illustrate_priors", "alpha_priors.jpg"), alpha_plot, width = 10, height = 6)

# plot the tau and alpha prior on two seperate plots that are used for prior predictive checks
prior_type1 <- function() {
  # generate data for the first prior distribution
  logtaus1 <- rnorm(8000, 0, 1)
  taus1 <- exp(logtaus1)
  data1 <- data.frame(taus = taus1, prior = "Lognormal (0, 1)")
  
  # generate data for the second prior distribution
  alphas1 <- runif(8000, 0, 1)
  data2 <- data.frame(alphas = alphas1, prior = "Uniform (0, 1)")

  # plot logtaus1 
  plot1 <- ggplot(data1, aes(x = taus, fill = prior)) +
    geom_density(alpha = 0.5) +
    labs(title = "tau",
         x = "tau",
         y = "Density") +
    xlim(0, 7) +
    ylim(0, 2) +
    scale_fill_manual(values = c("#FF0000")) +
    theme_bw()+
    theme(legend.position="bottom",
          legend.title=element_blank(), 
          legend.key.size = unit(0.4, 'cm'), 
          legend.text = element_text(size = 10),
          legend.box.spacing = unit(5, "pt"),
          axis.text=element_text(size=10), 
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 12, hjust = 0.5))

    # plot alphas1
    plot2 <- ggplot(data2, aes(x = alphas, fill = prior)) +
    geom_density(alpha = 0.5) +
    labs(title = "alpha",
         x = "alpha",
         y = "Density") +
    xlim(0, 1) +
    ylim(0, 2) +
    scale_fill_manual(values = c("#FF0000")) +
    theme_bw()+
    theme(legend.position="bottom",
          legend.title=element_blank(), 
          legend.key.size = unit(0.4, 'cm'), 
          legend.text = element_text(size = 10),
          legend.box.spacing = unit(5, "pt"),
          axis.text=element_text(size=10), 
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 12, hjust = 0.5))

    # arrange
    final_plot <- ggarrange(plot1, plot2, ncol = 2, nrow = 1)

    final_plot <- ggpubr::annotate_figure(final_plot,top=text_grob("Prior distributions", size=14))

    return(final_plot)
}

prior_type1_plot <- prior_type1()
ggsave(file.path("plots", "illustrate_priors", "alpha_and_tau_prior1.jpg"), prior_type1_plot, width = 10, height = 6)
