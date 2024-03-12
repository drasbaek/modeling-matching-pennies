# script for plotting priors
pacman::p_load(tidyverse)

# plot prior (0, 1)
logtaus <- rnorm(1000, 0, 1)
taus <- exp(logtaus)
hist(taus, breaks=50, main="Prior distribution of tau", xlab="tau", col="lightblue", border="black")

# plot prior (0, 0.2)
logtaus <- rnorm(1000, 0, 0.2)
taus <- exp(logtaus)
hist(taus, breaks=50, main="Prior distribution of tau", xlab="tau", col="lightblue", border="black")

plot_sigmoid_curves <- function(x_values, tau_values, colors, num_to_plot, filename) {
  plots <- list()
  
  for (i in 1:num_to_plot) {
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
  final_plot <- do.call(grid.arrange, c(plots, ncol = 3, nrow = 2))
  ggsave(filename, final_plot)
}

# Define x_values and tau_values
x_values <- seq(-10, 10, length=100)
tau_values <- c(0.1, 0.5, 1, 2, 3, 4) # Example tau values
colors <- c('#C9D4F6','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c')

# Plot three values of tau beside each other
filepath <- file.path("plots", "tau_sigmoid_curves.jpg")
plot_sigmoid_curves(x_values, tau_values, colors, length(tau_values), filepath)