# script for plotting priors
logtaus <- rnorm(1000, 0, 1)
taus <- exp(logtaus)

# plot the prior
hist(taus, breaks=50, main="Prior distribution of tau", xlab="tau", col="lightblue", border="black")

x_values <- seq(-10, 10, length=100)
tau_test <- c(0.1, 0.2, 0.3, 0.4, 0.7, 1, 2, 5, 10)
for (tau in tau_test) {
  y_values <- c()
  for (x in x_values) {
    y <- 1 / (1 + exp(x*-tau))
    y_values <- c(y_values, y)
  }
  plot(x_values, y_values, type="l", main=paste("Sigmoid curve with tau=", tau), xlab="x", ylab="y")
}

# determining the space of value (set feedback to either 0 or 1)
n_trials <- 20
values <- c(0.5)
alpha <- 0.3
feedback <- 0
for (trial in 2:n_trials) {
  value <- values[trial-1] + alpha * (feedback - values[trial-1])
  values <- c(values, value)
}
plot(1:n_trials, values, type="l", main="Learning curve", xlab="Trial", ylab="Value")