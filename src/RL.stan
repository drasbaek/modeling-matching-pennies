// Stan implementation of reinforcement learning agent in matching pennies

data {
  int<lower=1> trials;
  // define choice and feedback as arrays of integers with lower bound 0 and upper bound 1
  array[trials] int<lower=0, upper=1> choice; 
  array[trials] int<lower=0, upper=1> feedback;
  vector[2] initialValue;
}

// parameters 
parameters {
  real<lower=0, upper=1> alpha; // learning rate, continous val between 0 and 1
  real logTau; //
}

// transformed parameters
transformed parameters {
  real<lower=0.001> tau;
  tau = exp(logTau);
}

// model to be estimated
model {
  // def model variables
  vector[2] Value;
  vector[2] predError; 
  real<lower=0, upper=1> p;

  // priors 
  target += uniform_lpdf(alpha | 0, 1);
  target += normal_lpdf(logTau | 0, 1); 

  
  

}
