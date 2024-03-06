// stan implementation of reinforcement learning agent in matching pennies

data {
  int<lower=1> trials;
  int<lower= 0, upper = 1> onlyprior; // integer indicating whether posterior sampling should be influenced by the prior only
  // define choice and feedback as arrays of integers with lower bound 0 and upper bound 1
  array[trials] int<lower=0, upper=1> choice; 
  array[trials] int<lower=0, upper=1> feedback;
}

transformed data {
  real<lower=0, upper=1> initialValue;  // 0.5 is most appropriate
  initialValue = 0.5;
}

// parameters 
parameters {
  real<lower=0, upper=1> alpha; // learning rate, continous val between 0 and 1. Reparam: real logitAlpha;
  real logTau; //
}

// transformed parameters
transformed parameters {
  //real<lower=0, upper=1> alpha;  // For Reparam
  //alpha = inv_logit(logitAlpha); // For Reparam
  real<lower=0> tau;
  tau = exp(logTau);
}

// model to be estimated
model {
  // def model variables
  array[trials] real value1; // 1d array to hold value of choice 1 (right)
  array[trials] real value2; // 1d array to hold value of choice 2 (left)
  real diff;
  real p;

  // priors 
  target += uniform_lpdf(alpha | 0, 1); // Reparam: normal_lpdf(logitAlpha | 0, 10);
  target += normal_lpdf(logTau | 0, 1); 

  if (!onlyprior) { // if onlyprior is 1 then the likelihood is not calculated
    // likelihood
    // set initial values (first trial)
    value1[1] = initialValue;
    value2[1] = initialValue;

    // make choice on trial 1 
    diff = value1[1] - value2[1];
    p = inv_logit(-tau * diff);
    
    // add log-likelihood of choice on first trial to target
    target += bernoulli_lpmf(choice[1] | p);

    // add log-likelihood of choices of remaining trials
    for (t in 2:trials){

      value1[t] = value1[t-1] + alpha * choice[t-1] * (feedback[t-1] - value1[t-1]);
      value2[t] = value2[t-1] + alpha * (1 - choice[t-1]) * (feedback[t-1] - value2[t-1]);
      
      diff = value1[t] - value2[t];
      p = inv_logit(-tau * diff);

      target += bernoulli_lpmf(choice[t] | p);
    }
  }
}

generated quantities {
  real<lower=0, upper=1> alpha_prior; 
  real<lower=0> tau_prior;

  array[trials] int<lower=0, upper=1> choice_pred; // 1d array to hold predicted sequence of choices given a sampled set of parameters
  array[trials] real<lower=0, upper=1> value1; 
  array[trials] real<lower=0, upper=1> value2;
  real diff;
  real p;

  alpha_prior = uniform_rng(0, 1); // Reparam: alpha_prior = inv_logit(normal_rng(0, 10));
  tau_prior = exp(normal_rng(0, 1));

  value1[1] = initialValue;
  value2[1] = initialValue;

  // make choice on trial 1
  diff = value1[1] - value2[1];
  p = inv_logit(-tau * diff);
  choice_pred[1] = bernoulli_rng(p);

  // predict choice for remaining trials
  for (t in 2:trials){
    
    value1[t] = value1[t-1] + alpha * choice[t-1] * (feedback[t-1] - value1[t-1]);
    value2[t] = value2[t-1] + alpha * (1 - choice[t-1]) * (feedback[t-1] - value2[t-1]);
    
    diff = value1[t] - value2[t];
    p = inv_logit(-tau * diff);
    
    choice_pred[t] = bernoulli_rng(p);
  }
}
