// stan implementation of reinforcement learning agent in matching pennies

data {
  int<lower=1> trials;
  int<lower= 0, upper = 1> onlyprior; // integer indicating whether posterior sampling should be influenced by the prior only
  // define choice and feedback as arrays of integers with lower bound 0 and upper bound 1
  array[trials] int<lower=0, upper=1> choice; 
  array[trials] int<lower=0, upper=1> feedback;
}

transformed data {
  vector[2] initialValue;  // initial values (0.5, 0.5)
  initialValue = rep_vector(0.5, 2);
}

// parameters 
parameters {
  real<lower=0, upper=1> alpha; // learning rate, continous val between 0 and 1
  real logTau; //
}

// transformed parameters
transformed parameters {
  real<lower=0> tau;
  tau = exp(logTau);
}

// model to be estimated
model {
  // def model variables
  vector[2] Value;
  vector[2] PrevCorrectChoice;
  real diff;
  real p;

  // priors 
  target += uniform_lpdf(alpha | 0, 1);
  target += normal_lpdf(logTau | 0, 1); 

  if (!onlyprior) { // if onlyprior is 1 then the likelihood is not calculated
    // likelihood
    // set initial value of Value (first trial)
    Value = initialValue;

    for (t in 2:trials){
      // define previous correct choice  
      if (feedback[t-1] == 1) {
        PrevCorrectChoice = transpose([choice[t-1], 1 - choice[t-1]]); // if feedback is 1, then previous correct choice is the same 
      }
      else {
        PrevCorrectChoice = transpose([1 - choice[t-1], choice[t-1]]); // if feedback is 0, then previous correct choice is the opposite
      }

      // update values
      Value = (1-alpha) * Value + alpha * PrevCorrectChoice;

      // calculate probability
      diff = Value[1] - Value[2]; // difference in value of choices
      p = inv_logit(-tau * diff);
      
      // make choice and define likelihood (add log-likelihood to target)
      target += bernoulli_lpmf(choice[t] | p);
    }
  }
}

generated quantities {

  array[trials] int<lower=0, upper=1> choice_pred; // 1d array to hold predicted sequence of choices given a sampled set of parameters
  vector[2] Value;
  vector[2] PrevCorrectChoice;
  real diff;
  real p;

  Value = initialValue;

  // make choice on trial 1
  diff = Value[1] - Value[2];
  p = inv_logit(-tau * diff);
  choice_pred[1] = bernoulli_rng(p);

  // predict choice for remaining trials
  for (t in 2:trials){
    if (feedback[t-1] == 1) {
      PrevCorrectChoice = transpose([choice[t-1], 1 - choice[t-1]]); 
    }
    else {
      PrevCorrectChoice = transpose([1 - choice[t-1], choice[t-1]]);
    }

    Value = (1-alpha) * Value + alpha * PrevCorrectChoice;

    diff = Value[1] - Value[2];
    p = inv_logit(-tau * diff);
    
    choice_pred[t] = bernoulli_rng(p);
  }
}
