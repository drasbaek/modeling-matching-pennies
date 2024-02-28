// stan implementation of reinforcement learning agent in matching pennies

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
  vector[2] PrevCorrectChoice;
  real<lower=0, upper=1> p;

  // priors 
  target += uniform_lpdf(alpha | 0, 1);
  target += normal_lpdf(logTau | 0, 1); 

  // set initial value of Value (first trial)
  Value = initValue;

  for (t in 2:trials){
    // define previous correct choice  
    if (feedback[t] == 1) {
      PrevCorrectChoice = [choice[t-1], 1 - choice[t-1]] // if feedback is 1, then previous correct choice is the same 
    }
    else {
      PrevCorrectChoice = [1 - choice[t-1], choice[t-1]] // if feedback is 0, then previous correct choice is the opposite
    }

    // update values
    Value[t] = (1-alpha) * Value[t-1] + alpha * PrevCorrectChoice

    // calculate probability
    diff = Value[1] - Value[2]; // difference in value of choices
    p = softmax(diff * tau)
    
    // make choice and define likelihood (add log-likelihood to target)
    target += bernoulli_lpmf(choice[t] | p);
  }
}
