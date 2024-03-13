// stan implementation of reinforcement learning agent in matching pennies

data {
  int<lower=1> trials;                           // the number of trials
  int<lower= 0, upper = 1> onlyprior;            // integer indicating whether posterior sampling should be influenced by the prior only
  array[trials] int<lower=0, upper=1> choice;    // define choice as array of integers with lower bound 0 and upper bound 1
  array[trials] int<lower=0, upper=1> feedback;  // define feedback as array of integers with lower bound 0 and upper bound 1

  real priorSdTau;                               // the std deviation of the normal prior for log(tau)

  int<lower=0, upper=1> priorTypeAlpha;          // integer indicating which prior we use for alpha, 0 for uniform, 1 for beta
}

transformed data {
  real<lower=0, upper=1> initialValue;           // initiate value of the first trial
  initialValue = 0.5;                            // 0.5 is most appropriate
}
 
parameters {
  real<lower=0, upper=1> alpha;                  // learning rate, continous val between 0 and 1 
  real logTau;                                   // logTau, a transformation of tau to sample from an unbounded space
}

transformed parameters {
  real<lower=0> tau;                             // tau, continous with a lower bound of 0
  tau = exp(logTau);                             // define tau as the exponential of logTau
}

model {
  // def model variables
  array[trials] real value1;                     // 1d array to hold value of choice 1 (right)
  array[trials] real value2;                     // 1d array to hold value of choice 2 (left)
  real diff;                                     // the difference between the values
  real p;                                        // the probability of choosing 1 (right)

  // priors
  if (priorTypeAlpha == 0) {
    target += uniform_lpdf(alpha | 0, 1);        // add log probability of alpha according to uniform prior
  } else {
    target += beta_lpdf(alpha | 2, 2);           // add log probability of alpha according to beta prior
  }

  target += normal_lpdf(logTau | 0, priorSdTau); // add log probability of log tau according to normal prior. Note that priorSdTau is a hyperparameter that can be set by the user as a data input

  // likelihood
  if (!onlyprior) {                              // if onlyprior is 1 then the likelihood is not calculated

    value1[1] = initialValue;                    // set value at first trial to initial value
    value2[1] = initialValue;
 
    diff = value1[1] - value2[1];               // calculate value difference on first trial
    p = 1 / (1 + exp(-tau * diff));             // calculate probability of choosing 1 using the logistic function
    
    
    target += bernoulli_lpmf(choice[1] | p);    // add log-likelihood of choice on first trial

    for (t in 2:trials){

      value1[t] = value1[t-1] + alpha * choice[t-1] * (feedback[t-1] - value1[t-1]);        // update value 1
      value2[t] = value2[t-1] + alpha * (1 - choice[t-1]) * (feedback[t-1] - value2[t-1]);  // update value 2
      
      diff = value1[t] - value2[t];             // calculate value difference on remaining trials
      p = 1 / (1 + exp(-tau * diff));           // calculate probability of choosing 1 using the logistic function

      target += bernoulli_lpmf(choice[t] | p); // add log-likelihood of choices of remaining trials
    }
  }
}

generated quantities {
  real<lower=0, upper=1> alpha_prior;          // for generating a prior distribution of alpha
  real<lower=0> tau_prior;                     // for generating a prior distribution of tau

  array[trials] int<lower=0, upper=1> choice_pred; // 1d array to store predicted sequence of choices given a sampled set of parameters
  array[trials] real<lower=0, upper=1> value1; // 1d array to store values for the right hand
  array[trials] real<lower=0, upper=1> value2; // 1d array to store value for the left hand
  real diff;                                   // the difference between the values 
  real p;                                      // the probability of choosing 1 (right)

  if (priorTypeAlpha == 0) {                   // define prior dependent on priorTypealpha
    alpha_prior = uniform_rng(0, 1);           // sample alpha from a uniform prior
  } else {
    alpha_prior = beta_rng(2, 2);              // sample alpha from a beta prior
  }

  tau_prior = exp(normal_rng(0, 1));           // sample log tau from normal prior and transform to tau

  value1[1] = initialValue;                    // set value at first trial to initial value
  value2[1] = initialValue;

  diff = value1[1] - value2[1];                // calculate value difference on first trial
  p = 1 / (1 + exp(-tau * diff));              // calculate probability of choosing 1 using the logistic function
  choice_pred[1] = bernoulli_rng(p);           // sample choice on first trial from likelihood function

  // predict choice for remaining trials
  for (t in 2:trials){
    
    value1[t] = value1[t-1] + alpha * choice[t-1] * (feedback[t-1] - value1[t-1]);       // update value 1
    value2[t] = value2[t-1] + alpha * (1 - choice[t-1]) * (feedback[t-1] - value2[t-1]); // update value 2
    
    diff = value1[t] - value2[t];             // calculate value difference on remaining trials
    p = 1 / (1 + exp(-tau * diff));           // calculate probability of choosing 1 using the logistic function
    
    choice_pred[t] = bernoulli_rng(p);        // sample choice on remaining trials from likelihood function
  }
}
