// This Stan program is a result of a live coding session with Riccardo Fusaroli (Advanced Cognitive Modeling)

// data block is used to define model inputs
// note that if we would want to model several participnats, we would need another variable e.g., "int<lower=1> participants;" in addition to trials 
// and then our input and outcome would be a matrix
data {
  int<lower = 1> trials; // makes no sense that trials is below 0, so we define lower bound as 1. If it was a continous number, we would specify as "real<lower=0> trials;" and not "int<lower=0> trials;"
  array[trials] int choice; // outcome  NB. note to make previous choice from this
  // vector[trials - 1] prevChoice;
  vector[trials] feedback; // input 
  vector[2] initialValue; // expected value before starting
}

// arameters accepted by the model
parameters {
  real<lower = 0, upper = 1> learningRate; // boundaries of the learning rate is between 0 and 1 and its continuous (real number)
  //real<lower = 0.001> invTemperature; // to define values we should ideally run a bunch of simulations and see what values makes sense (e.g., ranging from 10 to 1000). Theoretically, we know that it cannot be 0.
  real logInvTemperature; // we have decided to use log of the inverse temperature to make it easier to define a prior for it
}

transformed parameters {
  real<lower = 0.001> invTemperature;
  invTemperature = exp(logInvTemperature);
}

// model to be estimated
model {
  // define model variables (vectors of size 2)
  vector[2] Value;
  vector[2] predError; 
  vector[2] rate;

  // defining priors 
  learningRate ~  beta(2, 2); // could figure out vals of a beta by typing in R console e.g., hist(rbeta(1000, 2, 2) where args are # n samples, alpha, beta)
  logInvTemperature ~ normal(0, 1);


  Value = initialValue; // we need to define the initial value (for the first trial)

  for (t in 1:trials){
    rate = softmax(invTemperature * Value);
    choice[t] ~ categorical(rate);
    
    predError = feedback[t] - Value; // prediction error
    Value = Value + learningRate * predError; 
  }
}

