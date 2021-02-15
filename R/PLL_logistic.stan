data {
  int<lower=0> N; // Number of observations
  int<lower=1> P; //Dimension of fixed effect coefficients
  int<lower=0, upper=1> Y[N]; // Responses
  vector[N] wgt;  // weights
  matrix[N, P] X; // Fixed effect model matrix
}
parameters {
  vector[P] beta; // fixed effects
}
model {
  vector[N] linPred;
  linPred = X*beta;
  for(n in 1:N){
    target += wgt[n] * bernoulli_logit_lpmf(Y[n]|linPred[n]); 
  }
  for(b in 1:P){
    beta[b] ~ normal(0,sqrt(10));
  }
}
