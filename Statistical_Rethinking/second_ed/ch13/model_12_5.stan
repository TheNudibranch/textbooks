data {
  int<lower=1> N;                 // number of observations
  int<lower=2> K;                 // number of ordinal categories for R (e.g. 7)
  array[N] int<lower=1,upper=K> R;


  vector[N] action;
  vector[N] contact;
  vector[N] intention;

}

parameters {
  real bA;
  real bC;
  real bI;

  ordered[K-1] kappa;             // ordered cutpoints for ordered_logistic
}


model {
  // priors
  kappa ~ normal(0, 1.5);
  bA ~ normal(0, 1);
  bC ~ normal(0, 1);
  bI ~ normal(0, 1);

  // likelihood
  for (n in 1:N) {
    real phi_n =
      bA * action[n]
      + bC * contact[n]
      + bI * intention[n];

    R[n] ~ ordered_logistic(phi_n, kappa);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N){
    real phi_n =
      bA * action[i]
      + bC * contact[i]
      + bI * intention[i];
    log_lik[i] =  ordered_logistic_lpmf(R[i] | phi_n, kappa);
  }
}
