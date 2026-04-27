data {
  int<lower=1> N;                 // number of observations
  int<lower=2> K;                 // number of ordinal categories for R (e.g. 7)
  array[N] int<lower=1,upper=K> R;


  vector[N] action;
  vector[N] contact;
  vector[N] intention;
  
  int<lower=1> n_unq_id;
  array[N] int<lower=1> id;
}

parameters {
  real bA;
  real bC;
  real bI;

  ordered[K-1] kappa;             // ordered cutpoints for ordered_logistic
  
  real<lower=0> sig_id;
  vector[n_unq_id] beta_raw;
}

transformed parameters {
  vector[n_unq_id] beta = beta_raw * sig_id;
}


model {
  // priors
  kappa ~ normal(0, 1.5);
  bA ~ normal(0, 1);
  bC ~ normal(0, 1);
  bI ~ normal(0, 1);
  beta_raw ~ std_normal();
  sig_id ~ exponential(1);

  // likelihood
  for (n in 1:N) {
    real phi_n =
      bA * action[n]
      + bC * contact[n]
      + bI * intention[n]
      + beta[id[n]];

    R[n] ~ ordered_logistic(phi_n, kappa);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N){
    real phi_n =
      bA * action[i]
      + bC * contact[i]
      + bI * intention[i]
      + beta[id[i]];
    log_lik[i] =  ordered_logistic_lpmf(R[i] | phi_n, kappa);
  }
}
