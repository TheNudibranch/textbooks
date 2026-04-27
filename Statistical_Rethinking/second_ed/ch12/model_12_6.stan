data {
  int<lower=1> N;                 // number of observations
  int<lower=2> K;                 // number of ordinal categories for R (e.g. 7)
  array[N] int<lower=1,upper=K> R;

  array[N] int<lower=1,upper=8> E_id; // maps each obs to E in {1..7}

  vector[N] action;
  vector[N] contact;
  vector[N] intention;

  vector<lower=0>[7] alpha;       // Dirichlet concentration parameters
}

parameters {
  real bA;
  real bC;
  real bI;
  real bE;

  simplex[7] delta;               // as in ulam
  ordered[K-1] kappa;             // ordered cutpoints for ordered_logistic
}

transformed parameters {
  vector[8] cum_delta;            // cumulative sum used in linear predictor

  cum_delta[1] = 0;
  cum_delta[2:8] = cumulative_sum(delta);
}

model {
  // priors
  kappa ~ normal(0, 1.5);
  bA ~ normal(0, 1);
  bC ~ normal(0, 1);
  bI ~ normal(0, 1);
  bE ~ normal(0, 1);
  delta ~ dirichlet(alpha);

  // likelihood
  for (n in 1:N) {
    real phi_n =
      bE * cum_delta[E_id[n]]
      + bA * action[n]
      + bC * contact[n]
      + bI * intention[n];

    R[n] ~ ordered_logistic(phi_n, kappa);
  }
}

generated quantities {
  vector[N] phi;
  for (n in 1:N) {
    phi[n] =
      bE * cum_delta[E_id[n]]
      + bA * action[n]
      + bC * contact[n]
      + bI * intention[n];
  }
}
