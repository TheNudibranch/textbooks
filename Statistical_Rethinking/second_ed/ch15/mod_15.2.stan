data {
  int<lower=0> N;
  vector[N] D_obs;
  vector<lower=0>[N] D_se;
  vector[N] M_obs;
  vector<lower=0>[N] M_se;
  vector[N] A;
}

parameters {
  vector[N] D_true;
  vector[N] M_true;
  real alpha;
  real beta_A;
  real beta_M;
  real<lower=0> sigma;
}

model {
  // priors
  alpha  ~ normal(0, 0.2);
  beta_A ~ normal(0, 0.5);
  beta_M ~ normal(0, 0.5);
  sigma  ~ exponential(1);
  M_true ~ normal(0, 1);

  // measurement models
  D_obs ~ normal(D_true, D_se);
  M_obs ~ normal(M_true, M_se);

  // process model
  D_true ~ normal(alpha + beta_A * A + beta_M * M_true, sigma);
}
