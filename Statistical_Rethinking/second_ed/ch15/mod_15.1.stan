data {
  int<lower=0> N;
  vector[N] D_obs;
  vector<lower=0>[N] D_se;
  vector[N] A;
  vector[N] M;
}

parameters {
  vector[N] D_true;
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

  // measurement model
  D_obs ~ normal(D_true, D_se);

  // process model
  D_true ~ normal(alpha + beta_A * A + beta_M * M, sigma);
}
