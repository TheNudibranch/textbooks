data {
  int<lower=0> N;
  int<lower=0> N_obs;
  int<lower=0> N_miss;
  array[N_obs]  int<lower=1, upper=N> obs_idx;
  array[N_miss] int<lower=1, upper=N> miss_idx;
  vector[N_obs] B_obs;
  vector[N] K;
  vector[N] log_M;
}

parameters {
  vector[N_miss] B_miss;
  real alpha;
  real beta_B;
  real beta_M;
  real<lower=0> sigma;
  real nu;
  real<lower=0> sigma_B;
}

transformed parameters {
  vector[N] B;
  B[obs_idx]  = B_obs;
  B[miss_idx] = B_miss;
}

model {
  // priors
  alpha   ~ normal(0, 0.5);
  beta_B  ~ normal(0, 0.5);
  beta_M  ~ normal(0, 0.5);
  sigma   ~ exponential(1);
  nu      ~ normal(0.5, 1);
  sigma_B ~ exponential(1);

  // imputation model — applies to all B (observed and imputed)
  B ~ normal(nu, sigma_B);

  // outcome model
  K ~ normal(alpha + beta_B * B + beta_M * log_M, sigma);
}
