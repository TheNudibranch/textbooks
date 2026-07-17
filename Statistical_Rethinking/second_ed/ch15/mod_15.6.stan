data {
  int<lower=0> N;
  vector[N] B;
  vector[N] K;
  vector[N] log_M;
}

parameters {
  real alpha;
  real beta_B;
  real beta_M;
  real<lower=0> sigma;
}

model {
  alpha  ~ normal(0, 0.5);
  beta_B ~ normal(0, 0.5);
  beta_M ~ normal(0, 0.5);
  sigma  ~ exponential(1);
  K ~ normal(alpha + beta_B * B + beta_M * log_M, sigma);
}
