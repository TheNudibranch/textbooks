data {
  int<lower=1> N;
  int<lower=1> N_cafe;
  array[N] int<lower=1, upper=N_cafe> cafe;
  vector[N] afternoon;
  vector[N] wait;
}

parameters {
  real mu_a;
  real mu_b;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma;
  vector[N_cafe] z_a;
  vector[N_cafe] z_b;
}

transformed parameters {
  vector[N_cafe] a_cafe = mu_a + sigma_a * z_a;
  vector[N_cafe] b_cafe = mu_b + sigma_b * z_b;
}

model {
  mu_a ~ normal(0, 10);
  mu_b ~ normal(0, 10);
  sigma_a ~ exponential(1);
  sigma_b ~ exponential(1);
  sigma ~ exponential(1);

  z_a ~ std_normal();
  z_b ~ std_normal();

  vector[N] mu;
  for (i in 1:N) {
    mu[i] = a_cafe[cafe[i]] + b_cafe[cafe[i]] * afternoon[i];
  }
  wait ~ normal(mu, sigma);
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    real mu_i = a_cafe[cafe[i]] + b_cafe[cafe[i]] * afternoon[i];
    log_lik[i] = normal_lpdf(wait[i] | mu_i, sigma);
  }
}