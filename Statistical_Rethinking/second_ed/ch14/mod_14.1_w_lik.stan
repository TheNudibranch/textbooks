data {
  int<lower=1> N;
  int<lower=1> N_cafe;
  array[N] int<lower=1, upper=N_cafe> cafe;
  vector[N] afternoon;
  vector[N] wait;
}

parameters {
  vector[2] mu_ab;               
  vector<lower=0>[2] sigma_cafe; 
  cholesky_factor_corr[2] L_Rho; 
  real<lower=0> sigma;           
  matrix[2, N_cafe] z_cafe;      
}

transformed parameters {
  matrix[N_cafe, 2] ab_cafe;
  // Non-centered parameterization transformation
  ab_cafe = (rep_matrix(mu_ab, N_cafe) + 
             diag_pre_multiply(sigma_cafe, L_Rho) * z_cafe)';
}

model {
  mu_ab[1] ~ normal(5, 2);
  mu_ab[2] ~ normal(-1, 0.5);
  sigma_cafe ~ exponential(1);
  sigma ~ exponential(1);
  L_Rho ~ lkj_corr_cholesky(2);
  
  to_vector(z_cafe) ~ std_normal();

  vector[N] mu;
  for (i in 1:N) {
    mu[i] = ab_cafe[cafe[i], 1] + ab_cafe[cafe[i], 2] * afternoon[i];
  }
  wait ~ normal(mu, sigma);
}

generated quantities {
  // Recover the correlation matrix
  matrix[2, 2] Rho;
  Rho = multiply_lower_tri_self_transpose(L_Rho);

  // Pointwise log-likelihood for LOO-CV
  vector[N] log_lik;
  for (i in 1:N) {
    real mu_i = ab_cafe[cafe[i], 1] + ab_cafe[cafe[i], 2] * afternoon[i];
    log_lik[i] = normal_lpdf(wait[i] | mu_i, sigma);
  }
}
