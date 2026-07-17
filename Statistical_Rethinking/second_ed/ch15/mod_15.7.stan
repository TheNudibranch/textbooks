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

transformed data {
  vector[N_obs]  log_M_obs  = log_M[obs_idx];
  vector[N_miss] log_M_miss = log_M[miss_idx];
}

parameters {
  vector[N_miss] z_miss;          // non-centered standard normals for B_miss
  real alpha;
  real beta_B;
  real beta_M;
  real<lower=0> sigma;
  vector[2] mu_BM;
  cholesky_factor_corr[2] L_Rho;
  vector<lower=0>[2] sigma_BM;
}

transformed parameters {
  vector[N_miss] B_miss;
  {
    real rho        = L_Rho[2, 1];
    real sigma_cond = sigma_BM[1] * sqrt(1 - square(rho));
    vector[N_miss] mu_cond = mu_BM[1]
                             + rho * (sigma_BM[1] / sigma_BM[2])
                             * (log_M_miss - mu_BM[2]);
    B_miss = mu_cond + sigma_cond * z_miss;
  }
}

model {
  // hyperpriors
  mu_BM    ~ normal(0, 0.5);
  L_Rho    ~ lkj_corr_cholesky(2);
  sigma_BM ~ exponential(1);
  alpha    ~ normal(0, 0.5);
  beta_B   ~ normal(0, 0.5);
  beta_M   ~ normal(0, 0.5);
  sigma    ~ exponential(1);

  // MVN as prior — non-centered conditional for missing B given log_M
  z_miss ~ normal(0, 1);

  // MVN as likelihood — observed [B, log_M] pairs
  {
    matrix[N_obs, 2] BM_obs;
    BM_obs[, 1] = B_obs;
    BM_obs[, 2] = log_M_obs;
    for (i in 1:N_obs)
      target += multi_normal_cholesky_lupdf(BM_obs[i] | mu_BM, diag_pre_multiply(sigma_BM, L_Rho));
  }

  // outcome likelihood — index directly, no full B vector needed
  {
    vector[N] mu;
    mu[obs_idx]  = alpha + beta_B * B_obs  + beta_M * log_M_obs;
    mu[miss_idx] = alpha + beta_B * B_miss + beta_M * log_M_miss;
    K ~ normal(mu, sigma);
  }
}

generated quantities {
  matrix[2, 2] Rho = multiply_lower_tri_self_transpose(L_Rho);
}
