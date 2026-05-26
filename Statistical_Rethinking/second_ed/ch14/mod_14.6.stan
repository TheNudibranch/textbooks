data {
  int<lower=1> N;
  vector[N] W;
  vector[N] E;
  vector[N] Q;
}

parameters {
  real aW;
  real aE;
  real bEW;
  real bQE;
  cholesky_factor_corr[2] L_Rho;
  vector<lower=0>[2] Sigma;
}

transformed parameters {
  matrix[2, 2] L_Sigma;
  L_Sigma = diag_pre_multiply(Sigma, L_Rho);
}

model {
  // Priors
  aW ~ normal(0, 0.2);
  aE ~ normal(0, 0.2);
  bEW ~ normal(0, 0.5);
  bQE ~ normal(0, 0.5);
  L_Rho ~ lkj_corr_cholesky(2);
  Sigma ~ exponential(1);

  // Likelihood
  array[N] vector[2] Y;
  array[N] vector[2] mu;
  for (i in 1:N) {
    Y[i][1] = W[i];
    Y[i][2] = E[i];
    mu[i][1] = aW + bEW * E[i];
    mu[i][2] = aE + bQE * Q[i];
  }
  Y ~ multi_normal_cholesky(mu, L_Sigma);
}

generated quantities {
  matrix[2, 2] Rho;
  Rho = multiply_lower_tri_self_transpose(L_Rho);

  vector[N] log_lik;
  for (n in 1:N) {
    vector[2] Y_n;
    vector[2] mu_n;
    Y_n[1] = W[n];
    Y_n[2] = E[n];
    mu_n[1] = aW + bEW * E[n];
    mu_n[2] = aE + bQE * Q[n];
    log_lik[n] = multi_normal_cholesky_lpdf(Y_n | mu_n, L_Sigma);
  }
}
