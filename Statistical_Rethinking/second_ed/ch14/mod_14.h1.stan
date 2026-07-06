data {
  int<lower=1> N;
  int<lower=1> N_district;
  array[N] int<lower=1, upper=N_district> district_id;
  vector[N] urban;
  array[N] int<lower=0, upper=1> use_contraception;
}

parameters {
  vector[2] mu_ab;               // population means [intercept, slope]
  vector<lower=0>[2] sigma_district; // SDs for intercept and slope
  cholesky_factor_corr[2] L_Rho;
  matrix[2, N_district] z_district;  // raw offsets (non-centered)
}

transformed parameters {
  matrix[N_district, 2] ab_district;
  ab_district = (rep_matrix(mu_ab, N_district) +
                 diag_pre_multiply(sigma_district, L_Rho) * z_district)';
}

model {
  mu_ab[1] ~ normal(0, 1.5);
  mu_ab[2] ~ normal(0, 1);
  sigma_district ~ exponential(1);
  L_Rho ~ lkj_corr_cholesky(2);
  to_vector(z_district) ~ std_normal();

  vector[N] logit_p;
  for (i in 1:N) {
    logit_p[i] = ab_district[district_id[i], 1] +
                 ab_district[district_id[i], 2] * urban[i];
  }
  use_contraception ~ bernoulli_logit(logit_p);
}

generated quantities {
  matrix[2, 2] Rho;
  Rho = multiply_lower_tri_self_transpose(L_Rho);

  vector[N] log_lik;
  for (i in 1:N) {
    real logit_p_i = ab_district[district_id[i], 1] +
                     ab_district[district_id[i], 2] * urban[i];
    log_lik[i] = bernoulli_logit_lpmf(use_contraception[i] | logit_p_i);
  }
}