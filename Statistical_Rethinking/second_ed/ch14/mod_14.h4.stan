data {
  int<lower=1> N;
  int<lower=1> N_subject;
  array[N] int<lower=1, upper=N_subject> subject_id;
  vector[N] age;
  vector[N] height;
}

parameters {
  vector[2] mu_ab;
  vector<lower=0>[2] sigma_subject;
  cholesky_factor_corr[2] L_Rho;
  matrix[2, N_subject] z_subject;
  real<lower=0> sigma;
}

transformed parameters {
  matrix[N_subject, 2] ab_subject;
  ab_subject = (rep_matrix(mu_ab, N_subject) +
                diag_pre_multiply(sigma_subject, L_Rho) * z_subject)';
}

model {
  mu_ab[1] ~ normal(150, 20);
  mu_ab[2] ~ normal(0, 10);
  sigma_subject ~ exponential(1);
  sigma ~ exponential(1);
  L_Rho ~ lkj_corr_cholesky(2);
  to_vector(z_subject) ~ std_normal();

  vector[N] mu;
  for (i in 1:N) {
    mu[i] = ab_subject[subject_id[i], 1] +
            ab_subject[subject_id[i], 2] * age[i];
  }
  height ~ normal(mu, sigma);
}

generated quantities {
  matrix[2, 2] Rho;
  Rho = multiply_lower_tri_self_transpose(L_Rho);

  vector[N] log_lik;
  for (i in 1:N) {
    real mu_i = ab_subject[subject_id[i], 1] +
                ab_subject[subject_id[i], 2] * age[i];
    log_lik[i] = normal_lpdf(height[i] | mu_i, sigma);
  }
}
