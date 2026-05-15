data {
  int<lower=1> N;
  int<lower=1> N_actor;
  int<lower=1> N_block;
  int<lower=1> N_tid; // Usually 4
  array[N] int<lower=1, upper=N_actor> actor;
  array[N] int<lower=1, upper=N_block> block_id;
  array[N] int<lower=1, upper=N_tid> tid;
  array[N] int<lower=0, upper=1> L;
}

parameters {
  vector[N_tid] g;                 // Fixed intercepts per treatment
  
  // Actor Varying Effects
  vector<lower=0>[N_tid] sigma_actor;
  cholesky_factor_corr[N_tid] L_Rho_actor;
  matrix[N_tid, N_actor] z_actor;  // Standardized actor effects
  
  // Block Varying Effects
  vector<lower=0>[N_tid] sigma_block;
  cholesky_factor_corr[N_tid] L_Rho_block;
  matrix[N_tid, N_block] z_block;  // Standardized block effects
}

transformed parameters {
  // Non-centered reconstruction: scale and correlate the z-scores
  // We transpose at the end to get [ID, Treatment] indexing
  matrix[N_actor, N_tid] alpha;
  matrix[N_block, N_tid] beta;

  alpha = (diag_pre_multiply(sigma_actor, L_Rho_actor) * z_actor)';
  beta = (diag_pre_multiply(sigma_block, L_Rho_block) * z_block)';
}

model {
  // Fixed Priors
  g ~ normal(0, 1);
  sigma_actor ~ exponential(1);
  sigma_block ~ exponential(1);
  L_Rho_actor ~ lkj_corr_cholesky(4);
  L_Rho_block ~ lkj_corr_cholesky(4);

  // Non-centered priors (unit normals)
  to_vector(z_actor) ~ std_normal();
  to_vector(z_block) ~ std_normal();

  // Likelihood
  vector[N] logit_p;
  for (i in 1:N) {
    logit_p[i] = g[tid[i]] + alpha[actor[i], tid[i]] + beta[block_id[i], tid[i]];
  }
  L ~ bernoulli_logit(logit_p);
}

generated quantities {
  // Convert Cholesky factors back to correlation matrices for inspection
  matrix[N_tid, N_tid] Rho_actor;
  matrix[N_tid, N_tid] Rho_block;
  Rho_actor = multiply_lower_tri_self_transpose(L_Rho_actor);
  Rho_block = multiply_lower_tri_self_transpose(L_Rho_block);
}