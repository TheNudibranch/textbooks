data {
  int<lower=1> n;
  int<lower=1> n_households;
  
  // Updated to modern Stan array syntax and added bounds for safety
  array[n] int<lower=1, upper=n_households> hidA;
  array[n] int<lower=1, upper=n_households> hidB;
  array[n] int<lower=0> giftsAB;
  array[n] int<lower=0> giftsBA;
  array[n] int<lower=1, upper=n> did;
}

parameters {
  real a;
  
  // Non-centered parameterization for 'gr' (households)
  matrix[2, n_households] z_gr;
  cholesky_factor_corr[2] L_Rho_gr;
  vector<lower=0>[2] sigma_gr;
  
  // Non-centered parameterization for 'd' (dyads)
  matrix[2, n] z_d; 
  cholesky_factor_corr[2] L_Rho_d;
  real<lower=0> sigma_d;
}

transformed parameters {
  // Transformed to matrices for easier extraction in the model block
  matrix[n_households, 2] gr;
  matrix[n, 2] d;
  
  // Apply the Cholesky factor and scales to standard normals, then transpose
  gr = (diag_pre_multiply(sigma_gr, L_Rho_gr) * z_gr)';
  d = (diag_pre_multiply(rep_vector(sigma_d, 2), L_Rho_d) * z_d)';
}

model {
  // Linear predictors on the log scale
  vector[n] log_lambdaAB;
  vector[n] log_lambdaBA;
  
  // Priors
  a ~ std_normal(); // std_normal() is optimized under the hood in Stan
  
  // NCP Priors for 'gr'
  to_vector(z_gr) ~ std_normal();
  sigma_gr ~ exponential(1);
  L_Rho_gr ~ lkj_corr_cholesky(4);
  
  // NCP Priors for 'd'
  to_vector(z_d) ~ std_normal();
  sigma_d ~ exponential(1);
  L_Rho_d ~ lkj_corr_cholesky(8);
  
  // Build the linear predictors
  for (i in 1:n) {
    log_lambdaBA[i] = a + gr[hidB[i], 1] + gr[hidA[i], 2] + d[did[i], 2];
    log_lambdaAB[i] = a + gr[hidA[i], 1] + gr[hidB[i], 2] + d[did[i], 1];
  }
  
  // Likelihood using poisson_log (avoids manual exp() for better stability)
  giftsBA ~ poisson_log(log_lambdaBA);
  giftsAB ~ poisson_log(log_lambdaAB);
}

generated quantities {
  // Recover the original correlation matrices if you need to analyze them
  corr_matrix[2] Rho_gr = multiply_lower_tri_self_transpose(L_Rho_gr);
  corr_matrix[2] Rho_d = multiply_lower_tri_self_transpose(L_Rho_d);
}
