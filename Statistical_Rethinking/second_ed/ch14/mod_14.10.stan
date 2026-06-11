data {
  int<lower=1> N_spp;
  vector[N_spp] B;
  vector[N_spp] M;
  vector[N_spp] G;
  matrix[N_spp, N_spp] R;  // phylogenetic correlation matrix (V / max(V))
}

parameters {
  real a;
  real bM;
  real bG;
  real<lower=0> sigma_sq;
}

model {
  vector[N_spp] mu;
  matrix[N_spp, N_spp] SIGMA;

  a        ~ normal(0, 1);
  bM       ~ normal(0, 0.5);
  bG       ~ normal(0, 0.5);
  sigma_sq ~ exponential(1);

  mu = a + bM * M + bG * G;

  SIGMA = R * sigma_sq;

  B ~ multi_normal(mu, SIGMA);
}
