data {
  int<lower=1> N_spp;
  vector[N_spp] B;
  vector[N_spp] M;
  vector[N_spp] G;
  matrix<lower=0>[N_spp, N_spp] Dmat;  // phylogenetic distance matrix (scaled)
}

parameters {
  real a;
  real bM;
  real bG;
  real<lower=0> etasq;
  real<lower=0> rhosq;
}

transformed parameters {
  matrix[N_spp, N_spp] SIGMA;

  // cov_GPL1: OU kernel — linear distance, not squared
  for (i in 1:N_spp) {
    for (j in i:N_spp) {
      SIGMA[i, j] = etasq * exp(-rhosq * Dmat[i, j]);
      SIGMA[j, i] = SIGMA[i, j];
    }
    SIGMA[i, i] += 0.01;
  }
}

model {
  vector[N_spp] mu;

  a    ~ normal(0, 1);
  bM   ~ normal(0, 0.5);
  bG   ~ normal(0, 0.5);
  etasq ~ normal(1, 0.25);   // half_normal via <lower=0> constraint on parameter
  rhosq ~ normal(3, 0.25);

  mu = a + bM * M + bG * G;

  B ~ multi_normal(mu, SIGMA);
}
