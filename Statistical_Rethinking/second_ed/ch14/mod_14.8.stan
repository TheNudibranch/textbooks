data {
  int<lower=1> n_societies;
  array[n_societies] int<lower=0> T;
  vector<lower=0>[n_societies] P;
  matrix<lower=0>[n_societies, n_societies] Dmat;
}

parameters {
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> g;
  real<lower=0> etasq;
  real<lower=0> rhosq;
  vector[n_societies] k;
}

transformed parameters {
  // cov_GPL2: squared exponential kernel with diagonal jitter for positive definiteness
  matrix[n_societies, n_societies] SIGMA;
  for (i in 1:n_societies) {
    for (j in i:n_societies) {
      SIGMA[i, j] = etasq * exp(-rhosq * square(Dmat[i, j]));
      SIGMA[j, i] = SIGMA[i, j];
    }
    SIGMA[i, i] += 0.01;
  }
}

model {
  // Priors
  a ~ exponential(1);
  b ~ exponential(1);
  g ~ exponential(1);
  etasq ~ exponential(2);
  rhosq ~ exponential(0.5);

  // GP prior on society-level spatial effects
  k ~ multi_normal(rep_vector(0, n_societies), SIGMA);

  // Likelihood: lambda = (a * P^b / g) * exp(k)
  //             log_lambda = log(a) + b*log(P) - log(g) + k
  T ~ poisson_log(log(a) + b * log(P) - log(g) + k);
}
