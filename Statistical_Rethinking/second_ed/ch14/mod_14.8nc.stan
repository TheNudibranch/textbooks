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
  vector[n_societies] z;
}

transformed parameters {
  matrix[n_societies, n_societies] SIGMA;
  matrix[n_societies, n_societies] L_SIGMA;
  vector[n_societies] k;

  for (i in 1:n_societies) {
    for (j in i:n_societies) {
      SIGMA[i, j] = etasq * exp(-rhosq * square(Dmat[i, j]));
      SIGMA[j, i] = SIGMA[i, j];
    }
    SIGMA[i, i] += 0.01;
  }

  L_SIGMA = cholesky_decompose(SIGMA);
  k = L_SIGMA * z;
}

model {
  a ~ exponential(1);
  b ~ exponential(1);
  g ~ exponential(1);
  etasq ~ exponential(2);
  rhosq ~ exponential(0.5);

  z ~ std_normal();

  T ~ poisson_log(log(a) + b * log(P) - log(g) + k);
}
