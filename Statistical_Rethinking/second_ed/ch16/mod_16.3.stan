data {
  int<lower=1> N;
  array[N] int n;
  vector[N] age;
  vector[N] seconds;
}
parameters {
  real<lower=0> k;
  real<lower=0> phi;
  real<lower=0> theta;
}
model {
  k ~ lognormal(log(1), 0.1);
  phi ~ lognormal(log(2), 0.25);
  theta ~ lognormal(log(5), 0.25);
  vector[N] lam = seconds .* phi .* (1 - exp(-k .* age))^theta;
  target += poisson_lpmf(n | lam);
}
