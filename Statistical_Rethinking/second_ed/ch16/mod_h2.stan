data {
  int<lower=1> N;
  array[N] int n;
  vector[N] age;
  vector[N] seconds;
  array[N] int chimp;
}
parameters {
  real<lower=0> k;
  real<lower=0> theta;
  real<lower=0> phi_mean;
  real<lower=0> phi_sig;
  vector[N] phi_raw;
}
transformed parameters {
  vector[N] phi = phi_mean + phi_sig * phi_raw;
}
model {
  k ~ lognormal(log(1), 0.1);
  phi_mean ~ normal(log(2), 0.25);
  phi_sig ~ exponential(1);
  phi_raw ~ std_normal();
  theta ~ lognormal(log(5), 0.25);
  vector[N] lam = seconds .* exp(phi[chimp]) .* (1 - exp(-k .* age)) ^ theta;
  target += poisson_lpmf(n | lam);
}
