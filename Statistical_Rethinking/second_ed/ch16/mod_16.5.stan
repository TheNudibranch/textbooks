data {
  int<lower=1> N;
  vector<lower=0>[N] w;
  vector<lower=0>[N] h;
}
parameters {
  real<lower=0, upper=1> p;
  real<lower=0> k;
  real<lower=0> sigma;
  real<lower=0> alpha;
}
model {
  vector[N] mu = log(3.141593 * k * square(p) * pow(h, alpha));
  w ~ lognormal(mu, sigma);
  p ~ beta(2, 18);
  k ~ exponential(0.5);
  sigma ~ exponential(1);
  alpha ~ normal(3, 1);
}
