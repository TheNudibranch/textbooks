data {
  int<lower=0> N;
  array[N] int<lower=0, upper=10> H;
  vector[N] S;
}

parameters {
  real a;
  real bS;
}

model {
  a  ~ normal(0, 1);
  bS ~ normal(0, 0.5);
  H  ~ binomial_logit(10, a + bS * S);
}
