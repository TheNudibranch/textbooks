data {
  int<lower=2> N;
  array[N, 2] real<lower=0> pelts;
}
parameters {
  vector<lower=0>[2] alpha;
  vector<lower=0>[2] beta_self;
  vector[2] beta_cross;
  vector<lower=0>[2] sigma;
}
model {
  alpha ~ lognormal(log(1), 1);
  beta_self ~ normal(0, 1);
  beta_cross ~ normal(0, 1);
  sigma ~ exponential(1);
  
  for (t in 2 : N) {
    real mu_lynx = alpha[1] + beta_self[1] * pelts[t - 1, 1]
                   + beta_cross[1] * pelts[t - 1, 2];
    real mu_hare = alpha[2] + beta_self[2] * pelts[t - 1, 2]
                   + beta_cross[2] * pelts[t - 1, 1];
    
    if (mu_lynx <= 0 || mu_hare <= 0) {
      target += negative_infinity();
    } else {
      pelts[t, 1] ~ lognormal(log(mu_lynx), sigma[1]);
      pelts[t, 2] ~ lognormal(log(mu_hare), sigma[2]);
    }
  }
}
generated quantities {
  array[N, 2] real pelts_pred;
  pelts_pred[1, 1] = pelts[1, 1];
  pelts_pred[1, 2] = pelts[1, 2];
  
  for (t in 2 : N) {
    real mu_lynx = alpha[1] + beta_self[1] * pelts[t - 1, 1]
                   + beta_cross[1] * pelts[t - 1, 2];
    real mu_hare = alpha[2] + beta_self[2] * pelts[t - 1, 2]
                   + beta_cross[2] * pelts[t - 1, 1];
    pelts_pred[t, 1] = lognormal_rng(log(mu_lynx), sigma[1]);
    pelts_pred[t, 2] = lognormal_rng(log(mu_hare), sigma[2]);
  }
}
