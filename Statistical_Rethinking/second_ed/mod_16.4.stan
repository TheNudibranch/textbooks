functions {
  vector population_dynamics(real t, vector pop, array[] real theta,
                             array[] real x_r, array[] int x_i) {
    vector[2] dpop_dt;
    real L = pop[1];
    real H = pop[2];
    real b_H = theta[1];
    real m_H = theta[2];
    real m_L = theta[3];
    real b_L = theta[4];
    
    dpop_dt[1] = (b_L * H - m_L) * L;
    dpop_dt[2] = (b_H - m_H * L) * H;
    return dpop_dt;
  }
}
data {
  int<lower=1> N;
  array[N, 2] real<lower=0> pelts;
}
transformed data {
  array[N - 1] real times_measured;
  for (i in 1 : (N - 1)) {
    times_measured[i] = i + 1;
  }
}
parameters {
  real<lower=0> b_H;
  real<lower=0> b_L;
  real<lower=0> m_H;
  real<lower=0> m_L;
  vector<lower=0>[2] pop_init;
  vector<lower=0>[2] sigma;
  vector<lower=0, upper=1>[2] p;
}
transformed parameters {
  array[N] vector[2] pop;
  array[N - 1] vector[2] pop_ode;
  array[4] real theta = {b_H, m_H, m_L, b_L};
  
  pop[1] = pop_init;
  if (N > 1) {
    pop_ode = ode_rk45_tol(population_dynamics, pop_init, 1.0,
                           times_measured, 1e-5, 1e-3, 100000, theta,
                           rep_array(0.0, 0), rep_array(0, 0));
    for (i in 2 : N) {
      pop[i] = pop_ode[i - 1];
    }
  }
}
model {
  b_H ~ normal(1, 0.5);
  b_L ~ normal(0.05, 0.05);
  m_H ~ normal(0.05, 0.05);
  m_L ~ normal(1, 0.5);
  pop_init ~ lognormal(log(10), 1);
  sigma ~ exponential(1);
  p ~ beta(40, 200);
  
  for (t in 1 : N) {
    for (k in 1 : 2) {
      pelts[t, k] ~ lognormal(log(pop[t][k] * p[k]), sigma[k]);
    }
  }
}
generated quantities {
  array[N, 2] real pelts_pred;
  for (t in 1 : N) {
    for (k in 1 : 2) {
      pelts_pred[t, k] = lognormal_rng(log(pop[t][k] * p[k]), sigma[k]);
    }
  }
}
