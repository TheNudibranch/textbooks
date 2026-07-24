data {
  int<lower=1>  N_obs;
  array[N_obs]  int<lower=0>          x_obs;
  array[N_obs]  int<lower=1, upper=8> obs_idx;
  vector<lower=0>[8]                   alpha_dir;
  int<lower=0>                         M_max;   // upper bound on M = N - x_sum
}

transformed data {
  int x_sum = sum(x_obs);  // 120

  // Constant: -sum(log(x_i!)) for observed counts
  real log_obs_const = 0;
  for (i in 1:N_obs)
    log_obs_const -= lgamma(x_obs[i] + 1);
}

parameters {
  simplex[8]    theta;
  real<lower=0> lambda;   // Poisson prior mean for N
}

model {
  theta  ~ dirichlet(alpha_dir);
  lambda ~ gamma(2, 0.01);        // mean ~200, weakly informative

  // Observed log(theta) terms — constant across M, factor out of sum
  real log_obs_theta = 0;
  for (i in 1:N_obs)
    log_obs_theta += x_obs[i] * log(theta[obs_idx[i]]);

  real log_theta45 = log(theta[4] + theta[5]);

  // Single loop over M — multinomial theorem collapses (x4,x5) analytically
  // Σ_{x4+x5=M} Multinomial(...) = N!/M! * (θ4+θ5)^M / obs_facts
  vector[M_max + 1] log_terms;
  for (m in 0:M_max) {
    int N = x_sum + m;
    log_terms[m + 1] = poisson_lpmf(N | lambda)   // P(N)
                       + lgamma(N + 1)             // N!
                       - lgamma(m + 1)             // 1/M!
                       + m * log_theta45;          // (θ4+θ5)^M
  }

  target += log_obs_theta + log_obs_const + log_sum_exp(log_terms);
}

generated quantities {
  int M_draw;
  int x4_draw;
  int x5_draw;
  {
    real log_theta45 = log(theta[4] + theta[5]);
    vector[M_max + 1] log_probs;
    for (m in 0:M_max) {
      int N = x_sum + m;
      log_probs[m + 1] = poisson_lpmf(N | lambda)
                         + lgamma(N + 1)
                         - lgamma(m + 1)
                         + m * log_theta45;
    }
    // Draw total missing count M, then split between faces 4 and 5
    M_draw  = categorical_rng(softmax(log_probs)) - 1;
    x4_draw = binomial_rng(M_draw, theta[4] / (theta[4] + theta[5]));
    x5_draw = M_draw - x4_draw;
  }
}
