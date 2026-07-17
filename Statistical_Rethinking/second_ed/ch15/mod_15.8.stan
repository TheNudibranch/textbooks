data {
  int<lower=0> N;
  array[N] int<lower=0>  notes;
  array[N] int           cat;    // observed 0/1; -9 sentinel when RC==1
  array[N] int<lower=0, upper=1> RC;
}

parameters {
  real a;
  real b;
  real<lower=0, upper=1> k;
}

model {
  // priors
  a ~ normal(0, 1);
  b ~ normal(0, 0.5);
  k ~ beta(2, 2);

  for (i in 1:N) {
    if (RC[i] == 0) {
      // cat is observed — standard Bernoulli + Poisson
      cat[i]   ~ bernoulli(k);
      notes[i] ~ poisson_log(a + b * cat[i]);
    } else {
      // cat is missing — marginalize over cat in {0, 1}
      target += log_sum_exp(
        log(k)       + poisson_log_lpmf(notes[i] | a + b),
        log(1 - k)   + poisson_log_lpmf(notes[i] | a)
      );
    }
  }
}

generated quantities {
  vector[N] p_cat;            // Pr(C_i = 1 | N_i)
  array[N] int cat_imputed;   // posterior draw of C_i

  for (i in 1:N) {
    if (RC[i] == 0) {
      // cat observed — posterior is degenerate
      p_cat[i]       = cat[i];
      cat_imputed[i] = cat[i];
    } else {
      // cat missing — apply Bayes' theorem in log space
      real lp1       = log(k)       + poisson_log_lpmf(notes[i] | a + b);
      real lp0       = log(1 - k)   + poisson_log_lpmf(notes[i] | a);
      p_cat[i]       = exp(lp1 - log_sum_exp(lp1, lp0));
      cat_imputed[i] = bernoulli_rng(p_cat[i]);
    }
  }
}
