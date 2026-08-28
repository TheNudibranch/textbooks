data {
  int<lower=1> N;
  array[N] int<lower=1, upper=3> y;
  array[N] int<lower=0, upper=1> majority_first;
}
parameters {
  simplex[5] p;
}
model {
  p ~ dirichlet(rep_vector(4, 5));
  
  for (i in 1 : N) {
    vector[5] log_strategy_prob;
    
    log_strategy_prob = rep_vector(negative_infinity(), 5);
    
    // Follow the majority.
    if (y[i] == 2) 
      log_strategy_prob[1] = 0;
    
    // Follow the minority.
    if (y[i] == 3) 
      log_strategy_prob[2] = 0;
    
    // Choose the color no demonstrator chose.
    if (y[i] == 1) 
      log_strategy_prob[3] = 0;
    
    // Choose randomly.
    log_strategy_prob[4] = log(1.0 / 3.0);
    
    // Follow the first demonstrator.
    if (majority_first[i] == 1 && y[i] == 2) 
      log_strategy_prob[5] = 0;
    if (majority_first[i] == 0 && y[i] == 3) 
      log_strategy_prob[5] = 0;
    
    target += log_sum_exp(log(p) + log_strategy_prob);
  }
}
