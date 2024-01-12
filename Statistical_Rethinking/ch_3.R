##### Vampires
PrPV <- 0.95 # Probability of P(pos|vamp)
PrPM <- 0.01 # Probability of P(pos|mortal)
PrV <- 0.001

  # What is the probability that someone is a vamp of they test postive? P(vamp|positive)
likelihood <- c(PrPV, PrPM)
prior <- c(PrV, 1-PrV)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
posterior[1]



###### Sampling from the globe distribution
p_grid <- seq(0,1, length.out = 1000)
prior <- rep(1,length(p_grid))
likelihood <- dbinom(6,9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

  # Sample from the posterior
samples <- sample(p_grid, 1e5, replace=TRUE, prob = posterior)
plot(samples)
library(rethinking)
dens(samples)

  # What is the posterior probability that the proportion of water is less than 0.5
sum(posterior[p_grid<0.5])
  # About 17% of the posterior probability is less than 0.5

  # The same thing can be done with the sample
sum(samples < 0.5) / length(samples)

  # 50% confiedence interval (range for the middle 50% of the data)
PI(samples, 0.5)

  # Narrowest 50% confidence interval (should be relatively the same as above since the dist isn't very skewed)
HPDI(samples, 0.5)

  ### Calculated globe dist again, except for 3 tosses and 3 waters
p_grid <- seq(0,1, length.out = 1000)
prior <- rep(1,length(p_grid))
likelihood <- dbinom(3,3, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

  # Now, suppose d is the correct proportion, and we guess p
  # Then the loss is proportional to abs(d-p)
  # Calculate the expected loss for all values of p
loss <- sapply(p_grid, function(d) sum(posterior * (abs(d-p_grid))))
p_grid[which.min(loss)]
  # This is acutally the posterior median



######## Sampling to simulate prediction
dummy_w <- rbinom(1e5, size=9, prob=0.7)
simplehist(dummy_w)

  # Sampling from posterior distribution calcuatled above
w <- rbinom(1e5, size=9, prob=samples)
simplehist(w)
