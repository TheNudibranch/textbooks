#### M_1
p_grid <- seq(0,1, length.out = 100)
prior <- rep(1,length(p_grid))

posterior_1 <- dbinom(3,3, prob = p_grid) * prior
posterior_2 <- dbinom(3,4, prob = p_grid) * prior
posterior_3 <- dbinom(5,7, prob = p_grid) * prior
par(mfrow=c(1,3))
plot(p_grid, posterior_1)
plot(p_grid, posterior_2)
plot(p_grid, posterior_3)

#### H_1
# Panda just gave birth to twins
# What is probability that panda is A
p_t_a <- 0.1
p_t_b <- 0.2
prior <- c(1,1)
likelihood <- c(p_t_a, p_t_b)

posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

# Now, have twins again with new prior
prior <- posterior
posterior <- likelihood * prior

# The average likelihood is the sum of the posterior above
  # Or, which we are more interested in, the probability of have two sets of twins in a row, without knowing the species info
sum(posterior)



#### H_4
# Using the test
p_species_a_test_a <- 0.8
p_species_a_test_b <- 1-0.65
prior <- c(1,1)
likelihood <- c(p_species_a_test_a, p_species_a_test_b)

posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
posterior[1]

# Incorporate the other two births (twin and then singleton, though the order does not matter)

# Twin
posterior <- posterior * c(0.1, 0.2)
posterior <- posterior / sum(posterior)

# Not twins
posterior <- posterior * c(0.9, 0.8)
posterior <- posterior / sum(posterior)
posterior[1]
