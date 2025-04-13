#Example code from Ch.2

######## Grid Approximation

# Define grid
p_grid <- seq(0,1,length.out = 20)

# Define prior
##prior <- rep(0.5,length(p_grid))
##prior <- ifelse(p_grid < 0.5, 0, 1) # Figure 2.5
prior <- exp(-5 * abs(p_grid - 0.5))

# Compute the likelihood at each value in the grid
likelihood <- dbinom(6, size=9, prob=p_grid)

# Compute the product of the liklihood and prior
unstd_posterior <- likelihood * prior

# Standardize the posterior
posterior <- unstd_posterior / sum(unstd_posterior)
plot(p_grid, posterior, type='b', xlab='probability of water', ylab='posterior probability')
mtext('20 points')
points(p_grid, prior / sum(prior), type='l', lwd=3, lty=2)

######## Quadratic Approximation
library(rethinking)
globa_qa <- map(
  alist(
    w ~ dbinom(9,p), # Binomial Likelihood
    p ~ dunif(0,1) # Uniform prior
  ),
  data = list(w=6) # Number of water values
)

# display summary of quadratic approximation
precis(globa_qa)
# Read output as: "Assuimg the posterior is gaussian, it is maximized at 0.67, and its standard deviation is 0.16"

# Now, find the analytical solution
  # We will be using a beta distribution for the posterior of p (proprotion of water)
  # Explanation for this is found here: https://www2.stat.duke.edu/courses/Spring12/sta104.1/Lectures/Lec23.pdf

# analytical solution
w <- 6
n <- 9

curve(dbeta(x ,w+1, n-w+1))
curve(dnorm(x, 0.67, 0.16), lty=2, add=T)
