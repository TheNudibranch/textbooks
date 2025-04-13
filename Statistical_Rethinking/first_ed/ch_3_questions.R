library(rethinking)
data(homeworkch3)
##### H1
p_grid <- seq(0, 1, length.out = 1000)
prior <- rep(1,length(p_grid))
likelihood <- dbinom(sum(birth1 + birth2), length(append(birth1, birth2)), prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior, type='l')

loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
p_grid[which.min(loss)]

#### H2
samples <- sample(p_grid, 1e5, replace=TRUE, prob = posterior)
HPDI(samples, prob=0.5)
HPDI(samples, prob=0.89)
HPDI(samples, prob=0.97)


#### H3
  # Simulate 10,0000 replicates of 200 births
b <- rbinom(1e4, length(append(birth1, birth2)), prob=samples)
dens(b)
abline(v = sum(birth1 + birth2), col='darkred')

#### H4
  # Compare 10,000 counts of boys from 100 simulated first borns only
  # To the number of boys in the first births: birth1
b_first <- rbinom(1e4, length(birth1), prob=samples)
dens(b_first)
abline(v = sum(birth1), col='darkred')
  # Model overestimates the number of boys for the first child

#### H5
  # Check the assumption of the model that the sex of the first and second child are independent

  # Focus on second births that followed females
birth1_fem <- birth1[birth1 == 0]
birth2_follow_fem <- birth2[birth1 == 0]

b_f_g <- rbinom(1e5, length(birth1_fem), prob=samples)
simplehist(b_f_g)
abline(v = sum(birth2_follow_fem), col='darkred')

median(b_f_g)
mean(b_f_g)
sum(birth2_follow_fem)
  # Model severely underestimates the number of boys
  # This is because the gender of the second child is not independent of the first