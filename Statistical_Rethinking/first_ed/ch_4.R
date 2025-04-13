library(rethinking)

#### Why distributions are normal
  ## Normal by multiplication
growth <- replicate(1e4, prod(1 + runif(12,0,0.1)))
dens(growth)

  # Normal by multiplication works best for smaller effects
big <- replicate(1e4, prod(1 + runif(12,0,0.5)))
small <- replicate(1e4, prod(1 + runif(12,0,0.01)))
par(mfrow=c(1,2))
dens(big)
dens(small)
  # Clearly the bigger one is right skewed

  ## Normal by log multiplication
log_big <- replicate(1e4, log(prod(1 + runif(12,0,0.5))))
par(mfrow=c(1,1))
dens(log_big)



#### Gaussian model of height
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]
  ## Going back and decreasing the number of samples for height to get show a bigger right tail on sigma
d2 <- d2[sample(1:nrow(d2), 20),]
dens(d2$height)

  # Set prior for mu to be normal centered at 178
curve(dnorm(x, 178, 20), 100, 250)
curve(dunif(x,0,50), -10,60)

sample_mu <- rnorm(100, 178,20)
sample_sig <- runif(length(sample_mu), 0, 50)
prior_h <- rnorm(length(sample_mu), sample_mu, sample_sig)
dens(prior_h)

  # Grid approximation for posterior
mu_list <- seq(140, 160, length.out = 200)
sig_list <- seq(4,9,length.out = length(mu_list))
post <- expand.grid(mu=mu_list, sigma=sig_list)
  # Do everything in the log scale so there are no rounding errors
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(d2$height, mean = post$mu[i], sd = post$sigma[i], log=TRUE)))
  # Adding since we are in log scale
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod  - max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

  # Sampling from posterior
sample_rows <- sample(1:nrow(post), size=1e4, replace = T, prob=post$prob)
sample_mu <- post$mu[sample_rows]
sample_sig <- post$sigma[sample_rows]
plot(sample_mu, sample_sig, cex=0.5, pch=16, col=col.alpha(rangi2,0.1))

par(mfrow=c(1,2))
dens(sample_mu, main='mu')
dens(sample_sig, main='sigma', norm.comp = T)
par(mfrow=c(1,1))

  #### Same thing but with MAP
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4_1 <- map(flist, data=d2)
precis(m4_1)

m4_2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ),
  data=d2
)
precis(m4_2)

  # Covariance among all parameters
vcov(m4_1)
diag(vcov(m4_1))
cov2cor(vcov(m4_1))
  
  # Sample from posterior
post <- extract.samples(m4_1, 1e4)
head(post)
precis(post)
plot(post,cex=0.5, pch=16, col=col.alpha(rangi2,0.1))



######## Adding a predictor
plot(d2$height~ d2$weight)

m4_3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0,50)
  ),
  data=d2
)
precis(m4_3, corr=T)
cov2cor(vcov(m4_3))
  ## Notice the high correlation between a and b
  ## To reduce this, we will center the weight

d2$weight_c <- d2$weight - mean(d2$weight)
m4_4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight_c,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0,50)
  ),
  data=d2
)
precis(m4_4)
cov2cor(vcov(m4_4))
    ## Notice, there is now no correlation

  ## Now, superimpose the MAP values over the weight and height of the data
plot(height ~ weight, data=d2)
abline(a=coef(m4_3)['a'], b=coef(m4_3)['b'])


  ## Adding uncertainty to the mean
  # Easier to show with sample of values from d
par(mfrow=c(2,2))
N <- 352
dn <- d[sample(1:nrow(d), N),]
mN <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data=dn
)
post <- extract.samples(mN, n=20)
plot(dn$weight, dn$height, xlim=(range(d2$weight)), ylim=range(d2$height), col=rangi2, xlab="weight", ylab='height')
mtext(concat("N = ", N))

  # Plot lines with transparency
for (i in 1:20){
  abline(a=post$a[i], b=post$b[i], col=col.alpha('black', 0.3))
}


  ## Plotting uncertainty around mean (regression intervals and contours)
weight_seq <- seq(25,70,1)

  # Use link to compute the posterior for each value of weight
mu <- link(m4_3, data=data.frame(weight=weight_seq))
par(mfrow=c(1,1))

plot(height ~ weight, data=d2, type='n') # Use n for no plotting
for (i in 1:100){
  points(weight_seq, mu[i,], pch=16, col=col.alpha(rangi2,0.1))
}

  # summarize distribution for each weight value
mu_mean <- apply(mu,2,mean)
mu_HDPI <- apply(mu,2,HPDI, prob=0.89)

plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
lines(weight_seq, mu_mean)
shade(mu_HDPI, weight_seq)

  ## Prediction intervals (we just had the mean, now lets incorporate the sd as well)
  # simulate some height values
sim_height <- sim(m4_3, data=list(weight=weight_seq))
height_pi <- apply(sim_height, 2, PI, prob=0.89)

plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
lines(weight_seq, mu_mean)
shade(mu_HDPI, weight_seq)
shade(height_pi, weight_seq)


######## Polynomial Regression

  #Standardize weight
  # Notice, here we are using the whole dataset instead of the only >= 18
d$weight_s <- (d$weight - mean(d$weight)) /sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4_5 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight_s + b2*weight_s2,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data=d
)
precis(m4_5)

weight_seq <- seq(-2.2, 2, length.out = 30)
pred_dat <- list(weight_s=weight_seq, weight_s2=weight_seq^2)
mu <- link(m4_5, data=pred_dat)
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI, prob=0.89)
sim_height <- sim(m4_5, data=pred_dat)
height_pi <- apply(sim_height, 2, PI, prob=0.89)

  # Plotting the above
plot(height ~ weight_s, d, col=col.alpha(rangi2, 0.5))
lines(weight_seq, mu_mean)
shade(mu_PI, weight_seq)
shade(height_pi, weight_seq)

  # Converting back to natural scale
plot(height ~ weight_s, d, col=col.alpha(rangi2, 0.5), xaxt='n')
at <- c(-2:2)
labels <- at * sd(d$weight) + mean(d$weight)
axis(side=1, at=at, labels=round(labels, 1))
