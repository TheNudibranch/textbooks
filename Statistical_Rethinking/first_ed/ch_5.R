library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

#### Spurious Association
  
  # predicting divorce rate from median age married (normalized)
d$MedianAgeMarriage_s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)

m5_1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA * MedianAgeMarriage_s,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data=d
)

mam_seq <- seq(-3, 3.5, length.out = 30)
mu <- link(m5_1, data = list(MedianAgeMarriage_s = mam_seq))
mu_pi <- apply(mu, 2, PI)

plot(Divorce ~ MedianAgeMarriage_s, data=d, col=rangi2)
abline(m5_1)
shade(mu_pi, mam_seq)

  # using both median age married and marriage rate as predictors
d$Marriage_s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)
m5_3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage_s + bA*MedianAgeMarriage_s,
    a ~ dnorm(10,10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
precis(m5_3)
plot(precis(m5_3))

  ## predictor residual plots
m5_4 <- map(
  alist(
    Marriage_s ~ dnorm(mu, sigma),
    mu <- a + b * MedianAgeMarriage_s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

  # compute the residuals
mu <- coef(m5_4)['a'] + coef(m5_4)['b'] * d$MedianAgeMarriage_s
m_resid <- d$Marriage_s - mu

  # plot the residuals
plot(Marriage_s ~ MedianAgeMarriage_s, d, col=rangi2)
abline(m5_4)
for (i in 1:length(m_resid)){
  x <- d$MedianAgeMarriage_s[i]
  y <- d$Marriage_s[i]
  lines(c(x,x), c(mu[i], y), lwd=0.5, col=col.alpha('black', 0.7))
}

  ## counterfactual plots
a_avg <- mean(d$MedianAgeMarriage_s)
r_seq <- seq(-3,3, length.out = 30)
pred_data <- data.frame(Marriage_s=r_seq, MedianAgeMarriage_s=a_avg)

  #compute counterfactual mean
mu <- link(m5_3, data=pred_data)
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI)

r_sim <- sim(m5_3, data=pred_data, n=1e4)
r_pi <- apply(r_sim, 2, PI)

  # Display predictions, hiding raw data
plot(Divorce ~ Marriage_s, data=d, type='n')
mtext("MedianAgeMarriage_s = 0")
lines(r_seq, mu_mean)
shade(mu_pi, r_seq)
shade(r_pi, r_seq)

  ## Overthinking: simulating spurious association
  # When predictor influences both outcome and spurious predictor

N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <- rnorm(N, x_real)
d <- data.frame(y,x_real, x_spur)
pairs(d)

#### Masked Relationships
data(milk)
d <- milk
dcc <- d[complete.cases(d),]

m5_5 <- map(
  alist(
    kcal.per.g <- dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ),
  data=dcc
)

precis(m5_5, digits=3)
max(dcc$neocortex.perc)

dcc$log.mass <- log(dcc$mass)
m5_6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm*log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data=dcc
)
precis(m5_6)


  # Now, model with two predictors
m5_7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm*log.mass * bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data=dcc
)
precis(m5_6)

  ## Simulating masking relationship
N <- 100
rho <- 0.7
x_pos <- rnorm(N)
x_neg <- rnorm(N, rho*x_pos, sqrt(1-rho^2))
y <- rnorm(N, x_pos - x_neg)
d <- data.frame(y, x_pos, x_neg)
pairs(d)


#### When Adding variables hurts

  ## Multicollinear legs
N <- 100
height <- rnorm(N,10,2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop*height + rnorm(N,0,0.2)
leg_right <- leg_prop*height + rnorm(N,0,0.2)

d <- data.frame(height, leg_left, leg_right)


m5_8 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10,100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
precis(m5_8)

plot(precis(m5_8))

  # bivariate posterior distribution for bl and br
post <- extract.samples(m5_8)
plot(bl ~ br, post, col=col.alpha(rangi2, 0.1), pch=16)

sum_blbr <- post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab='sum of bl and br')

  ## Post treatment bias
  # simulation for the treatment of fungus
N <- 100
h0 <- rnorm(N,10,2) # initial heights
treatment <- rep(0:1, each=N/2)
fungus <- rbinom(N, size=1, prob=0.5 - treatment*0.4)
h1 <- h0 + rnorm(N, 5-3*fungus)

d <- data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)

m5_13 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment + bf*fungus,
    a ~ dnorm(0, 100),
    c(bh, bt, bf) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
precis(m5_13)
  # Notice treatment is not a valubale predictor since we included fungus in the model, which was a consequence of the treatment

  ## Unique Intercepts
  # Instead of using dummy variables, make the intercept a vector
data(milk)
d <- milk
d$clade_id <- coerce_index(d$clade)

m5_16 <- map(
  alist(
    kcal.per.g <- dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
precis(m5_16, depth=2)
