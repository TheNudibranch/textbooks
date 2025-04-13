library(rethinking)

##### H1
  #predict the height for the corresponding weight
data("Howell1")
d <- Howell1
pred_weight <- c(46.95, 43.72, 64.78, 32.59, 54.63)

mh1 <- map(
  alist(
    height ~ dnorm(a+b*weight, sigma),
    a ~ dnorm(178,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data=d
)
sim_height <- sim(mh1, data=list(weight=pred_weight))
m_height <- apply(sim_height, 2, mean)
pi_height <- apply(sim_height, 2, PI, prob=0.89)

##### H2
d1 <- d[d$age < 18,]
mh2 <- map(
  alist(
    height ~ dnorm(mu,sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
  data=d1
)
precis(mh2)
coef(mh2)['b']*10
max(d1$weight)

weight_seq <- seq(-4,45, length.out=100)
mu <- link(mh2, data=list(weight=weight_seq))
mu_mean <- apply(mu, 2,mean)
mu_pi <- apply(mu, 2, PI, prob=0.89)
h_sim <- sim(mh2, data=list(weight=weight_seq))
h_pi <- apply(h_sim, 2, PI, prob=0.89)
plot(d1$weight, d1$height,col=col.alpha(rangi2, 0.5))
lines(weight_seq, mu_mean)
shade(mu_pi, weight_seq)
shade(h_pi, weight_seq)

###### H3
  # model the relationship between height and weight as logarithmic
mh3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * log(weight),
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0,50)
  ),
  data=Howell1
)
precis(mh3)


weight_seq <- seq(4, 65, length.out = 100)
mu <- link(mh3, data = list(weight=weight_seq))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI, prob=0.97)
sim_height <- sim(mh3, data = list(weight=weight_seq))
height_pi <- apply(sim_height, 2, PI, prob=0.89)

plot(d1$weight, d1$height,col=col.alpha(rangi2, 0.5))
lines(weight_seq, mu_mean)
shade(mu_pi, weight_seq)
shade(height_pi, weight_seq)
