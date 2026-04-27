#M2, H1, H2, H3, H7

library(rethinking)
library(tidyverse)
library(cmdstanr)
rstudioapi::getActiveDocumentContext()$path |> dirname() |> setwd()
options(mc.cores=4)
#################
# M2
#################
ratings <- 1:4
cnts <- c(12, 36, 7, 41)
prop <- cnts/sum(cnts)
cuml_prop <- cumsum(prop)
plot(NULL, xlim=range(ratings), ylim=range(cuml_prop))
for (i in seq_along(ratings)) segments(x0=ratings[i], y0=0, y1=cuml_prop[i], lwd=3, col='grey')
for (i in seq_along(ratings)) segments(x0=ratings[i]+0.03, y0=replace_na(lag(cuml_prop), 0)[i], y1=cuml_prop[i], lwd=3, col='darkblue')
lines(cuml_prop, type='b')

#################
# H1
#################
data("Hurricanes")
d <- Hurricanes
d$femininity

d$deaths

m_h1_1 <- quap(alist(
  deaths ~ dpois(lambda),
  log(lambda) <- a,
  a ~ dnorm(3, 0.5)
), data=d)

m_h1_2 <- quap(alist(
  deaths ~ dpois(lambda),
  log(lambda) <- a + beta*femininity,
  beta ~ dnorm(0, 1),
  a ~ dnorm(3, 0.5)
), data=d)
compare(m_h1_1, m_h1_2)

precis(m_h1_2)
# Looks like model 2 performs somewhat better, but it is not significant

# postcheck(m_h1_1)
# postcheck(m_h1_2)
# 
# plot(precis(m_h1_2))

f_seq <- range(d$femininity) %>% {seq(.[1], .[2], length.out=100)}
lambda <- link(m_h1_2, data=data.frame(femininity=f_seq))
plot(f_seq, colMeans(lambda), type='l', lwd=2, ylim=range(lambda, d$deaths))
apply(lambda, 2, PI) %>% shade(f_seq,)
points(d$femininity, d$deaths, pch=16)

d$deaths |> table() |> barplot()
# It does better for the lower death hurricanes, but has a very week relationship

#################
# H2
#################

# Lets fit a gamma-poisson model
m_h2_1 <- quap(alist(
  deaths ~ dgampois(lambda, phi),
  log(lambda) <- a + beta*femininity,
  beta ~ dnorm(0, 1),
  a ~ dnorm(0, 1),
  phi ~ dexp(1)
), data=d)

f_seq <- range(d$femininity) %>% {seq(.[1], .[2], length.out=100)}
lambda <- link(m_h2_1, data=data.frame(femininity=f_seq))
plot(f_seq, colMeans(lambda), type='l', lwd=2, ylim=range(lambda, d$deaths))
apply(lambda, 2, PI) %>% shade(f_seq,)
points(d$femininity, d$deaths, pch=16)

precis(m_h2_1)

# because the gamma-poisson is allowed to vary its variance, it allows the model
# to load most of the uncertainty onto the variance parameter, not the mean (which is the same in poisson)
# if we modeled this with a normal we would probably see a similar scenerio where the mean would be zero and the sigma would increase

#################
# H3
#################
# Fit a series of models where we consider the interaction between damage_norm and min_pressure

d$damage_norm_2 <- d$damage_norm/1e5
d$damage_norm_2 |> hist()
m_h3_1 <- quap(alist(
  deaths ~ dgampois(lambda, phi),
  log(lambda) <- a + b_f*femininity + b_d*damage_norm_2 + b_i*femininity*damage_norm_2,
  b_f ~ dnorm(0, 1),
  b_d ~ dnorm(0,1),
  b_i ~ dnorm(0,1),
  a ~ dnorm(3, 0.5),
  phi ~ dexp(1)
), data=d)

d$min_pressure_2 <- d$min_pressure/1e3
m_h3_2 <- quap(alist(
  deaths ~ dgampois(lambda, phi),
  log(lambda) <- a + b_f*femininity + b_d*min_pressure_2 + b_i*femininity*min_pressure_2,
  b_f ~ dnorm(0, 1),
  b_d ~ dnorm(0,1),
  b_i ~ dnorm(0,1),
  a ~ dnorm(3, 0.5),
  phi ~ dexp(1)
), data=d)

compare(m_h3_1, m_h3_2, m_h2_1, m_h1_1)


dm <- range(d$damage_norm_2) %>% {seq(.[1], .[2], length.out=4)}
f_seq <- range(d$femininity) %>% {seq(.[1], .[2], length.out=100)}
plot(NULL, type='l', lwd=2, ylim=range(d$deaths), xlim=range(f_seq))
for (i in seq_along(dm)){
  lambda <- link(m_h3_1, data=data.frame(femininity=f_seq, damage_norm_2=dm[i]))
  lines(f_seq, colMeans(lambda), type='l', lwd=2, ylim=range(lambda, d$deaths), col=i)
  apply(lambda, 2, PI) %>% shade(f_seq,)
}
legend('topleft', col=seq_along(dm), legend=paste0('Damage Norm: ',dm|> round(2)), lwd=2)
points(d$femininity, d$deaths, pch=16)

# It actually doesnt look bad
# It looks like there is moderating effect where as feminity increase, it increases the present effect of damage
# I bet there is some other confounder though
plot(precis(m_h3_1), xlim=c(-1,3))


#################
# H7
#################
# Look back at the Trolley problem
data("Trolley")
d <- Trolley
edu_levels <- c(6, 1, 8, 4, 7, 2, 5, 3)
d$edu_new <- edu_levels[d$edu] # only works if d$edu is a factor

dat <- list(
  N=nrow(d),
  K=length(unique(d$response)),
  R = d$response,
  action = d$action,
  intention = d$intention,
  contact = d$contact,
  E_id = as.integer(d$edu_new),
  alpha=rep(2, 7)
)
model <- cmdstan_model('model_12_6.stan')
fit <- model$sample(data=dat)


m12.6 <- ulam(alist(
  R ~ ordered_logistic(phi, kappa),
  phi <- bE*sum(delta_j[1:E]) + bA*action + bC*contact + bI*intention,
  kappa ~ normal(0, 1.5),
  c(bA, bI, bC, bE) ~ normal(0,1),
  vector[8]: delta_j <<- append_row(0, delta),
  simplex[7]: delta ~ dirichlet(alpha)
), data=dat, chains=4, cores=4, iter=200, start=list(kappa=seq(-3,3,length.out=6)))

