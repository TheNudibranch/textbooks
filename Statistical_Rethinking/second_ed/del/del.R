library(magrittr)
erfz <- pracma::erfz


N <- 100
x <- runif(N, -2*pi, 2*pi)
y <- cos(x) + rnorm(N,0,0.1)
plot(x,y)

kern_func <- function(r, l){
  exp(- r^2 / (2*l^2))
}

spec_func <- function(w, l){
  sqrt(2*pi) * l * exp(- l^2 * w^2 / 2)
}

## Plot GP Priors
par(mfrow=c(2,2))
N_seq <- 1e3
x_seq <- seq(-5, 5, length.out=N_seq)
for (l in c(0.1, 0.5, 1, 2)){
  gp_prop_cov <- matrix(NA, nrow=N_seq, ncol=N_seq)
  for (i in 1:(N_seq-1)){
    for (j in (i+1):N_seq){
      r <- abs(x_seq[i] - x_seq[j])
      gp_prop_cov[i,j] <- gp_prop_cov[j,i] <- kern_func(r, l=l)
    }
  }
  diag(gp_prop_cov) <- 1
  
  N_draws <- 5
  gp_prior_draws <- MASS::mvrnorm(N_draws, mu=rep(0, N_seq), Sigma=gp_prop_cov)
  
  plot(NULL, xlim=range(x_seq), ylim=range(gp_prior_draws), main=sprintf('Length Scale: %s', l))
  for (i in 1:N_draws){
    lines(x_seq, gp_prior_draws[i,], col=i, lwd=2)
  }
}


## Plot GP Priors given fixed points
  # Observed (x,y)
par(mfrow=c(2,2))
obs <- c(
  c(-2, 2),
  c(0, -3),
  c(4, 2),
  c(-4, 0)
) |> matrix(ncol=2, byrow=T)

n_obs <- nrow(obs)
N_seq <- 1e3
x_seq <- seq(-5, 5, length.out=N_seq)
N_new <- N_seq + n_obs
x_seq_new <- c(x_seq, obs[,1])
draws <- list()
l_scale <- c(0.1, 0.5, 1, 2)

for (l in seq_along(l_scale)){
  gp_prop_cov <- matrix(NA, nrow=N_new, ncol=N_new)
  
  for (i in 1:(N_new-1)){
    for (j in (i+1):N_new){
      r <- abs(x_seq_new[i] - x_seq_new[j])
      gp_prop_cov[i,j] <- gp_prop_cov[j,i] <- kern_func(r, l=l_scale[l])
    }
  }
  diag(gp_prop_cov) <- 1
  
  sig_11 <- gp_prop_cov[1:N_seq, 1:N_seq]
  sig_12 <- gp_prop_cov[1:N_seq, (N_seq+1):N_new]
  sig_21 <- gp_prop_cov[(N_seq+1):N_new, 1:N_seq]
  sig_22 <- gp_prop_cov[(N_seq+1):N_new, (N_seq+1):N_new]
    
    
  mu_cond <- sig_12 %*% solve(sig_22) %*% obs[,2]
  sig_cond <- sig_11 - sig_12 %*% solve(sig_22) %*% sig_21
  
  N_draws <- 5
  draws[[l]] <- MASS::mvrnorm(N_draws, mu=mu_cond, Sigma=sig_cond)
  
  # plot(NULL, xlim=range(x_seq), ylim=range(gp_prior_draws), main=sprintf('Length Scale: %s', l))
  # for (i in 1:N_draws){
  #   lines(x_seq, gp_prior_draws[i,], col=i, lwd=2)
  # }
  # points(obs, pch=16, cex=1.2)
  
}

for (l in seq_along(l_scale)){
  plot(NULL, xlim=range(x_seq), ylim=range(unlist(draws)), main=sprintf('Length Scale: %s', l_scale[l]))
  for (i in 1:N_draws){
    lines(x_seq, draws[[l]][i,], col=i, lwd=2)
  }
  points(obs, pch=16, cex=1.2)
}


par(mfrow=c(1,1))

## Plot spectral density and kernel function
w_seq <- seq(0, 4, length.out=100)
w <- spec_func(w_seq, l=1)
par(mfrow=c(1,2))
plot(w_seq, w, type='l', main='Spectral Density')
x_seq[x_seq>0] %>% {plot(., kern_func(., l=1), type='l', main='Kernel')}

## Plot Kernel function after filtering out higher frequencies
kern_func_filter <- function(r, l, s_high_bound = 5) {
  r <- as.numeric(r)
  
  log_prefactor <- -r^2 / (2 * l^2)
  
  result <- numeric(length(r))
  
  overflow <- log_prefactor < -500
  safe     <- !overflow
  
  if (any(safe)) {
    x <- (l / sqrt(2)) * (s_high_bound - (r[safe] * 1i / l^2))
    result[safe] <-  exp(log_prefactor[safe]) * Re(erfz(x))
  }
  
  # overflow entries stay 0 (already initialised)
  result
}

kern_func_filter(3, 1, 4)
kern_func(3, 1)

N_seq <- 1e3
x_seq <- seq(-10, 10, length.out=N_seq)

par(mfrow=c(1,1))
plot(NULL, xlim=range(x_seq), ylim=c(-0.1,1.1))
lines(x_seq, kern_func(x_seq, l=1), lwd=10)
w_vals <- c(0.5, 1, 1.5, 2, 2.5)
for (i in seq_along(w_vals)){
  lines(x_seq, kern_func_filter(x_seq, l=1, w_vals[i]), col=i+1, lwd=4)
}
legend('topleft', col=seq_along(w_vals)+1, legend=paste0('Freq. Cutoff: ', w_vals), lwd=2)


## Plot the GP prior with varying frequency cutoffs
set.seed(123)
par(mfrow=c(2,2))
N_seq <- 400
x_seq <- seq(-5, 5, length.out=N_seq)
w_max <- 5
for (w in c(1, 2, 3, 4)){
  gp_prop_cov <- matrix(NA, nrow=N_seq, ncol=N_seq)
  for (i in 1:(N_seq-1)){
    r <- abs(x_seq[i] - x_seq[(i+1):N_seq])
    gp_prop_cov[i,(i+1):N_seq] <- gp_prop_cov[(i+1):N_seq,i] <- kern_func_filter(r, l=1, w)
  }
  diag(gp_prop_cov) <- 1
  
  N_draws <- 5
  gp_prior_draws <- MASS::mvrnorm(N_draws, mu=rep(0, N_seq), Sigma=gp_prop_cov)
  
  plot(NULL, xlim=range(x_seq), ylim=range(gp_prior_draws), main=sprintf('Max Frequency: %s', w))
  for (i in 1:N_draws){
    lines(x_seq, gp_prior_draws[i,], col=i, lwd=2)
  }
}

