# This is the code for H2 and H3 of the exercises The code wasnt runnning in quarto well (similar to ch 12)
# so I moved those over to this R script. I think it has a lot to do with how the ordinal model
# specifically the cutpoints are defined

library(rethinking)
library(tidyverse)
library(cmdstanr)
rstudioapi::getActiveDocumentContext()$path |> dirname() |> setwd()
options(mc.cores=4)
library(loo)

#########################
## H2
#########################
data("Trolley")
d <- Trolley

dat <- list(
  N = nrow(d),
  K = d$response |> unique() |> length(),
  R = d$response,
  action = d$action,
  intention = d$intention,
  contact = d$contact,
  alpha = rep(1,7),
  id = d$id |> as.integer(),
  n_unq_id = d$id |> as.integer() |> max()
)

model_12_5 <- cmdstan_model('model_12_5.stan')
fit_12_5 <- model_12_5$sample(data=dat, chains=4, iter_warmup = 500, iter_sampling = 500)
loo_12_5 <- fit_12_5$loo()

# Now one with varying intercepts

model_13_h2 <- cmdstan_model('model_13_h2.stan')
fit_13_h2 <- model_13_h2$sample(data=dat, chains=4)
loo_13_h2 <- fit_13_h2$loo()

fit_13_h2$draws('sig_id', format='df') %>% select(-starts_with('.')) %>% unlist() %>% hist()
# Average about 1.9, some very strong variation among people

loo::loo_compare(loo_12_5, loo_13_h2)

# elpd_diff se_diff
# model2     0.0       0.0
# model1 -2874.6      86.1

# Looks like model 2 is significantly better, which makes sense as there is a high degree of variation

#########################
## H3
#########################
dat_h3 <- list(
  N = nrow(d),
  K = d$response |> unique() |> length(),
  R = d$response,
  action = d$action,
  intention = d$intention,
  contact = d$contact,
  alpha = rep(1,7),
  id = d$id |> as.integer(),
  n_unq_id = d$id |> as.integer() |> max(),
  n_unq_story = d$story |> as.integer() |> max() ,
  story = d$story |> as.integer()
)

# lets now add story to the model
model_13_h3 <- cmdstan_model('model_13_h3.stan')
fit_13_h3 <- model_13_h3$sample(data=dat_h3, chains=4, iter_warmup = 500, iter_sampling = 500)
loo_13_h3 <- fit_13_h3$loo()

fit_13_h3$draws(c('sig_id', 'sig_story'), format='df') %>% select(-starts_with('.')) %>% apply(2,\(x) c(mean(x), sd(x)))

# sig_id sig_story
# [1,] 1.95751428 0.6295694
# [2,] 0.08164766 0.1619491

# Looks like sig_story is still important, but not as much

loo::loo_compare(loo_12_5, loo_13_h2, loo_13_h3)

# elpd_diff se_diff
# model3     0.0       0.0
# model2  -324.6      24.1
# model1 -3199.4      87.4

# But model 3 does produce significantly better results

fit_13_h3$draws('beta_story', format='df') %>% select(-starts_with('.')) %>% set_names(levels(d$story)) %>% 
  colMeans() %>% as.data.frame() %>% rownames_to_column() %>% set_names(c('story', 'val')) %>% arrange(val)

# story         val
# 1    pon -0.46256816
# 2    sha -0.40414493
# 3    spe -0.33079468
# 4    swi -0.24340795
# 5    boa -0.15693322
# 6    car -0.05540208
# 7    box  0.08620288
# 8    che  0.09451732
# 9    rub  0.31601727
# 10   bur  0.70754585
# 11   shi  0.97572297
# 12   aqu  1.21796197

# Recall here that a positive value actually moves us to 
# a higher category (which might be counter-intuitive since prob is cutpoint - phi) 
# (look at ch12 doc if need viz)
# so "aqu" will push the scores higher (more permissible) while "pon" will push them up (less permissible)