library(markmyassignment)
library(aaltobda)
assignment_path <-
  paste("https://github.com/avehtari/BDA_course_Aalto/",
        "blob/master/assignments/tests/assignment2.yml", sep="")

set_assignment(assignment_path)
show_tasks()
mark_my_assignment()

data('algae')
algae_test <- c(0, 1, 1, 0, 0, 0)

beta_point_est <- function(prior_alpha, prior_beta, data){
  post_alpha <- prior_alpha + sum(data)
  post_beta <- prior_beta + length(data) - sum(data)
  return(post_alpha / (post_alpha + post_beta))
}
beta_point_est(prior_alpha = 2, prior_beta = 10, data = algae_test)
mark_my_assignment(tasks = 'beta_point_est')

beta_interval <- function(prior_alpha, prior_beta, data, prob){
  post_alpha <- prior_alpha + sum(data)
  post_beta <- prior_beta + length(data) - sum(data)
  lower <- qbeta((1-prob)/2, post_alpha, post_beta)
  upper <- qbeta((1-prob)/2 + prob, post_alpha, post_beta)
  return(c(lower,upper))
}

beta_interval(2, 10, algae_test, 0.9)
mark_my_assignment(task = 'beta_interval')

beta_low <- function(prior_alpha, prior_beta, data, pi_0){
  post_alpha <- prior_alpha + sum(data)
  post_beta <- prior_beta + length(data) - sum(data)
  cuml_prob <- pbeta(pi_0, post_alpha, post_beta)
  return(cuml_prob)
}

beta_low(2, 10, algae_test, 0.2)
mark_my_assignment()
