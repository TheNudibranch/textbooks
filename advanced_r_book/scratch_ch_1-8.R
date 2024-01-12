library(dplyr)
library(tibble)
library('tidyverse')
install.packages('cli')
remove.packages('cli')
library(cli)
package_version(cli)
sessionInfo()

View(setNames)

a <- structure(1:5, comment='abc')
str(a)
attributes(a)
dim(a) <- c(1,5)
a

a <- factor(c('a', 'b', 'b', 'c'))
b <- c(a, 'd')

f1 <- factor(letters)
levels(f1) <- rev(levels(f1))

f2 <- rev(factor(letters))
str(f1)
f1
f2
mtcars
str(mtcars)
attributes(mtcars)

dplyr::starwars$films

df <- data.frame(x=1:3, y=letters[1:3]) %>% as.tibble()
as.matrix(df)
data.matrix(df)
help("data.matrix")

rownames(df) <- c('a', 'az', 'b')
attributes(df)

df <- tibble(x=1:3, y=letters[1:3])
attributes(df)


a <- list('a'=c(1,2,3), letters[3:9], 5)
a['a'] <- list(NULL)
str(a)

mod <- lm(mpg~wt,data=mtcars)
mod$residuals %>% str()
mod$residuals %>% attributes()


View(mtcars)
mod$coefficients['wt']


(if (F) 1) %>% is.null()


ifelse((1:10) %% 3 == 0, T, 'a')

x <- letters[1:4]
sapply(x, function(x) switch(x,
                             a=1,
                             b=2,
                             c=3,
                             stop('Invalid x'))
)

means <- c(1,50,20)
out <- vector('list', length(means))
for (i in 1:length(means)){
  out[i] <- rnorm(10, means[[i]]) %>% list()
}


double <- function(x){
  message('calc')
  x * 2
}

h03 <- function(x){
  c(x,x)
}

h03(double(20))
ls()


x <- NULL
length(x)

# && only computes as many comparisons as it needs to
# For example, the first is False so anything after must be False, it can stop there
T && 1 && x > 0

c(T,T) & F

y <- 10;cat('\014')
f1 <- function(x={y <- 1;2}, y={x <- 5; 3}){
  # print(y)
  print(c(x,y))
  y <- 4
  print(x)
}
f1(x=30)

# There is a promise for x and a promise for y
# First the vector c(x,y) is printed
# Since x is already provided, the function does not have to look up x and therefore the promise is not run
# y is not defined so the function then evaluates the promise. This the reassigns x to 5 while assigning 3 to y.
# That is why x changes to 5 later on in the function


args(library)
args(sum)
args(mean)
sum(1,2,3, na.omit=T, NA)
View(plot.default)


f1 <- function(){
  on.exit(message('a'), add=T)
  on.exit(message('b'), add=T)
}
f1()

body(f1)
formals(f1)
environment(f1)
library(magrittr)
mtcars %>% plot(hp, mpg, data=.)
plot(mtcars$hp, mtcars$mpg)


mtcars %>% apply(., 1,\(x) mean(x^2))

mtcars %>% plot(.$hp)
plot(mtcars, mtcars$hp)
mtcars %>% {plot(.$hp, .$mpg)}


h01 <- function(x){
  10
}
h01(runif(1e10))


h05 <- function(x=ls()){
  a <- 1
  x
}

# ls() was evaluated in the funciton env, giving c('a', 'x')
h05()
h05(ls()) # In the global environment


help(missing)

args(plot)
View(plot.default)


for (i in 1:3) print(i)
`for`(i,1:3, print(i))


`+`(`+`(1,2),3)


### ch7
library(rlang)
env_parents(current_env())
e2a <- env(d=4, e=5)
l1 <- list('e1'=e2a)
e2b <- env(e2a, a=1, b=1, c = 3)
env_parent(e2a) |> env_print()
env_print(env_parent(e2b))

current_env() |> env_print()
current_env() |> env_parent() |> env_print()
e2b |> env_parents(last = empty_env())

plus <- function(x){
  function(y) x + y
}

plus_one <- plus(1)
fn_env(plus_one) |> env_print()

### Ch8
f <- function() g()
g <- function() h()
h <- function() rlang::abort('Plz stop')
h()

default <- NULL
try(default <- read.csv('bad_csv'), silent=T)
default <- try(read.csv('bad_csv'))
attributes(default)

cnd <- rlang::catch_cnd(stop('Error!'))
cnd$call %>% attributes()

a <- function(){
  tryCatch(
    message=\(x) {
      cat('hi\n')
      env_print(current_env())
      },
    {
      env_print(current_env())
      message('1')
      }
  )
}
a()


f <- function() g()
g <- function() h()
h <- function() message('!')

withCallingHandlers(
  message=function(cnd){
    lobstr::cst()
    rlang::cnd_muffle(cnd)
  },
  f()
)

a <- catch_cnd(stop('error'))
b <- catch_cnd(abort('error'))
help(abort)

abort_bad_argument <- function(arg, must, not=NULL){
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)){
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }
  
  abort("error_bad_argument",
        message=msg,
        arg=arg,
        must=must,
        not=not
  )
}


my_log <- function(x, base=exp(1)){
  if (!is.numeric(x)){
    abort_bad_argument('x', must='be numeric', not=x)
  }
  if (!is.numeric(base)){
    abort_bad_argument('base', must='be numeric', not=base)
  }
  base::log(x,base=base)
}
my_log(letters)
a <- catch_cnd(my_log(letters))

match.arg('info')
?match.arg

log <- function(message, level=c('info', 'error', 'fatal')){
  print(level)
  print(match.arg(level))
  level <- match.arg(level)
  print(level)
  signal(message, 'log', level=level)
}
log('Run')
a <- c('info', 'error', 'fatal')
b <- match.arg(a)
?signal
View(match.arg)
attributes(match.arg)
