####################
#### Chapter 22
####################
s
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) stop('`d` must be numeric', call.=FALSE)
  d + 10
}


j <- function() k()
k <- function() rlang::abort('Oops!')

f('a')
traceback()

aa <- function(x){
  browser()
}
aa(2)

trace
# options(error = recover)
options(error=NULL)
f('x')

aa <- function(x){
  1+1
}
debug(aa)
aa(2)
str(aa)
undebug(aa)

attributes(aa)

callr:::r(paste, list(1,2))
.libPaths()
environment(.libPaths)$.lib.loc
debugger()

####################
#### Chapter 23
####################
library(profvis)
library(bench)

source('profiling_example.R')
profvis(f())

profvis({
  x <- integer()
  for (i in 1:1e4){
    x <- c(x,i)
  }
})



profvis(j(i()))
help("match.call")
f <- function(...){
  match.call(expand.dots = FALSE)
}
f(a=2, b=3)$... |> typeof()
rm


x <- runif(100)
(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5
))
plot(lb)
lb[,c('expression', 'min', 'median', 'itr/sec', 'n_gc')]

(lb <- bench::mark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1/2),
  exp(log(x)/2)
))
plot(lb)
x <- runif(100,max=100)
a <- cbind(x, trunc(x), floor(x))

####################
#### Chapter 24
####################
library(bench)
quick_df <- function(l){
  class(l) <- 'data.frame'
  attr(l, 'row.names') <- .set_row_names(length(l[[1]]))
  l
}

l <- lapply(1:26, \(x) runif(1e3))
names(l) <- paste0('a_', seq(1:length(l)))

bench::mark(
  as.data.frame = as.data.frame(l),
  quick_df = quick_df(l)
)

  # q 20.5 #3
x <- rnorm(2e3, mean=10) |> matrix(ncol=2)
w <- runif(1e3)

crossprod(x,w)
colSums(x*w)

help("crossprod")

bench::mark(
  a = crossprod(x,w)[1],
  b = sum(x*w)
)
bench

.set_row_names(40)
help(".set_row_names")

####################
#### Chapter 24
####################
library(Rcpp)
cppFunction('
int add(int x, int y, int z){
  int sum = x + y + z;
  return sum;
}
')
add(1,2,3)
sourceCpp('mean_c.cpp')
Position
help("Position")
