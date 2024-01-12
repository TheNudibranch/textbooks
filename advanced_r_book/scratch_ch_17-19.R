library(rlang)
library(lobstr)

#### Ch 17
a <- expr(f(x=1, y=2))
typeof(a)

a[[1]]

ast(1 + 2 * 3)
ast(f1(f2(a,b, c), f3(1, f4(2))))

eval(expr(x+y), env(x=1, y=3))


#### Ch 18

View(expr(f(x, "y", 1, g(1,2))))
ast(f(x, "y", 1, g(1,2)))

ast(read.table('important.csv', row.names=F))
expr(`~`) |> typeof()
expr(a + 1) |> typeof()
help(str)

x <- 1
eval(x+1, envir = env(x = 2))
eval(expr(x+1), envir = env(x = 2))


parse(text = 'x +1') |> eval()

ast(x4 <- x3 <- x2 <- x1 <- 0)

a <- pairwise.t.test(1,2)
sloop::s3_dispatch(print(a))
print.pairwise.htest(a)
stats::print.pairwise.htest
b <- getAnywhere(print.pairwise.htest)
ast(b$objs[[1]](a))

getAnywhere %>% View()

x <- 'print.pairwise.htest'
help(find)

find %>% View()

search()

ls('package:stats')

'stats' %in% loadedNamespaces()
a <- asNamespace('stats')
a$print.pairwise.htest

a <- ns_env('stats')
a$print.pairwise.htest
ns_env |> View()

a <- call2('<-', expr(x), 10)
a |> eval()

expr(mean(f(1,x))) |> typeof()
expression(x+1) |> typeof()

quote(mean(1:10))[2]

expr_type <- function(x){
  if(rlang::is_syntactic_literal(x)) 'constant'
  else if (is.symbol(x)) 'symbol'
  else if (is.call(x)) 'call'
  else if (is.pairlist(x)) 'pairlist'
  else typeof(x)
}


switch_expr <- function(x,...){
  switch(expr_type(x),
         ...,
         stop('Dont know how to handle type ', typeof(x), call. = F)
  )
}

logical_abbr_rec <- function(x){
  switch_expr(
    x,
    constant = FALSE,
    symbol = as_string(x) %in% c('T', 'F'),
    call = ,
    pairlist = purrr::some(x, logical_abbr_rec)
  )
}


logical_abbr <- function(x) logical_abbr_rec(enexpr(x))

logical_abbr(mean(x, na.rm=TRUE))


g <- function(x,y) {x+y}
a <- expr(mean(1:10, na.rm=F))
a[[4]]
ast(function(x,y) {x+y;g(1)})
quote(1 + 1) |> is.pairlist()

purrr::some(1:100, ~sample(c(T,F), 1, prob = c(0.01, 0.99)))

a <- expr(function(x = TRUE) {
  g(x + T)
})
a[[4]] |> typeof()
a[[3]] |> typeof()
a[[2]] |> typeof()
a[[1]] |> typeof()
is.symbol(a)


ast(names(x(y)) <- y <- z)
a <- expr(names(x(y)) <- y <- z)
a[[2]][[2]] |> is.call()

#### Ch 19
library(purrr)

f <- function(...) ensyms(...)
f(x = 3)[[1]] |> typeof()
a <- alist(x = 1, y = x + 1, z = )
a |> typeof()

a <- exprs(x = x^2, y=y^3, z=z^4)
typeof(a); class(a); attributes(a)
a[[1]][[1]] 


substitute
  # 19.3. q6
#a)
substitute(x, env())
#b)
foo <- function(x) substitute(x)
foo(a + b *     sin(10))
#c)
substitute(x, env(x=7))
x <- 7
substitute(x, .GlobalEnv) # Equivalent to `substitute(x)`


x <- expr(4 + z)
expr(f(!!x, y))

mean_rm <- function(var){
  # var <- ensym(var)
  expr(mean(!!var, na.rm=TRUE))
}
mean_rm(x)

  # When in doubt use parentheses
f <- expr(foo)
expr((!!f)(x,y))
expr(!!f(x,y))

x <- expr(el_1)
expr(`$`(df, !!x))

a <- 3
xs <- exprs(1, !!a, -b)
expr(f(!!!xs, y))

ys <- set_names(xs, c('a', 'b', 'c'))
expr(f(!!!ys, y=4))

expr_deparse

  # 19.4 q4
xy <- expr(x+y); xz <- expr(x + z); yz <- expr(y + z); abc <- exprs(a,b,c)

expr(!!xy / !!yz)
expr(-(!!xz)^!!yz)
expr(!!xy + !!yz - !!xy)
expr(atan2(!!xy, !! yz))
expr(sum(!!xy, !!xy, !!yz))
expr(sum(!!!abc))
expr(mean(c(!!!abc), na.rm=TRUE))
a <- list(a=xy, b=yz)
expr(foo(!!!a))


xyz <- bquote(x + y + z)
bquote(-.(xyz) / 2)

  # Use list2 to get lists of expressions into a call or use the bang operators
set_attr <- function(.x, ...){
  attr <- rlang::list2(...)
  attributes(.x) <- attr
  .x
}

attrs <- list(x = 1, y = 2)
attr_name <- 'z'

a <- expr(set_attr(w=0, !!!attrs, !!attr_name := 3))
1:10 |> set_attr(w=0, !!!attrs, !!attr_name := 3) |> str()

  # If you don't have control over the body of the function, use `exec`
    # Related to `call2`, except `call2` returns an expression
args <- list(x =1:10, na.rm=TRUE, trim=0.1)
exec('mean', !!!args)

x <- c(runif(10), NA)
funs <- c('mean', 'median', 'sd')

vapply(funs, exec, x, na.rm=TRUE, FUN.VALUE=numeric(1))

dfs <- list(
  data.frame(x = 1, y= 2),
  data.frame(x = 3, y = 4)
)

dplyr::bind_rows(!!!dfs)
do.call(rbind, dfs)


  # Linear model equation builder
    # Assumes that the first val is the intercept
linear <- function(var, val){
  var <- ensym(var)
  coef_name <- purrr::map(seq_along(val[-1]), \(x) expr((!!var)[[!!x]]))
  
  summands <- purrr::map2(val[-1], coef_name, \(x,y) expr((!!x * !!y)))
  summands <- c(val[[1]], summands)
  
  purrr::reduce(summands, \(x, y) expr(!!x + !!y))
}

linear(x+1, c(10, 2, -3))


  # 19.7 q2
bc <- function(lambda){
  if (lambda == 0) bd <- expr(log(x))
  else bd <- expr((x^(!!lambda) - 1) / !!lambda)
  
  return(new_function(exprs(x = ), bd, .GlobalEnv))
}

a <- bc(1.5)
a

  # 19.7 q3
compose <- function(f ,g) {
  f_name <- ensym(f)
  g_name <- ensym(g)
  
  new_function(
    exprs(... = ), 
    expr((!!f_name)((!!g_name)(list2(...)[[1]]))), 
    .GlobalEnv
  )
}


a <- compose(sin, cos)
x <- list(c(1,2,3))
a(!!!x)
a(expr(x[[1]]))


df <- data.frame(list(!!x := c(1,2,3), y = c(4,5,6)))
setNames(df, c(a, 'b'))
tibble::tibble_q
