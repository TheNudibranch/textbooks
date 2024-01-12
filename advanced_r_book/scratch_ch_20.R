library(rlang)
library(lobstr)
#### Ch 20
a <- '
x <- 1;y <- 2
x + y

'
parse_exprs(a) |> `[`(1)  typeof()
parse(text=a) |> typeof()

x <- 10; y <- 20;
f <- eval(expr(function(x, y) !!x + !!y))
f

  # q 20.2 2
eval(expr(eval(expr(eval(expr(2+2))))))

  # q 20.2 3
get2 <- function(name, env=caller_env()){
  n <- ensym(name)
  force(env)
  eval(expr(`$`(env, !!n)), current_env())
}
x <- 1
get(x)

assign2 <- function(name,value, env=caller_env()){
  n <- ensym(name)
  eval(expr(`<-`(!!n, !!value)), envir = env)
}
assign2(x, 1)

enexpr
  # q 20.2 5
local3 <- function(expr, envir=new.env()){
  # rm(expr)
  call <- substitute(eval(quote(expr), envir))
  call
  # eval(call, envir=parent.frame())
}
local3(
  {
    x <- 1
    y <- 2
    x + y
  }
)
  # Note that substitute keeps everything but 'expr' unchanged
  # This is because 'expr' in this context is a promise and is therefore replaced for the 'expr' symbol
  # In the substitute function


f <- function(...) {x <- 1; g(..., f = x)}
g <- function(...) {enquos(...)}


x <- 0
a <- f(global = x)
a |> lapply(eval_tidy)

q2 <- new_quosure(expr(x), env(x=1))
q3 <- new_quosure(expr(x), env(x=11))
x <- expr(!!q2 + !!q3 + x)
x # Formula heriatage
x[[2]]


with2 <- function(data, expr){
  expr <- enquo(expr)
  eval_tidy(expr, data)
}

select2 <- function(data,...){
  dots <- enquos(...)
  
  vars <- as.list(set_names(seq_along(data), names(data)))
  cols <- unlist(purrr::map(dots, eval_tidy, vars))
  
  df[,cols,drop=F]
  return(dots)
}

df <- data.frame(a=1,b=2, c=3, d=4, e=5)
a <- select2(df, b:d)

subset2 <- function(data, rows){
  rows <- enquo(rows)
  rows_val <- eval_tidy(rows, data)
  stopifnot(is.logical(rows_val))
  
  data[rows_val, , drop=FALSE]
}

threshold_x <- function(df, val){
  subset2(df, x >= val)
}

x <- 10
no_x <- data.frame(y=1:3)
# X in encolsing but not mask
threshold_x(no_x, 2)

has_val <- data.frame(x = 1:3, val = 9:11)
threshold_x(has_val,2)
# 2 is ignored since x and val are arguments in mask

w_csv <- function(...){
  call <- match.call(write.table,expand.dots = TRUE)
  
  call[[1]] <- quote(write.table)
  call$sep <- ','
  call$dec <- '.'
  
  call
}

  ## Q 20.6.1
lm3a <- function(formula, data){
  formula <- enexpr(formula)
  lm_call <- expr(lm(!!formula, data=!!ensym(data)))
  expr_print(lm_call)
  eval(lm_call, caller_env())
}

lm3a(mpg ~ disp, mtcars)

  ## Q 20.6.2
resp_wraper <- function(predictor, response, data){
  formula <- expr(!!ensym(response) ~ !!enexpr(predictor))
  lm_call <- expr(lm(!!formula, data=!!ensym(data)))
  expr_print(lm_call)
  eval(lm_call, caller_env())
}

resp_wraper(disp*cyl, mpg, mtcars)
resp_wraper(I(1/disp), mpg, mtcars)

f <- function(e = current_env()){
  e
  enquo(e)
}
f()
enquo
asNamespace('rlang')$ffi_enquo

help(enquo)
