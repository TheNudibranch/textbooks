library(purrr)
library(rlang)

rstudioapi::getActiveDocumentContext()$path |> dirname() |> setwd()
source('dsl-html-attributes.r')

##############
## HTML
##############
html <- function(x) structure(x, class='advr_html')

print.advr_html <- function(x, ...){
  out <- paste0('<HTML>', x)
  cat(paste(strwrap(out), collapse='\n'), '\n', sep='')
}

escape <- function(x) UseMethod('escape')

escape.character <- function(x){
  x <- gsub('&', '&amp;', x)
  x <- gsub('<', '&lt;', x)
  x <- gsub('>', '&gt;', x)
  
  html(x)
}

escape.advr_html <- function(x) x

escape('This is some text')
escape('x > 1 & y < 2')
a <- escape('x > 1 & y < 2')
help(list2)

dots_partition <- function(...){
  dots <- list2(...)
  
  if(is.null(names(dots))) is_named <- rep(FALSE, length(dots))
  else is_named <- names(dots) != ''
  
  list(
    named = dots[is_named],
    unnamed = dots[!is_named]
  )
}

p <- function(...){
  dots <- dots_partition(...)
  attribs <- html_attributes(dots$named)
  children <- map_chr(dots$unnamed, escape)
  
  html(
    paste0(
      '<p', attribs, '>',
      paste(children, collapse=''),
      '</p>'
    )
  )
}


p('some text&', id='myid')
p('some text', class = 'important', `data-value`='10')

tag <- function(tag){
  new_function(
    exprs(... =),
    expr({
      dots <- dots_partition(...)
      attribs <- html_attributes(dots$named)
      children <- map_chr(dots$unnamed, escape)
      
      html(paste0(
        paste0('<', !!tag), attribs, '>',
        paste(children, collapse = ''),
        paste0('</', !!tag, '>')
      ))
    }),
    caller_env()
  )
}

p <- tag('p'); b <- tag('b'); i <- tag('i')
p('some text. ', b(i('some bold italic text')), class='mypara')


void_tag <- function(tag){
  new_function(
    exprs(... =),
    expr({
      dots <- dots_partition(...)
      if (length(dots$unnamed) > 0) abort(!!paste0('<', tag, '> must not have unnamed arguments'))
      
      attribs <- html_attributes(dots$named)
      
      html(paste0(!!paste0('<', tag), attribs, ' />'))
    }),
    caller_env()
  )
}

img <- void_tag('img')
img(src='myimage.png', width=100, height=100)

html_tags <- c(
  tags |> set_names() |> lapply(tag),
  void_tags |> set_names() |> lapply(void_tag)
)

  # Messy view
html_tags$p(
  'some text.',
  html_tags$b(html_tags$i('some bold and italix text')),
  class='mypara'
)

  # Using a data mask
with_html <- function(code){
  # code <- enexpr(code)
  code <- enquo(code)
  print(current_env())
  eval_tidy(code)
}

with_html(
{  current_env() |> env_parents() |> print()
  current_env() |> env_parent() |> print()
  current_env()}
  # body(
  #   h1('A heading', id='first'),
  #   p('some text &', b('some bold text.')),
  #   img(src='myimg.png', width=100, height=100)
  # )
)
help("eval_tidy")


exprs(a, b=1)

############
## Latex
############
source('latex_helper.R')
to_math <- function(x){
  expr <- enexpr(x)
  out <- eval_bare(expr, latex_env(expr))
  
  latex(out)
}

latex <- function(x) structure(x, class='advr_latex')
print.advr_latex <- function(x) cat('<LATEX>', x, '\n', sep='')

greek_list <- set_names(paste0('\\', greek), greek)
greek_env <- as_environment(greek_list)
latex_env <- function(expr) greek_env

to_math(pi); to_math(beta)

all_names_rec <- function(x){
  switch_expr(x,
    constant = character(),
    symbol = as.character(x),
    call = lapply(as.list(x[-1]), all_names) |> unlist()
  )
}
all_names <- function(x) unique(all_names_rec(x))

all_names(expr(x + 1 + 1 + foo(y+1)))

  ### Example of recursive list function
a <- function(x) {
  if (identical(x,1)){
    return('v')
  }
  else{
    switch(typeof(x),
           double = a(x-1),
           list=lapply(x,\(y) a(y))
    )
  }
}

a(list(list(2,9,list(10)),5))

latex_env <- function(expr){
  # Unkown Symbols
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names))
  env_clone(greek_env, symbol_env)
}

unary_op <- function(left, right){
  new_function(
    exprs(e1 = ),
    expr(paste0(!!left, e1, !!right)),
    caller_env()
  )
}
binary_op <- function(sep){
  new_function(
    exprs(e1=,e2=),
    expr(paste0(e1,!!sep,e2)),
    caller_env()
  )
}
# Binary operators
f_env <- child_env(
  .parent = empty_env(),
  `+` = binary_op(" + "),
  `-` = binary_op(" - "),
  `*` = binary_op(" * "),
  `/` = binary_op(" / "),
  `^` = binary_op("^"),
  `[` = binary_op("_"),
  
  # Grouping
  `{` = unary_op("\\left{ ", " \\right}"),
  `(` = unary_op("\\left( ", " \\right)"),
  paste = paste,
  
  # Other math functions
  sqrt = unary_op("\\sqrt{", "}"),
  sin =  unary_op("\\sin(", ")"),
  log =  unary_op("\\log(", ")"),
  abs =  unary_op("\\left| ", "\\right| "),
  frac = function(a, b) {
    paste0("\\frac{", a, "}{", b, "}")
  },
  
  # Labelling
  hat =   unary_op("\\hat{", "}"),
  tilde = unary_op("\\tilde{", "}")
)

latex_env <- function(expr){
  f_env
  # Unkown Symbols
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names), parent = f_env)
  # Known Symbols
  env_clone(greek_env, symbol_env)
}

to_math(sin(x + pi))

all_calls_rec <- function(x){
  switch_expr(x,
    constant=,
    symbol= character(),
    call = {
      fname <- as.character(x[[1]])
      children <- lapply(as.list(x[-1]), all_calls) |> unlist()
      c(fname, children)
    }
  )
}

all_calls <- function(x) unique(all_calls_rec(x))
all_calls(expr(foo(c+1)))

unknown_op <- function(op){
  new_function(
    exprs(...=),
    expr({
      contents <- paste(list(...), collapse=', ')
      paste0(!!paste0('\\mathrm{', op, "}("), contents, ")")
    })
  )
}

latex_env <- function(expr){
  # Unknown functions
  calls <- all_calls(expr)
  call_list <- map(set_names(calls), unknown_op)
  call_env <- as_environment(call_list)
  
  # Known functions
  f_env <- env_clone(f_env, call_env)
  
  # Default Symbols
  names <- all_names(expr)
  symbol_env <- as_environment(set_names(names), parent=f_env)
  
  # Known Symbols
  greek_env <- env_clone(greek_env, parent=symbol_env)
  greek_env
}

to_math(sin(pi) + f(a,b))

