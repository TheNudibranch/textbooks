library(tidyverse)

as_mapper(~ length(unique(.x, .y, .z)))
a <- function(..., .x=..1, .y=..2){
  ..3
}
a(1, c(2,2,3))

x <- rep(0,4)
map_dbl(x, sum, runif(1))
map_dbl(x, \(y) sum(y,runif(1)))
map_dbl(x, ~sum(.x,runif(1)))

  # ch 9 q 1
help(as_mapper)
as_mapper(map_chr)
map_chr(mtcars, typeof) %>% as_mapper()

  # ch 9 q 4
trials <- map(1:100, ~t.test(rpois(10,10), rpois(7,10)))
map_dbl(trials, 'p.value')

  # ch 9 q 5
x <- list(
  list(1, c(2,3)),
  list(c(3,6), 7, c(4,7,6))
)
triple <- function(x) x*3
map(x, ~ map(.x, triple))
map(x, map, triple)


split(mtcars, mtcars$cyl) %>%
  map(~lm(mpg ~ wt, data=.x)) %>%
  map_dbl(~coef(.x)[2])

map(1:8, ~runif(10)) %>% .[c(1,2)]

x <- map(1:8, ~sample(1e3,10))
imap_chr(x, ~paste0("The highest value of ", .y, " is", max(.x)))


l <- map(1:4, ~sample(1:10, 15, replace=T))
out <- l[[1]]; out <- intersect(out, l[[2]]); out <- intersect(out, l[[3]]); out <- intersect(out,l[[4]])
out
  # or
out <- reduce(l, intersect)

df <- data.frame(x=1:3, y=c('a', 'b', 'c'))
df[[2]] %>% is.factor()
detect(df, is.factor)


help(rapply)


### Chapter 10
View(purrr::safely)
View(purrr::as_mapper)
View(purrr::capture_error)

### Chapter 13
library(sloop)
a <- factor(1:4)
class(a)
a %>% as.vector() %>% set_names(letters[1:4]) %>% attributes()
a %>% as.vector() %>% s3_class()
matrix(1:4, nrow=2) %>% attr('class')

matrix(1:4, nrow=2) %>% otype()
print(unclass(a))

ftype(print)
ftype(print.factor)
ftype(function(x) x^2)

  # q 13.2.1
x <- ecdf(rpois(100,10))
class(x)
typeof(x)
attributes(x)


b <- structure(
  0:1,
  levels='a',
  class='ttt'
)
otype(b)

print
help("UseMethod")
UseMethod
print.default

sloop::s3_methods_class('factor')

x <- structure(1:10, class='test')
t
s3_dispatch(t(x))

  # q 13.6.1
`[.Date`
.Date
sloop::s3_dispatch(as.Date('2020-01-01'))
as.Date(c('2020-01-01','2020-01-01')) %>% class()
as.Date(c('2020-01-01','2020-01-01')) %>% .[1] %>% class()
sloop::s3_dispatch(as.Date(c('2020-01-01','2020-01-01'))[1])

    # [.Date will not preserve the attributes
a <- structure(1:4, test = 'test', class=c('myDate', 'Date'))
attributes(a[1])

  # q 13.6.3
generic2 <- function(x) UseMethod('generic2')
generic2.a1 <- function(x) 'a1'
generic2.a2 <- function(x) 'a2'
generic2.b <- function(x) {
  class(x) <- 'a1'
  print(.Class)
  print(class(x))
  NextMethod()
}

generic2(structure(list(), class=c('b', 'a2')))
    # The reason is because `NextMethod` use a special global '.Class' variable to find the next method which
    # has already been created by the time generic.b has been called


### Chapter 14
library(R6)
Accumulator <- R6::R6Class('Accumulator', list(
  sum = 0,
  add = function(x = 1){
    self$sum <- self$sum + x
    invisible(self)
  },
  print = function() {
    cat('This is the sum: ', self$sum)
  }
))

x <- Accumulator$new()
sloop::s3_methods_class(print)
names(x)

Accumulator

### Chapter 15
  # q 15.2.1
a <- lubridate::period(1)
str(a)


sloop::ftype(print.factor)
sloop::ftype |> View()
is |> View()
extends(class(print.factor))
methods(class='Period')

sloop::is_s3_generic

codetools::findGlobals(`[`, merge=F)
show
showMethods(show)


setClass('Person',
         slots = c(name='character', age = 'numeric'),
         prototype = list(name = NA_character_, age=NA_real_)
)
me <- new('Person', name ='Ian')
setGeneric('name<-', function(x,value) standardGeneric('name<-'))
setMethod('name<-', 'Person', function(x,value){
  x@name <- value
  validObject(x)
  x
})

typeof(me)
name(me) <- 'Ian'

args(getGeneric('name<-'))
args

length_per <- function(x) '180 cm'
length(me)
sloop::ftype(length.Person)
sloop::ftype %>% View()
sloop::is_s3_generic |> View()
sloop::is_s3_generic('length.Pe')
sloop::caller_env()

parent.frame()
a <- function() {rlang::current_env()}
b <- rlang::enexpr(length.Person)
length.Person |> as.character()
