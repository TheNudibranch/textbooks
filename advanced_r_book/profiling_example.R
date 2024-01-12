f <- function(){
  pause(0.1)
  g()
  h()
}
g <- function() {
  pause(0.1)
  h()
}

h <- function() {
  pause(0.1)
}

i <- function(){
  pause(0.1)
  10
}

j <- function(x){
  x + 10
}