## This is a test function created to understand the correct way to create functions in R
## The first function generate random normal variables and calculate the mean of theM


myfunction <- function (){
  x <- rnorm(100)
  mean(x)
}
second <- function (x){
  x + rnorm(length(x))
}
