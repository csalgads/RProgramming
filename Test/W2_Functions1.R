add2 <- function(x,y) {
        x+y
}
above10 <- function(x){
        use <- x>10
        x[use]
}
above <- function(x,n = 10){
        use <- x>n
        x[use]
}






columnmean <- function(y, removeNA = TRUE) { # y is going to be a data frame or a matrix 
                                             # an by default will remove NA values  
        nc <- ncol(y)             # calculates the number of columns
        means <- numeric(nc)      # Create a vector means to store the mean of each column
        for (i in 1:nc){          # Loop each column and calculates its mean
                means[i] <- mean(y[,i], na.rm = removeNA) # Assing the mean of each column to vector means
        }
        means                     # Prints mean vector
}
x <- Sys.time()
p <- as.POSIXlt(x)
names(unclass(p))
p$mon

make.power <- function(n) {
        pow <- function(x){ 
                x^n 
        } 
        pow 
}


