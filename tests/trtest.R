## VT::15.01.2020 - this will render the output independent
##  from the version of the package
suppressPackageStartupMessages(library(trimcluster))

  set.seed(10001)
  n1 <-60
  n2 <-60
  n3 <-70
  n0 <-10
  nn <- n1+n2+n3+n0
  pp <- 2
  X <- matrix(rep(0,nn*pp),nrow=nn)
  ii <-0
  for (i in 1:n1){
    ii <-ii+1
    X[ii,] <- c(5,-5)+rnorm(2)
  }
  for (i in 1:n2){
    ii <- ii+1
    X[ii,] <- c(5,5)+rnorm(2)*0.75
  }
  for (i in 1:n3){
    ii <- ii+1
    X[ii,] <- c(-5,-5)+rnorm(2)*0.75
  }
  for (i in 1:n0){
    ii <- ii+1
    X[ii,] <- rnorm(2)*8
  }

  tkm1 <- trimkmeans(X, k=3, trim=0.1, runs=5, scaling=TRUE)
# runs=5 is used to save computing time.
  print(tkm1)
  plot(tkm1, X)

##  With fixed initial values
  set.seed(1)
  k <- 3
  means <- X[sample(nrow(X), k), , drop=FALSE]
  tkm2 <- trimkmeans(X, k=3, trim=0.1, runs=5, points=means)

##  Univariate data
  set.seed(2)
  tkm3 <- trimkmeans(X[, 1, drop=FALSE], k=3, trim=0.1, runs=5)
  plot(tkm3, X[, 1, drop=FALSE])

##  Multivariate data (p > 2)
  set.seed(3)
  X1 <- as.matrix(iris[, 1:4])
  tkm4 <- trimkmeans(X1, k=3, trim=0.1, runs=5)
  plot(tkm4, X1)

## use deprecated arguments  
try(tkm5 <- trimkmeans(iris[, 1:4], k=3, trim=0.1, runs=5, countmode=100, 
    printcrit=TRUE, maxit=1000))

  tkmx1 <- trimcluster:::trimkmeans_orig(X, k=3, trim=0.1, runs=3, scaling=TRUE, countmode=10, printcrit=TRUE)
# runs=3 is used to save computing time.

  set.seed(1)
  k <- 3
  means <- X[sample(nrow(X), k), , drop=FALSE]
  tkmx2 <- trimcluster:::trimkmeans_orig(X, k=3, trim=0.1, runs=3, points=means)
