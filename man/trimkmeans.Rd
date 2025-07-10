\name{trimkmeans}
\alias{trimkmeans}
\alias{print.tkm}
\alias{plot.tkm}
\title{Trimmed k-means clustering}
\description{
  The trimmed k-means clustering method by Cuesta-Albertos, Gordaliza
  and Matran (1997). This optimizes the k-means criterion under trimming a
  portion of the points.
}
\usage{
  trimkmeans(data,k,trim=0.1, scaling=FALSE, 
        runs=500, niter1=3, niter2=20, nkeep=5, points=NULL,
        countmode, printcrit, maxit,
        parallel=FALSE, n.cores=-1, trace=0, ...)

  \method{print}{tkm}(x, ...)
  \method{plot}{tkm}(x, data, ...)
}
\arguments{
  \item{data}{matrix or data.frame with raw data}
  \item{k}{integer. Number of clusters.}
  \item{trim}{numeric between 0 and 1. Proportion of points to be trimmed.}
  \item{scaling}{logical. If \code{TRUE}, the variables are centered at their
    means and scaled to unit variance before execution.}
    \item{runs}{The number of random initializations to be performed.}
    
    \item{niter1}{The number of concentration steps to be performed for the nstart initializations.}
    
    \item{niter2}{The maximum number of concentration steps to be performed for the 
    \code{nkeep} solutions kept for further iteration. The concentration steps are 
    stopped, whenever two consecutive steps lead to the same data partition.}
    
    \item{nkeep}{The number of iterated initializations (after niter1 concentration 
    steps) with the best values in the target function that are kept for further iterations}
    
  \item{points}{\code{NULL} or a matrix with k vectors used
    as means to initialize the algorithm. If
    initial mean vectors are specified, \code{runs} should be 1
    (otherwise the same initial means are used for all runs).}
  \item{countmode}{(deprecated) optional positive integer. Every \code{countmode}
    algorithm runs \code{trimkmeans} shows a message.}
  \item{printcrit}{(deprecated) logical. If \code{TRUE}, all criterion values (mean
    squares) of the algorithm runs are printed.} 
    \item{maxit}{(deprecated, use the combination \code{nkeep, niter1 and niter2}) 
    The maximum number of concentration steps to be performed.
    The concentration steps are stopped, whenever two consecutive steps lead
    to the same data partition.}
    \item{parallel}{A logical value, specifying whether the nstart initializations should be done in parallel.}
    
    \item{n.cores}{The number of cores to use when paralellizing, only taken into account if parallel=TRUE.}

    \item{trace}{Defines the tracing level, which is set to 0 by default. Tracing level 1 
    gives additional information on the stage of the iterative process.}
  \item{x}{object of class \code{tkm}.}
  \item{...}{further arguments to be transferred to \code{plot} or
    \code{\link[fpc]{plotcluster}}.} 
}
\details{
    The function \code{trimkmeans()} now calls the function \code{tkmeans()} from 
    the package \code{tclust}. This makes the procedure much faster since
    (a) \code{tkmeans()} is implemented in C++, (b) a new random initialization is introduced
    (see the parameters \code{niter1}, \code{niter2} and \code{nkeep} which replace 
    the previous \code{maxit} and (c) it is posible to run the initialization in parallel
    (see the argument \code{parallel} and \code{ncores}.
    
    
  \code{plot.tkm} calls \code{\link[fpc]{plotcluster}} if the
  dimensionality of the data \code{p} is 1, shows a scatterplot
  with non-trimmed regions if \code{p=2} and discriminant coordinates
  computed from the clusters (ignoring the trimmed points) if \code{p>2}.
}
\value{
  An object of class 'tkm' which is a LIST with components
  \item{classification}{integer vector coding cluster membership with trimmed
    observations coded as \code{k+1}.}
  \item{means}{numerical matrix giving the mean vectors of the k
    classes.}
  \item{disttom}{vector of squared Euclidean distances of all points to
    the closest mean.}
  \item{ropt}{maximum value of \code{disttom} so that the corresponding
    point is not trimmed.}
  \item{k}{see above.}
  \item{trim}{see above.}
  \item{runs}{see above.}
  \item{scaling}{see above.}
}
\references{
  Cuesta-Albertos, J. A., Gordaliza, A., and Matran, C. (1997)
  Trimmed k-Means: An Attempt to Robustify Quantizers,
  Annals of Statistics, 25, 553-576.

}
\author{Christian Hennig
  \email{chrish@stats.ucl.ac.uk}
  \url{http://www.homepages.ucl.ac.uk/~ucakche/}
}
\seealso{ \code{\link[fpc]{plotcluster}}
}
\examples{
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
  tkm1 <- trimkmeans(X, k=3, trim=0.1, runs=5)
## runs=5 is used to save computing time; runs must be >= nkeep

  print(tkm1)
  plot(tkm1,X)
}
\keyword{multivariate}
\keyword{cluster}

