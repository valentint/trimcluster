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
  trimkmeans(data,k,trim=0.1, scaling=FALSE, runs=100, points=NULL,
                       countmode=runs+1, printcrit=FALSE,
                       maxit=2*nrow(as.matrix(data)))

  \method{print}{tkm}(x, ...)
  \method{plot}{tkm}(x, data, ...)
}
\arguments{
  \item{data}{matrix or data.frame with raw data}
  \item{k}{integer. Number of clusters.}
  \item{trim}{numeric between 0 and 1. Proportion of points to be trimmed.}
  \item{scaling}{logical. If \code{TRUE}, the variables are centered at their
    means and scaled to unit variance before execution.}
  \item{runs}{integer. Number of algorithm runs from initial
    means (randomly chosen from the data points).}
  \item{points}{\code{NULL} or a matrix with k vectors used
    as means to initialize the algorithm. If
    initial mean vectors are specified, \code{runs} should be 1
    (otherwise the same initial means are used for all runs).}
  \item{countmode}{optional positive integer. Every \code{countmode}
    algorithm runs \code{trimkmeans} shows a message.}
  \item{printcrit}{logical. If \code{TRUE}, all criterion values (mean
    squares) of the algorithm runs are printed.} 
  \item{maxit}{integer. Maximum number of iterations within an algorithm
    run. Each iteration determines all points which
    are closer to a different cluster center than the one to which they are
    currently assigned. The algorithm terminates if no more points have
    to be reassigned, or if \code{maxit} is reached.}
  \item{x}{object of class \code{tkm}.}
  \item{...}{further arguments to be transferred to \code{plot} or
    \code{\link[fpc]{plotcluster}}.} 
}
\details{
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
  tkm1 <- trimkmeans(X,k=3,trim=0.1,runs=3)
# runs=3 is used to save computing time.
  print(tkm1)
  plot(tkm1,X)
}
\keyword{multivariate}
\keyword{cluster}

