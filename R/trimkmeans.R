trimkmeans <- function(data, k, trim=0.1, scaling=FALSE, 
            runs=500, niter1=3, niter2=20, nkeep=5, points=NULL,
            countmode, printcrit, maxit, 
            parallel=FALSE, n.cores=-1, trace=0, ...){

    data <- as.matrix(data)
    n <- nrow(data)
    nin <- ceiling((1-trim)*n)

    if(!missing(maxit)) {
        warning("The parameter 'maxit' is deprecated, please read the help and use the combination of 'niter1', 'niter2', 'nkeep'.")
        niter1 <- maxit
    }

##  countmode and printcrit are ignored
    if(!missing(countmode))
        warning("The parameter 'countmode' is deprecated, please use 'trace'.")
    if(!missing(printcrit))
        warning("The parameter 'printcrit' is deprecated, please use 'trace'.")
        
##    tkm <- tkmeans(data, k=k, alpha=trim, scale=scaling, nstart=runs, niter1=maxit, niter2=0, nkeep=1, points=points, ...)
##    tkm <- tkmeans(data, k=k, alpha=trim, scale=scaling, nstart=runs, points=points, ...)
    tkm <- tkmeans(data, k=k, alpha=trim, scale=scaling, 
        nstart=runs, niter1=niter1, niter2=niter2, nkeep=nkeep, points=points, 
        parallel=parallel, n.cores=n.cores, drop.empty.clust=FALSE, trace=trace, ...)

    means=t(tkm$center)
    disttom <- rep(0,n)
    if(scaling) 
        data <- scale(data)
    for(j in 1:n){
        dj <- rep(0,k)
        for(l in 1:k)
            dj[l] <- sum((data[j,] - means[l,])^2)
        disttom[j] <- min(dj)
    }      

    tkm$cluster[tkm$cluster == 0] <- tkm$par$k+1
    out <- list(classification=tkm$cluster, means=t(tkm$center),
              criterion=tkm$obj, disttom=disttom, ropt=sort(disttom)[nin],
              k=tkm$k, trim=tkm$par$alpha, runs=tkm$par$nstart, scaling=tkm$par$scale)
  
    class(out) <- "tkm"
    out
}

print.tkm <- function(x,...){
  cat("* trimmed k-means *\n")
  cat("trim=",x$trim,", k=",x$k,"\n")
  cat("Classification (trimmed points are indicated by ",x$k+1,"):\n")
  print(x$classification)
  cat("Means:\n")
  print(x$means)
  cat("Trimmed mean squares: ",x$criterion,"\n")
  invisible(x)
}

plot.tkm <- function(x,data,...){
  p <- dim(as.matrix(data))[2]
  if (p==1){
    fpc::plotcluster(data,x$classification, ...)
  }
  if (p==2){
# Create the graphs for summarizing results
    plot(data,type="n",main=paste("k = ", x$k, " and trim = ",x$trim ))
    points(data[x$classification==x$k+1,],col=1,pch=3)
    t <- seq(0,2*pi,length=1000)
    j <- 0
    for (i in 1:x$k){
      j <- j+1
      points(data[x$classification==j,],col=j+1)
      circ <- sqrt(x$ropt)*cbind(cos(t),sin(t));
      lines(rep(1,1000)%*%t(x$means[j,])+circ,col=1)
    }
  }
  if (p>2){
    cv <- x$classification<x$k+1
    dcx <- fpc::discrcoord(data[cv,],x$classification[cv])
    dproj <- data %*% dcx$units
    pchs <- c(sapply(1:x$k,toString),"T")
    plot(dproj,col=x$classification,pch=pchs[x$classification],
         main="Discriminant coordinates of clusters (T=trimmed)", ...)
  }
  invisible(x)
}
 
  
  
