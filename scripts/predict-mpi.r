## Prediction matrix for the `mpi` smooth class... 


Predict.matrix.mpi.smooth<-function(object,data)
## prediction method function for the `mpi' smooth class
{ m <- object$m+1; # spline order, m+1=3 default for cubic spline
  q <- object$df ## +1
  Sig <- matrix(0,q,q)   # Define Matrix Sigma
  # elements of matrix Sigma for increasing smooth
  for (i in 1:q)  Sig[i,1:i] <- 1
  ## find spline basis inner knot range...
  ll <- object$knots[m+1];ul <- object$knots[length(object$knots)-m]
  m <- m + 1
  x <- data[[object$term]]
  n <- length(x)
  ind <- x<=ul & x>=ll ## data in range
  if (sum(ind)==n) { ## all in range
     X <- splines::spline.des(object$knots,x,m)$design
     X <- X[,2:(q+1)]%*%Sig ## X <- X%*%Sig 
  } else { ## some extrapolation needed 
     ## matrix mapping coefs to value and slope at end points...
     D <- splines::spline.des(object$knots,c(ll,ll,ul,ul),m,c(0,1,0,1))$design
     X <- matrix(0,n,ncol(D)) ## full predict matrix
     if (sum(ind)> 0)  X[ind,] <- splines::spline.des(object$knots,x[ind],m)$design ## interior rows
     ## Now add rows for linear extrapolation...
     ind <- x < ll 
     if (sum(ind)>0) X[ind,] <- cbind(1,x[ind]-ll)%*%D[1:2,]
     ind <- x > ul
     if (sum(ind)>0) X[ind,] <- cbind(1,x[ind]-ul)%*%D[3:4,]
     X <- X[,2:(q+1)]%*%Sig 
  }
  X
}
