bridgeReg <- function(y, X, alpha, lambda, max.iter, tol)
{
  p <- dim(X)[2]
  iter <- 0
  current <- qr.solve(t(X)%*%X +  lambda*diag(p)) %*% (t(X)%*%y)
  diff <- tol + 1
  
  
  while(diff > tol && iter < max.iter)
  {
    iter <- iter + 1
    if(iter > max.iter)
    {
      print("Maximum iterations reached")
      stop
    }
    # MM steps
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    
    update <- qr.solve((t(X)%*%X)  + (lambda/alpha)*diag(alpha*(abs(current)^(alpha - 2)))) %*% t(X)%*%y # NR update

    diff <- norm(current - update, "2")
    current <- update
    
    
  }
  return(current)
}

set.seed(1)
n <- 100
p <- 5
beta.star <- c(3, 1, .01, -2, -.003)
# X matrix with intercept
X <- cbind(1, matrix(rnorm(n*(p-1)), nrow = n, ncol = (p-1)))
# generating response
y <- X %*% beta.star + rnorm(n)

alpha <- 1.5
lambda <- 2
max.iter <- 100
tol <- 1e-5
bridgeReg(y, X, alpha, lambda, max.iter, tol)



############################################# problem 3

set.seed(1)
n <- 100
p <- 5
beta.star <- c(3, 1, .01, -2, -.003)
# X matrix with intercept
X <- cbind(1, matrix(rnorm(n*(p-1)), nrow = n, ncol = (p-1)))
# generating response
y <- X %*% beta.star + rnorm(n)


max.iter <- 100
tol <- 1e-5
alpha.vec <- seq(1, 2, length = 5)
lambda <- c(.01, .1, 1, 10, 100)

for(i in 1:length(alpha.vec))
{
  for(j in 1:length(lambda))
  {
    bridge.est <- bridgeReg(y, X, alpha.vec[i], lambda[j], max.iter, tol)
    print(bridge.est)
  }

}
