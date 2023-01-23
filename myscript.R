#########################
## Accept-reject for
## Beta(4,3) distribution
## Using U(0,1) proposal
#########################
set.seed(1)
beta_ar <- function() 
{
  c <- 60 *(3/5)^3 * (2/5)^2 
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    
    # fill in the lines of code here to implement
    U <- runif(1)
    prop <- runif(1)
    ratio <- dbeta(prop, 4, 3)/c
    
    
    if(U <= ratio)
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}


### Obtaining 10^4 samples from Beta() distribution
N <- 1e4
samp <- numeric(length = N)
counts <- numeric(length = N)
for(i in 1:N){
  rep <-   beta_ar()
  samp[i] <-  rep[1]
  counts[i] <- rep[2]
}

# Make a plot of the estimated density from the samples
# versus the true density
x <- seq(0, 1, length = 500)
plot(density(samp), main = "Estimated density from 1e4 samples")
lines(density(rbeta(500,4,3))   , col = "red", lty = 2) ## Complete this
legend("topleft", lty = 1:2, col = c("black", "red"), legend = c("AR", "truth"))

# This is c
(c <- 60 *(3/5)^3 * (2/5)^2)

# This is the mean number of loops required
mean(counts)

#They should be almost the same!






#########################
## Accept-reject for
## Beta(2,0.1) distribution
#########################
set.seed(1)
beta_ar <- function() 
{
  c <- gamma(2+0.1)/(gamma(2)*gamma(0.1)*0.1)
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    
    # fill in the lines of code here to implement
    U <- runif(1)
    prop <- 1 - (1 - U)^(1/0.1)
    q_y <- 0.1*(1 - prop)^(1/0.1)
    ratio <- exp(log(dbeta(prop, 2, 0.1))-log(c)-log(q_y))
    
    u_check <- runif(1)
    if(u_check <= ratio)
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}


### Obtaining 10^4 samples from Beta() distribution
N <- 1e4
samp <- numeric(length = N)
counts <- numeric(length = N)
for(i in 1:N){
  rep <-   beta_ar()
  samp[i] <-  rep[1]
  counts[i] <- rep[2]
}

# Make a plot of the estimated density from the samples
# versus the true density
x <- seq(0, 1, length = 50000)
plot(density(samp), main = "Estimated density from 1e4 samples")
lines(x,dbeta(x,2,0.1)   , col = "red", lty = 2) ## Complete this
legend("topleft", lty = 1:2, col = c("black", "red"), legend = c("AR", "truth"))

# This is c
c <- gamma(2+0.1)/(gamma(2)*gamma(0.1)*0.1)

# This is the mean number of loops required
mean(counts)

#They should be almost the same!






#############################################################################
set.seed(1)
gamma_ar <- function() 
{
  c <- ((3^4)*(((4-1)/(3-(3/4)))^(4-1))*exp(1-4))/((3/4)*gamma(4)) 
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    
    # fill in the lines of code here to implement
    U <- runif(1)
    prop <- -(1/(3/4))*log(1 - U)
    f_y <- (3^4)
    
    
    if(U <= ratio)
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}


### Obtaining 10^4 samples from Beta() distribution
N <- 1e4
samp <- numeric(length = N)
counts <- numeric(length = N)
for(i in 1:N){
  rep <-   beta_ar()
  samp[i] <-  rep[1]
  counts[i] <- rep[2]
}

# Make a plot of the estimated density from the samples
# versus the true density
x <- seq(0, 1, length = 500)
plot(density(samp), main = "Estimated density from 1e4 samples")
lines(density(rbeta(500,4,3))   , col = "red", lty = 2) ## Complete this
legend("topleft", lty = 1:2, col = c("black", "red"), legend = c("AR", "truth"))

# This is c
(c <- 60 *(3/5)^3 * (2/5)^2)

# This is the mean number of loops required
mean(counts)

#They should be almost the same!









##################################
## Accept-reject for obtaining
## sample uniformlyfrom a standard circle
## using a box as a proposal
##############################
set.seed(1)
circle_ar <- function() 
{
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    prop <- c(2*runif(1) - 1, 2*runif(1) - 1) 
    
    if(prop[1]^2 + prop[2]^2 <= 1) # fill condition
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}

# Simulation 10^4 samples from circle
N <- 1e4
samp <- matrix(0, ncol = 2, nrow = N)
counts <- numeric(length = N)
for(i in 1:N)
{
  foo <- circle_ar()  # I use foo as a dummy name
  samp[i,] <- foo[1:2]
  counts[i] <- foo[3]
}


4/pi
# [1] 1.27324
mean(counts)  # should be very close

# Plotting the obtained samples
# no paritcular part of the circle is favored more
# than any other part.
plot(samp[,1], samp[,2], xlab = "x", ylab = "y", 
     main = "Uniform samples from a circle", asp = 1)





























