#Math 23c Script 4M-Integration.R

#Last modified: February 11, 2018

#Topic 1: Darboux integrals -- creating a partition
a <- 1.5; b = 2.8   #Lower and upper bounds of integration
f <- function(x) x+sin(10*x)/2   #the function to integrate
plot(f, a, b)

Psub <- runif(8, a, b);Psub #generate 8 subdivision points in (a,b)
P <- sort(c(a,b,Psub));P     #add endpoints and put the division points in order
axis(1, at = P, labels = FALSE, col = "red")
abline(v=P, col = "red")

#The R function "optimize" finds the maximum or minimum of a function on a interval.
#It returns a 2-component vector with the min or max value ("objective")
#and the x where it is achieved
m <- optimize(f, c(P[2], P[3])); m$minimum; abline(h=m$objective, col = "green");
abline(v=m$minimum, col = "green")
M <- optimize(f, c(P[2], P[3]),maximum = TRUE); M$objective;
abline(h=M$objective, col = "blue");abline(v=M$maximum, col = "blue")
#Now we can calculate the lower Darboux sum for a given partition
DarbouxL <- function(f, Part) {
  Dsum <- 0
  for (i in 1:(length(Part)-1)) {
    Dsum <- Dsum + (Part[i+1]-Part[i])*optimize(f, c(Part[i], Part[i+1]) )$objective
  }
  return(Dsum)
}
integrate(f,a,b)    #the correct value
DarbouxL(f,P);       #too small
Qsub <- runif(5, a, b) #generate 5 subdivision points in (a,b)
Q <- sort(c(a,b,Qsub))    #put the division points in order
axis(1, at = Q, labels = FALSE, col = "magenta")
abline(v=Q, col = "magenta")
DarbouxL(f,Q)       #also too small but not necessarily less than DarbouxL(f,P)
PQ <- sort(c(a,b,Psub,Qsub))     #union of partitions P and Q;
DarbouxL(f,PQ)    #guaranteed larger than either for P or for Q

#we can calculate the upper Darboux sum similarly
DarbouxU <- function(f, Part) {
  Dsum <- 0
  for (i in 1:(length(Part)-1)) {
    Dsum <- Dsum + (Part[i+1]-Part[i])*optimize(f, c(Part[i], Part[i+1]),maximum = TRUE )$objective
  }
  return(Dsum)
}
integrate(f,a,b)    #the correct value
DarbouxU(f,P); DarbouxU(f,Q)       #should both be too large
DarbouxU(f,PQ)    #guaranteed smaller than either for P or for Q
#If we use a large number of subdivisions the Darboux sums get close to the integral.
Psub <- runif(800, a, b) #generate 800 subdivision points in (a,b)
P <- sort(c(a,b,Psub))     #add endpoints and put the division points in order
integrate(f,a,b)    #the correct value
DarbouxL(f,P)     #slightly too small
DarbouxU(f,P)     #slightly too large
#This is not an efficient way to do numerical integration!

#Topic 2: Riemann integrals, done with n equal subdivisions: example for n = 2
seq( from = a, to = b, length.out = 3)
#Left Riemann sum
RiemannL <- function(f, a, b, n){
  Part <- seq( from = a, to = b, length.out = n+1)
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*f(Part[i])
  }
  return(Rsum)
}
RiemannR <- function(f, a, b, n){
  Part <- seq( from = a, to = b, length.out = n+1)
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*f(Part[i+1])
  }
  return(Rsum)
}
RiemannTrap <- function(f, a, b, n){
  Part <- seq( from = a, to = b, length.out = n+1)
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*(f(Part[i])+f(Part[i+1]))/2
  }
  return(Rsum)
}
RiemannMid <- function(f, a, b, n){
  Part <- seq( from = a, to = b, length.out = n+1)
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*f(Part[i]+(b-a)/(2*n))
  }
  return(Rsum)
}
RiemannSimpson <- function(f, a, b, n){
  Part <- seq( from = a, to = b, length.out = n+1)
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*(4*f(Part[i]+(b-a)/(2*n))+f(Part[i])+f(Part[i+1]))/6
  }
  return(Rsum)
}

integrate(f,a,b)    #the correct value
RiemannL(f,a,b,10)
RiemannR(f,a,b,10)
RiemannTrap(f,a,b,10); (RiemannL(f,a,b,10)+RiemannR(f,a,b,10))/2
RiemannMid(f,a,b,10)
RiemannSimpson(f,a,b,10); integrate(f,a,b)     #really close

#Topic 3: The gamma function
curve(gamma(x), from = 0.15, to = 4.2 )
abline(v=c(seq(from = 0.5, to = 4.0, by = 0.5)),col = "red")
#For integers, gamma(n) = factorial(n-1)
abline(h=factorial(c(0,1,2,3)),col = "blue")
#For half integers, gamma(n+1/2) is a rational multiple of sqrt(pi)
abline(h=sqrt(pi)*c(1,0.5,0.5*1.5,0.5*1.5*2.5),col = "green")

#R does a good job with improper integrals
my.gamma = function(r){
  f <- function(x) x^(r-1)*exp(-x)
  integrate(f, 0, Inf)
}
my.gamma(3)
my.gamma(1/2); sqrt(pi)

#R does not do so well with the Fresnel improper integral
f <- function(x) sin(x^2)
integrate(f, 0, Inf)
for (i in (1:100)*pi) {
  print (integrate(f, 0, sqrt(i)))
}
#This is a very slowly convergent sequence!

#Topic 4: Checking normalization and expectation for some density functions
#Exponential
curve(dexp(x,2), 0, 4)
f <- function(x) dexp(x,2)
integrate(f, 0, Inf)
f <- function(x) x*dexp(x,2)
integrate(f, 0, Inf)

#Standard normal
curve(dnorm(x), -4, 4)
f <- function(x) dnorm(x)
integrate(f, -Inf, Inf)
f <- function(x) x*dnorm(x)
integrate(f, -Inf, Inf)

#Gamma with r = 3, lambda = 2
curve(dgamma(x,3,rate = 2), 0, 4)
f <- function(x) dgamma(x,3,rate = 2)
integrate(f, 0, Inf)
#Theory says the expectation is r/lambda.
f <- function(x) x*dgamma(x,3,rate = 2)
integrate(f, 0, Inf)

#Chi square with 4 degrees of freedom
curve(dchisq(x, df=4), 0, 12)
curve(dgamma(x,2, rate = 1/2),col = "red", add = TRUE)    #the same function!
f <- function(x) dchisq(x, df=4)
integrate(f, 0, Inf)
f <- function(x) x*dchisq(x, df=4)
integrate(f, 0, Inf)

#Chi square with 1 degree of freedom
curve(dchisq(x, df=1), 0, 6)    #an unbounded function
curve(dgamma(x,1/2, rate = 1/2),col = "red", add = TRUE)    #the same function!
f <- function(x) dchisq(x, df=1)
integrate(f, 0, Inf)     #this integral is improper at both endpoints!
#For the gamma version, the expectation formula is r/lambda.
f <- function(x) x*dchisq(x, df=1)
integrate(f, 0, Inf)



