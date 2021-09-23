#Math 23c Script 4D-DensityFunctions.R

#Last modified: February 11, 2018

#Topic 1: Monte Carlo Integration
#Since we want to generalize to n dimensions, use the dyadic approach
#In each dyadic interval, we choose an evaluation point at random.

RiemannDyadic <- function(f, a, b, N){
  ff <- function(x) f(x)*((x >=a)&(x <=b))   #make the function zero outside [a,b]
  aInt <- floor(a); bInt <- ceiling(b)    #divide up interval with integer endpoints
  Part <- seq( from = aInt, to = bInt, length.out = (bInt-aInt)*2^N+1)
  Rsum <- 0      #to tally the Riemann sum
  for (i in 1:(length(Part)-1)) {
    xEval <- runif(1,min=Part[i],max=Part[i+1])  #choose a random point
    Rsum <- Rsum + ff(xEval)/(2^N)   #evaluate at a random point
  }
  return(Rsum)
}

f <- function(x) x^2
(1.7^3-0.4^3)/3      #the exact answer for the integral
RiemannDyadic(f, 0.4, 1.7, 8)
RiemannDyadic(f, 0.4, 1.7, 8)    #different result every time

#This is better than using either the lower or upper dyadic sum but worse than Simpson's Rule.
#Warning: Simpson's Rule has problems when integrating a discontinuous function.

#An alternative is simply to choose points at random
RiemannMonteCarlo <- function(f, a, b, N){
  ff <- function(x) f(x)*((x >=a)&(x <=b))   #make the function zero outside [a,b]
  aInt <- floor(a); bInt <- ceiling(b)
  Rsum <- 0
  for (i in 1:((bInt-aInt)*2^N)) {
    xEval <- runif(1,min=aInt,max=bInt)
    Rsum <- Rsum + ff(xEval)/(2^N)   #evaluate at a random point
  }
  return(Rsum)
}
(1.7^3-0.4^3)/3      #the exact answer for the integral
RiemannMonteCarlo(f, 0.4, 1.7, 8)
RiemannMonteCarlo(f, 0.4, 1.7, 8)    #different result every time

#Topic 2: Using the Monte Carlo approach to solve problems
#This approach may be the only way to deal with functions of several random variables.
#In effect you are fabricating data and then analyzing it.
#Simple example: generate x and y uniformly on [0,1]: what is the expectation of |x-y|?
N <- 10000; diff <- numeric(N)
for (i in 1:N){
  samp <- runif(2, min = 0, max = 1)
  diff[i] <- abs(samp[2]-samp[1])
}
mean(diff)    #exact answer is 1/3
hist(diff, breaks = "FD", probability = TRUE)
#In this simple case, you can guess that the density function is 2(1-x).

#Messy example: generate three random numbers uniformly on [0,1].
#what is the expectation of the square of the median times (maximum - minimum)
N <- 10000; value <- numeric(N)
for (i in 1:N){
  samp <- runif(3, min = 0, max = 1)
  s <- sort(samp)
  value[i] <- (s[2]^2)*(s[3]-s[1])
}
mean(value)    #exact answer is unknown to me
hist(value, breaks = "FD")


#Topic 3: Testing the hypothesis of a continuous distribution
#Another example from Chihara and Hesterberg
#Here is a table with 100 observations, placed into four "bins."
TheData<-c(rep("0-0.25",30),rep("0.25-0.75",30),rep("0.75-1.25",22),rep("1.25-Inf",18)); TheData
Numbers=table(TheData); Numbers #top line of table, middle of p. 65 of Chihara-Hesterberg

#Since the density is falling off, the data may come from an exponential distribution.
#We can calculate the expected number in each bin by integration.
pBin1 <- integrate(dexp, 0, 0.25)$value; 100*pBin1
pBin2 <- integrate(dexp, 0.25, 0.75)$value; 100*pBin2
pBin3 <- integrate(dexp, 0.75, 1.25)$value; 100*pBin3
pBin4 <- integrate(dexp, 1.25, Inf)$value; 100*pBin4
Expected <- 100*c(pBin1,pBin2,pBin3,pBin4); Expected
#Of course, we could also get the answer from the distribution function.
100*c(pexp(0.25),pexp(0.75)-pexp(0.25),pexp(1.25)-pexp(0.75),1-pexp(1.25)); Expected
#Now we have observed and expected values and can compute the chi-square statistic.
Chi2 <-sum((Numbers-Expected)^2/Expected); Chi2

#How probable is this large a value, according the chi-square distribution?
#Since there are only three independent values, use three degrees of freedom
Pvalue<- pchisq(Chi2,3,lower.tail = FALSE); Pvalue   
#The chance of this large a discrepancy arising by chance is about 6%

#Alternative approach
#Simulate drawing many samples of 100 from the hypothesized distribution.
N = 10^4; result = numeric(N)
for (i in 1:N){
  expData = rexp(100) #generate 100 random samples from an exponential distribution
  Counts=numeric(4)   #simulate the sort of data we are analyzing
  Counts[1] = sum(expData <= 0.25) 
  Counts[2] = sum((expData > 0.25) & (expData <= 0.75))
  Counts[3] = sum((expData > 0.75) & (expData <= 1.25))
  Counts[4] = sum(expData > 1.25)
  result[i] = sum((Counts-Expected)^2/Expected)
}
hist(result, xlim = c(0,20), breaks = "FD",probability =TRUE)
curve(dchisq(x, df=3), col = "blue", add= TRUE)     #nearly perfect fit, as expected
abline(v = Chi2, col = "red")
sum(result >= Chi2)/N; Pvalue   #should agree closely


