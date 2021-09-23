#Math 23c - Script 5D-ContinuousModels

#Last modified: January 19, 2018

#Topic 1: distribution of a function of a random variable
#Given one of the built-in density functions in R (unif, exp, gamma, chisq)
#we can generate random samples, then apply a function to them.

#Example: Generate X with a uniform distribution on (0,1],
#then take minus the logarithm to get Y = -log X
#In other words X = exp(-Y) and we know P(x > x) = 1-x since X is uniform
#Thus P(exp(-Y) > x) = P(-Y > log x) = P(Y < log x) = 1-x
#Set x = exp(z) to conclude P(Y < z) = 1 - exp(z); Y has an exponential distribution
#Let's test this by sampling.

Xsmp = runif(10000, min = 0, max = 1); head(Xsmp)   #10000 uniform samples for X
#Convert them into 10000 samples for Y
YSmp = -log(Xsmp); head(YSmp)      #log is a vectorized function
hist(YSmp, breaks = "FD", probability = TRUE)
curve(dexp, add = TRUE, col = "red", lwd = 2)  #Y has an exponential distribution

#Here, again, is that table with 100 observations, placed into four "bins."
TheData<-c(rep("0-0.25",30),rep("0.25-0.75",30),rep("0.75-1.25",22),rep(">1.25",18))
Numbers=table(TheData); Numbers    #top line, middle of page 65 of Chihara-Hesterberg
#If the observations Y are from an exponential distribution,
#then X=exp(-Y) is from a uniform distribution
#Compute the bin sizes for X
bins <- c(1-exp(-.25),exp(-.25)-exp(-.75),exp(-.75)-exp(-1.25),exp(-1.25)); bins
#For a uniform distribution, the expected number of observations
#is proportional to the bin width
Expected <- 100*bins; Expected
#For comparison, here are the numbers from script 4D
c(22.11992, 30.64342, 18.58618, 28.65048)

#Here is a case where we have not yet seen the density function.
#Generate three samples from a uniform distribution in [0,1] and choose the largest.
max(runif(3,0,1))
#Let's do this many times and plot a histogram
N <- 10000; maxes <- numeric()
for (i in 1:N) {
  maxes[i] <- max(runif(3,0,1))
}
hist(maxes, breaks = "FD", probability = TRUE)
#The histogram looks like a graph of the function 3x^2
f <- function(x)    3*x^2
curve(f, from = 0, to = 1, add= TRUE, col = "red")
#This distribution is available in R -- we just haven't met it yet
curve(dbeta(x, 3,1), from = 0, to = 1, add= TRUE, col = "blue")
#Let's figure out what function to apply to make the distribution uniform
#For X, the density function is 3x^2 and the distribution function is x^3
#So P(X < x) = x^3. Set z = x^3.
#Then P(X^3 < x^3) = x^3   or P(X^3 < z) = z
#In other words, the random variable Y = X^3 should have a uniform distribution. Let's try it.
N <- 10000; cubemaxes <- numeric()
for (i in 1:N) {
  cubemaxes[i] <- max(runif(3,0,1))^3
}
hist(cubemaxes, breaks = "FD", probability = TRUE)    #looks pretty uniform

#Topic 2: Using the normal distribution as a model
#Look at some data about babies born in Texas in 2004
TXBaby <- read.csv("TXBirths2004.csv");  head(TXBaby)
ounces <- TXBaby$Weight     #extract the weight column as a vector
hist(ounces, breaks = "FD", probability = TRUE)    #looks kind of Gaussian
nbaby <- length(ounces)    #the number of babies
#To find the best fitting normal distribution, compute the mean and variance of the data
mu <- mean(ounces); mu
sigma <- sd(ounces)     #estimates square root of the population variance
curve(dnorm(x, mu, sigma), from = 1000, to = 5000, add = TRUE, col = "red")
#Again, we will compare observed and expected counts for bins of data
#It is easy to use deciles of the normal distribution
n1 <- qnorm(0.1, mu, sigma); n1    #10% of the normal distribution lies below this value
pnorm(n1, mu, sigma)       #just a check
mean(ounces <= n1)         #about 8% of babies in this bin -- not too bad
#Now make a vector of deciles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu, sigma); dec   #11 bins
Exp <- rep(nbaby/10,10); Exp     #expected babies per bin
#Now we can count how many babies are in each bin
binbabies <- numeric(10)
for (i in 1:10)  
  binbabies[i] <- sum((ounces >= dec[i]) & (ounces <= dec[i+1]) ); binbabies
#Test for uniformity using chi square.
Chi2 <- sum((binbabies - Exp)^2/Exp); Chi2
#We estimated two parameters, which costs two degrees of freedom
curve(dchisq(x, df = 7), from = 0, to = 120 )
abline(v=Chi2, col = "red")
#The probability of such a large chi-square value is tiny
#The normal distribution was not a good model

#Now repeat, using average weight for a sample of 25 babies
N <- 2000; xbars <- numeric(N)
for (i in 1:N) xbars[i] <- mean(sample(ounces,25))
hist(xbars, breaks = "FD", probability = TRUE)    #looks more Gaussian
#For the best fit, compute the mean and variance of the data
mu25 <- mean(xbars); mu25; mu    #same as before
sig25 <- sd(xbars); sig25; sigma     #smaller by a factor of 5 because we used a mean of 25 samples
curve(dnorm(x, mu25, sig25), from = 2800, to = 3800, add = TRUE, col = "red")
#We want to compare observed and expected counts for bins of data
#Again use deciles of the normal distribution
n1 <- qnorm(0.1, mu25, sig25); n1    #10% of the normal distribution lies below this value
mean(xbars <= n1)         #almost exactly 10% of sample means are in this bin
#Make a vector of deciles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu25, sig25); dec   #11 bins
Exp <- rep(N/10,10); Exp     #expected means per bin
#Count how many means are in each bin
binmeans <- numeric(10)
for (i in 1:10)  binmeans[i] <- sum((xbars >= dec[i]) & (xbars <= dec[i+1]) ); binmeans
#This time the agreement looks much better
#Test for uniformity using chi square.
Chi2 <- sum((binmeans - Exp)^2/Exp); Chi2
#We estimated two parameters, which costs two degrees of freedom
curve(dchisq(x, df = 7), from = 0, to = 20 )
abline(v = Chi2, col = "red")
#This time the calculated chi-square value could easily have arisen by chance
#This result illustrates the central-limit theorem:
#Start with any distribution of finite variance and construct the mean of many samples
#The sample means will have a distribution that is approximately normal
#We need to prove this theorem!

#Topic 3: An alternative approach: compare quantiles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu, sigma); dec #deciles for normal distribution
#Compare with quantiles for the baby weights
bquant <- quantile(ounces, seq(0.0, 1, by = 0.1), type = 2); bquant
#Now plot one against the other
plot(dec, bquant, xlim = c(2500, 4100), ylim = c(2500, 4100))
curve(x+1-1, 2500, 4100, add = TRUE)   #not a great match to a straight line

#Try again, using the sample means instead of individual weights
dec <- qnorm(seq(0.0, 1, by = 0.1), mu25, sig25); dec   #deciles for normal distribution
bquant <- quantile(xbars, seq(0.0, 1, by = 0.1), type = 2); bquant
#Plot one against the other
plot(dec, bquant, xlim = c(3100, 3500), ylim = c(3100, 3500))
curve(x+1-1, 3100, 3500, add = TRUE)   #much better match to a straight line

#This approach has been automated in R, using lots of quantiles
qqnorm(ounces)    #baby weights are poor match to normal distribution
qqnorm(xbars)    #sample means are good match to normal distribution

