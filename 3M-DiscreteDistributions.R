#Math 23c Script 3M-DiscreteDistributions.R

#Last modified: February 4, 2018

#Topic 1: Binomial distribution. Toss 6 fair coins.
#There are 2^6=64 elements in the sample space.
HT <- c("H","T")  
#Create a data frame with names
C6 <- expand.grid(coin1=HT, coin2=HT,coin3=HT, coin4=HT,coin5=HT, coin6=HT)
C6$coin1     #each column is a vector
#The number of heads is a random variable
C6$nheads <- (C6$coin1=="H")+(C6$coin2=="H")+(C6$coin3=="H")+
             (C6$coin4=="H")+(C6$coin5=="H")+(C6$coin6=="H")
#Tally the number of rows for each value of this random variable.
table(C6$nheads)
#Convert to a vector of probabilities
probs <- table(C6$nheads)/length(C6$nheads)
barplot(probs)
#There is an easier way.
#Since this is a binomial distribution, R has a built-in function for it
dbinom(0:6,6,0.5)    #initial "d" for "density" but "mass function" is more standard.
#Here is a clever way to compare two barplots side by side - bind together two vectors as rows
barplot(rbind(probs,dbinom(0:6,6,0.5)), beside = TRUE, col = c("red","blue"))

#We make the distribution function by summing the mass function
#Calculate the value for 4, the probability of 4 or fewer heads.
mean(C6$nheads <= 4)
#To get all the values we write our own function
my.pbinom <- Vectorize(function(x) mean(C6$nheads <= x))
my.pbinom(4)   #check that it works
my.pbinom(0:6)  #thanks to Vectorize, it works on vectors too!
pbinom(0:6,6,0.5)   #the built-in function is already vectorized

#Compute expectation by summing over the values: value times mass function
sum(0:6 * dbinom(0:6,6,0.5) )
#Alternative: sum over the sample space (law of unconscious statistician)
mean(C6$nheads)
#Or use the tail-sum theorem!
sum(1-my.pbinom(0:6))
#pbinom has a special syntax for Prob(X > x)
pbinom(4,6,0.5)    #probability that X <= 4)
pbinom(4,6,0.5, lower.tail = FALSE)    #probability that X > 4)
#So use the tail sum theorem like this:
sum(pbinom((0:6),6,0.5, lower.tail = FALSE))

#Calculating the variance directly from the definition.
#We know that the expectation is 3.
sum((0:6-3)*(0:6-3) * dbinom(0:6,6,0.5) )
#Using the more standard formula
sum((0:6)*(0:6) * dbinom(0:6,6,0.5) )-3*3
#Or we can sum over the sample space
mean((C6$nheads-3)*(C6$nheads-3))

#Now we can sample from this distribution
sample(C6$nheads, 1 )   #generates one sample
sample(C6$nheads, 1 )   #generates one sample, probably different
sample(C6$nheads, 1 )   #generates one sample, probably different
sample(C6$nheads, 1 )   #generates one sample, probably different
sample(C6$nheads, 4 )   #generates four samples from different rows
sample(C6$nheads, 4, replace = TRUE )   #four samples, row may repeat
sample(C6$nheads, 100 )  #error because there are only 64 different rows.
#Generate a large vector of samples, which must include duplicates.
s <- sample(C6$nheads, 100 , replace = TRUE)   #generates 100 samples
barplot(table(s)/length(s))   
#Again we compare two barplots side by side - bind together two vectors as rows
barplot(rbind(probs,table(s)/length(s)), beside = TRUE, col = c("red","blue"))
#Repeat with 10000 samples and the frequencies match the probabilities better
s <- sample(C6$nheads, 10000 , replace = TRUE)   #generates 10000 samples
barplot(rbind(probs,table(s)/length(s)), beside = TRUE, col = c("red","blue"))

#Simulate an unfair coin for which the probability of a head is 2/3.
HHT <- c("H", "H","T")  
C6 <- expand.grid(coin1=HHT, coin2=HHT,coin3=HHT, coin4=HHT,coin5=HHT, coin6=HHT) 
C6$nheads <- (C6$coin1=="H")+(C6$coin2=="H")+(C6$coin3=="H")+
             (C6$coin4=="H")+(C6$coin5=="H")+(C6$coin6=="H")
#Count the number of rows for each value of this random variable, the number of heads
table(C6$nheads)
#Convert to a vector of probabilities
probs <- table(C6$nheads)/length(C6$nheads)
barplot(probs)
#Since this is a binomial distribution, R has a built-in function for it
dbinom(0:6,6,2/3)    # third argument is the probability of a head
barplot(rbind(probs,dbinom(0:6,6,2/3)), beside = TRUE, col = c("red","blue"))

#Topic 2: Implementation of the algorithm in the lecture notes
#for constructing a sigma-field
#Select a sample space S, the integers 1, 2, ...10
N <- 10; S <- 1:N
LMin <- 4; LMax <- 6      #range of sizes for subsets
#Generate one subset that must be in the sample space
size <- sample(LMin:LMax,1); size    #the size of the sample
rset <- sample(S,size);rset  #default is "without replacement," so no duplicates
#Now generate M vectors, of length LMin to LMax, that must be in the sigma-field
M <- 4
sets <- vector("list",M)    #create an empty list

for (i in 1:M) {
  size <- sample(LMin:LMax,1)
  sets[[i]] <- sample(S,size)  #use [[i]] to index a list
}
#Look at what we got
for (i in 1:M) {
   print(sets[[i]])
}

#Set up a list to hold the equivalence classes
classes <- vector("list",N)    #create an empty list
#Now use the algorithm described in the lecture notes.
for (k in 1:N) {
  class <- S    #initialize class to include everything
  for (i in 1:M){
    if (is.element(k, sets[[i]]))
      class <- intersect(class, sets[[i]])  #if set includes k, intersect with it
    else class <- setdiff(class,sets[[i]])  #if set does not include k, remove it
  }
  classes[[k]] <- class  #save the resulting equivalence class in our list
  print(c(k, classes[[k]]))
}
#Look at what we got
for (i in 1:M) {
  print(sets[[i]])
}
#Check for yourself that any two classes are equal or disjoint
#If k is never mentioned, you get the class of everything that was not mentioned

#Topic 3: Poisson distribution as a limit
lambda <- 5       #this will always be our expectation
#We can get this expectation from a binomial distribution with n=100, p = 0.05
barplot( dbinom(0:20, 100, 0.05), names.arg = 0:20)
#We can get the same expectation from a binomial distribution with n=1000, p = 0.005
barplot( dbinom(0:20, 1000, 0.005), names.arg = 0:20) #looks much the same
#The limit is a Poisson distribution with lambda = 5
barplot( dpois(0:20, 5), names.arg = 0:20)
#To see the similarity, display them side by side
barplot(rbind(dbinom(0:20, 100, 0.05),dbinom(0:20, 1000, 0.005),dpois(0:20, 5)), beside = TRUE, col = c("red","blue","green"), names.arg = 0:20)

#We can compute the expectation and variance of the Poisson distribution
#R will not do an infinite sum, so try 20 as a large upper limit.
mu <- sum(dpois(0:20, 5)*(0:20)); mu  #should be close to 5
sum(dpois(0:20, 5)*(0:20)*(0:20))- mu^2; #variance should be close to 5
#A larger upper limit improves the result
mu <- sum(dpois(0:50, 5)*(0:50)); mu  #should be very close to 5
sum(dpois(0:50, 5)*(0:50)*(0:50))- mu^2; #variance should be very close to 5
mu <- sum(dpois(0:Inf, 5)*(0:Inf)); mu  #R is unhappy (Inf will work for integrals)

#Topic 4: Geometric distribution - we have the usual set of built-in functions
p <- 0.2    #probability of success on one trial
dgeom(0:20, 0.2)     #gives the number of failures, not the total number of trials!
barplot(dgeom(0:20, 0.2), names.arg = 0:20)
#Calculate the expectation
mu <- sum(dgeom(0:100, 0.2)*0:100);mu #for the correct value 4, use a large upper limit
#Note: the total number of trials, including the final success, is 1/p = 5
#We can also use the tail-sum theorem
pgeom(0:20, 0.2, lower.tail = FALSE)    #probability that X > x
barplot(pgeom(0:20, 0.2, lower.tail = FALSE) , names.arg = 0:20)
sum(pgeom(0:100, 0.2, lower.tail = FALSE))   #the expected number of failures

#Negative binomial distribution, where we can require more than one success
p <- 0.2    #probability of success on one trial
dnbinom(0:20,size = 1, 0.2)     #second argument is the required number of successes
barplot(dnbinom(0:20,size = 1, 0.2), names.arg = 0:20)   #same as geometric
#Suppose we require three successes.
dnbinom(0:50,size = 3, 0.2)     #second argument is the required number of successes
barplot(dnbinom(0:50,size = 3, 0.2), names.arg = 0:50)  
#Calculate the expectation
mu <- sum(dnbinom(0:100,size = 3, 0.2)*0:100); mu #correct value 12 takes a big limit
#We can again use the tail-sum theorem

barplot(pnbinom(0:50,size = 3, 0.2, lower.tail = FALSE) , names.arg = 0:50) #ising probability that X > x
sum(pnbinom(0:100,size = 3, 0.2, lower.tail = FALSE))

#Topic 5: An example where the expectation does not exist
#Start with an urn that contains one white ball, one black ball
#If a white ball is drawn, you win. Otherwise add another black ball
#The probability of winning on the nth draw is given by the function
dBW <- function(x) 1/(x*(x+1))
dBW(2); dBW(1:10)  #this function is even vectorized
#Check that the total probability is close to 1
sum(dBW(1:1000))    #convergence is slow
#Try to calculate expectation
sum(1:1000*dBW(1:1000)); sum(1:2000*dBW(1:2000)) 
#The limit does not exist
#If we try to calculate variance, things are even worse
sum(1:1000*1:1000*dBW(1:1000)); sum(1:2000*1:2000*dBW(1:2000)) 
#In this case we can prove that the series for the expectation diverges
#Suppose that all we can see are samples from this population
#There is no sampling function, but we can use the urn model
rBW <- function() {
  black <- 1;
  while (TRUE) {    #will repeat until we draw the white ball
    if (runif(1, min = 0, max = 1) < 1/(black+1)) return (black)
    else (black <- black +1)
  }
}
#Let's test this by drawing 100 samples
N <- 100; smp <- numeric(N)    #vector to hold the samples
for (i in 1:N){
  smp[i] <- rBW()
}
hist(smp, breaks = "FD")   #usually has a very long tail to the right
mean(smp)

#Ordinarily, if we average 100 samples, the sample means cluster together
#in a band whose width is roughly the square root of the variance.
M <- 200; means100 <- numeric(M)
for (j in 1:M) {
  smp <- numeric(N)    #vector to hold the samples
  for (i in 1:N){
    smp[i] <- rBW()
  }
  means100[j] <- mean(smp)
}
hist(means100, breaks = "FD")   #occasional very large values
means100
