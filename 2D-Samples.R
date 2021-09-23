#Math 23c Script 2D-Samples.R

#Last modified: January 23, 2018

#Topic 1: Reconstructing a population from a sample
#This is a test case: we know the population and try to reconstruct it from the sample
#Population model for an incompetent test-taker: 
#on each question, gets it right with probability p = 0.2
#For a 6-question test, this leads to a binomial distribution with n=6, p=0.2
#Expectation is mu = np = 1.2, variance is npq = 0.96

#Suppose we get data on the scores from five tests
scores <- rbinom(5, 6, 0.2); scores
scores <- rbinom(5, 6, 0.2); scores    #different every time

#We can compute sample statistics
xbar <- mean(scores); xvar <- mean(scores^2)-(mean(scores))^2; popvar <- var(scores)
xbar; xvar; popvar
#Now simulate the result of doing this 10000 times
N = 10000
#Create empty vectors to hold the results
xbars <- numeric(N); xvars <- numeric(N); popvars <- numeric(N)
#Do N simulations
for (i in 1:N){
  scores <- rbinom(5, 6, 0.2);
  xbars[i] <- mean(scores)
  xvars[i] <- mean(scores^2)-(mean(scores))^2
  popvars[i] <- var(scores)
}
length(xbars); head(xbars); head(xvars) #We have some long vectors
#Did we get a good estimate for the mean?
mean(xbars); 1.2     #Yes
#Did xvars give a good estimate for the variance?
mean(xvars); 0.96     #No -- too small -- it's a biased estimate
#Did popvars give a good estimate for the variance?
mean(popvars); 0.96     #Yes -- an unbiased estimate -- proof in the lecture notes

#Look at a histogram of the results
hist(xbars, breaks = "FD")
abline(v=1.6, col = "red")
#This shows that observing xbar = 1.6 does not rule out p = 0.2

#If we know nothing about the underlying population, treat the sample as a population
#Let's try this approach with a sample of 20
scores20 <- rbinom(20, 6, 0.2); scores20; mean(scores20)
barplot(scores20)   #does not look very binomial
#Treat this as a population and take a sample of 5 with replacement
scores <- sample(scores20, 5, replace = TRUE); scores
#We can do this 10000 times and look at the sample means
sbars <- numeric(N)   #empty vector to hold the mean for 5 samples
for (i in 1:N){
  scores <- sample(scores20, 5, replace = TRUE)
  sbars[i] <- mean(scores)
}
length(sbars); head(sbars) #We have some long vectors
#Did we get a good estimate for the mean?
mean(sbars); mean(scores20)     #close to the mean of our sample of 20


#Look at a histogram of the results
hist(sbars, breaks = "FD")
#Bootstrap approach reconstructs the shape of the histogram 
#that we got by sampling from the population.
#Of course, the result will be different for each sample of 20.
#if the mean or variance of the sample is wrong, bootstrapping will not fix it.


#Topic 2: A permutation test where we know about the underlying population
#Again we get results from a 5-question test
#Suppose Elm Yard studies harder and is correct with probability pElm = 0.7.
#The rest of the Yard is correct with probability pYard = 0.6.
#Let's fabricate a set of 90 sample test results.
pElm = 0.7; pYard = 0.6
Yard <- c(rep("E",30),rep("Y",60)); Yard
score <- c(rbinom(30, 5, pElm), rbinom(60, 5, pYard)); score
ElmYard <- data.frame(Yard, score)  #"E" and "Y" are paired with Elm and Yard scores
write.csv(ElmYard,"ElmYard.csv")
#Calculate the average score for Elm and for the rest of the Yard
ElmAvg <- sum(score*(Yard=="E"))/sum(Yard=="E"); ElmAvg
YardAvg <- sum(score*(Yard=="Y"))/sum(Yard=="Y"); YardAvg
ElmAvg - YardAvg      #very likely to be positive
#Elm did better, but is the difference significant?
#Since we know the population, we can repeat this simulation 10000 times
N <- 10000
diffs <- numeric(N)    #empty vector to hold score differences
for (i in 1:N){
  score <- c(rbinom(30, 5, pElm), rbinom(60, 5, pYard)); score
  ElmAvg <- sum(score*(Yard=="E"))/sum(Yard=="E"); ElmAvg
  YardAvg <- sum(score*(Yard=="Y"))/sum(Yard=="Y"); YardAvg
  diffs[i] = ElmAvg - YardAvg
}
mean(diffs); 5*(pElm-pYard)
hist(diffs, breaks = "FD")
abline(v = 0, col = "red")
#It's clear that Elm could have had a lower average score
mean((diffs < 0))    #happens about 1.6% of the time

#Topic 3: a permutation test where we use nothing but one sample
#If we have nothing but a set of test results, treat the sample as a population
#Replace Elm with a random sample of 30 and look at the difference
#Do this many times, and plot a histogram of the results
#Reload our fabricated data as if it were real data
EY <- read.csv("ElmYard.csv")
#Calculate the observed score difference
ElmAvg <- sum(EY$score*(EY$Yard=="E"))/sum(EY$Yard=="E"); ElmAvg
YardAvg <- sum(EY$score*(EY$Yard=="Y"))/sum(EY$Yard=="Y"); YardAvg
observed <- ElmAvg - YardAvg; observed

#Now replace the Elm scores with a random sample of 30 scores
Yard <- sample(EY$Yard); Yard   #this permutes the original vector randomly
sum(Yard == "E")   #we still have 30 E's but they will match up with random scores
ElmAvg <- sum(EY$score*(Yard=="E"))/sum(Yard=="E"); ElmAvg #from a random subset of 30
YardAvg <- sum(EY$score*(Yard=="Y"))/sum(Yard=="Y"); YardAvg
ElmAvg - YardAvg; #this is as likely to be negative as positive
#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Yard <- sample(EY$Yard); #permute the Yard vector
  ElmAvg <- sum(EY$score*(Yard=="E"))/sum(Yard=="E"); ElmAvg
  YardAvg <- sum(EY$score*(Yard=="Y"))/sum(Yard=="Y"); YardAvg
  diffs[i] = ElmAvg - YardAvg
}
mean(diffs) #should be close to zero
hist(diffs, breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability that a difference this large could have arisen with a random subset?
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue

#Topic 4: A permutation test: we have a sample but know nothing about the population
#These are data collected by a student in a Minneapolis pub
#The data are analyzed in Chihara and Hesterberg, 
#Mathematical Statistics with Resampling and R
BW <- read.csv("Beerwings.csv"); BW
sum(BW$Gender == "M");sum(BW$Gender == "F")
#Calculate the observed beer consumption difference by gender
MaleAvg <- sum(BW$Beer*(BW$Gender=="M"))/sum(BW$Gender=="M"); MaleAvg
FemaleAvg <- sum(BW$Beer*(BW$Gender=="F"))/sum(BW$Gender=="F"); FemaleAvg
observed <- MaleAvg - FemaleAvg; observed     #the men drank more beer

#Now replace Male with a random sample of 15 customers
Gender <- sample(BW$Gender); Gender   #permuted gender column
sum(Gender == "M")  #still 15 men but they will match up with random beer consumption
MaleAvg <- sum(BW$Beer*(Gender=="M"))/sum(Gender=="M"); MaleAvg
FemaleAvg <- sum(BW$Beer*(Gender=="F"))/sum(Gender=="F"); FemaleAvg
MaleAvg - FemaleAvg    #as likely to be negative or positive
#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Gender <- sample(BW$Gender); Gender   #permuted gender column
  MaleAvg <- sum(BW$Beer*(Gender=="M"))/sum(Gender=="M"); MaleAvg
  FemaleAvg <- sum(BW$Beer*(Gender=="F"))/sum(Gender=="F"); FemaleAvg
  diffs[i] <- MaleAvg - FemaleAvg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero
hist(diffs, breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue







