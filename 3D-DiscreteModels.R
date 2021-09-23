#Math 23c Script 3D-DiscreteModels.R

#Last modified: February 4, 2018

#Topic 1: Modeling with a binomial distribution 
#Sometimes you believe that an observation comes from a binomial distribution.
#Your scout says, "This rookie is a .300 hitter."
#After 100 at-bats he has only 26 hits and is batting .260
#Should you fire your scout?
#Calculate the probability of 26 or fewer hits for a .300 hitter in 100 at bats.
pbinom(26, 100, 0.3)    #will happen by chance about 22% of the time
#For the uninitiated, a graphical presentation might be better
barplot(dbinom(10:50,100, 0.3),names.arg = 10:50,col =c(rep("red",17), rep("blue",23)))

#Assessing how well observations match expectation
#Given an vector of observed counts and a  vector of expected counts, 
#here is the standard way to assess the discrepancy.
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}
#Simple case: we make five observations, each with an expected value of 100
Expect <- rep(100,5)
#As our observations, take samples from a binomial distribution n =1000, p = 0.1
n <- 1000; p <- 0.1
v <- rbinom(5, n, p); v   #vector of 5 samples, all not far from 100
#Use chi square to measure the discrepancy '
ChiSq(v,Expect)    #just a number -- we must figure out how to interpret it
#If we do this many times, we can spot when a discrepancy is large
N <- 10000; cs <- numeric(N)   #create an empty vector to hold values
for (i in 1:N){
  v <- rbinom(5, n, p);
  cs[i] <- ChiSq(v,Expect)
}
csh <- hist(cs,breaks = "FD", col=rgb(0,0,1,1/4),probability = TRUE)
#The color is transparent: it is only 1/4 blue
#Let's do the same, using  n =10000, p = 0.01; expectation is still 100
n <- 10000; p <- 0.01
N <- 10000; cs2 <- numeric(N)
for (i in 1:N){
  v <- rbinom(5, n, p);
  cs2[i] <- ChiSq(v,Expect)
}
csh2 <- hist(cs2,breaks = "FD", col=rgb(1,0,0,1/4), probability = TRUE, add = TRUE)
#The second histogram was transparent red

#since the limit of binomial is Poisson,
#there is a third way to get an expectation of 100
N <- 10000; cs3 <- numeric(N)
for (i in 1:N){
  v <- rpois(5, n*p)    #parameter lambda = 100
  cs3[i] <- ChiSq(v,Expect)
}
csh3 <- hist(cs3,breaks = "FD", col=rgb(0,1,0,1/4), probability = TRUE, add = TRUE) #again, familiar

#The continuous chi-square distribution closely matches these three histograms
curve(dchisq(x, df=5), col = "black", lwd = 2, add= TRUE)    

#Now we can assess the discrepancy of a specific observation
#Fabricate one by sampling from a Poisson distribution.
vSave <- rpois(5, n*p); ChiSave <- ChiSq(vSave, Expect)
mean(cs3 > ChiSave)   #probability of getting a larger chi square value by chance
pchisq(ChiSave, df=5, lower.tail = FALSE)  #gives roughly the same pValue
#If you use the observations to estimate parameters for the expected values,
#you must decrease the degrees of freedom, df, by 1 for each parameter.

#For example, in the real world, the mean of the population is usually not known.
#It must be estimated from the observations. We can simulate this
v <- rpois(5, 100);   #samples from a population with expected value 100
rep(mean(v),5);       #but we can only use the sample mean for our expected values
#Try this 10000 times
N <- 10000; cs4 <- numeric(N)
for (i in 1:N){
  v <- rpois(5, n*p);
  Expect <- rep(mean(v),5)    
  cs4[i] <- ChiSq(v,Expect)
}
hist(cs4, breaks = "FD", probability = TRUE)
curve(dchisq(x, df=5), col = "red", lwd = 2, add= TRUE)   #poor match
curve(dchisq(x, df=4), col = "blue", lwd = 2, add= TRUE) #one less df gives good match
#For our saved observation
Expect <- rep(mean(vSave),5)
ChiSave <- ChiSq(vSave,Expect)
abline(v=ChiSave, , lwd = 2, col = "blue")
#So we have three ways to estimate the probability
#that a deviation from uniform observations
#at least as large as in our sample could arise by chance
mean(cs4 > ChiSave)      #compare with a histogram
pchisq(ChiSave, df=4, lower.tail = FALSE)  #distribution function for P(X > ChiSave)
chisq.test(vSave)    #use the built-in chi square teat

#Topic 2: Some real-world examples of chi-square from Laura Chihara and Tim Hesterberg,
#Mathematical Statistics with R and Resampling

#Here is a vector listing the birth quarters for a large number of athletes
#Are an equal number of athletes born in each quarter?
#Create a vector containing the data from Example 3.8 in the book:
Births<-c(rep("Aug-Oct",150),rep("Nov-Jan",138),rep("Feb-April",140),rep("May-July",100))
Obs<-table(Births);Obs   #tally of the number of observations for each quarter
Expected <- rep(sum(Obs)/length(Obs),length(Obs)); Expected    #assuming all quarters equally likely
#First approach: the built-in test calculates the expected values
Pvalue <-chisq.test(Obs); Pvalue #P-value of .012 suggests that distribution is not uniform
#Note that df was reduced to 3 because we used the data to estimate the expected counts.
#Second approach: use our own chi-square function
CSq <- ChiSq(Obs, Expected); CSq   #calculate chi square
pchisq(CSq, df = 3, lower.tail = FALSE)   #our function gives same P value as the built-in test
#Third approach: assume uniformity and see how often we exceed the observed Chi square value

#Make a vector with just four elements
months=c("Aug-Oct","Nov-Jan","Feb-April","May-July"); months
#We can sample from this vector to get the right total number of observations

#Now do a lot of simulations and treat each sample just as we did the real data:
N =10^4 ; result<-numeric(N)
for (i in 1:N){
  Births.sim<-sample(months,sum(Obs), replace= TRUE)
  Obs.sim<-table(Births.sim);
  Expected <- rep(sum(Obs.sim)/length(Obs.sim),length(Obs.sim)) #uses the data
  result[i]<-ChiSq(Obs.sim, Expected)
}
hist(result, breaks = "FD", probability = TRUE)
abline(v = CSq, col = "blue") 
mean(result >= CSq)     #same as the pvalue from the built-in test
curve(dchisq(x, df=3), col = "red", add= TRUE)  #look at area to right of blue line
pchisq(CSq, df = 3, lower.tail = FALSE)   #or use the distribution function

#Another example: Do home runs per game have a Poisson distribution?
Phils= read.csv("Phillies2009.csv");head(Phils)
lambda <- mean(Phils$HomeRuns); lambda     
#Poisson parameter is equal to the expectation -- use the sample mean (costs 1 df)

HomeTable<-table(Phils$HomeRuns); HomeTable
#Should not do chi square with tiny counts.  Combine all games with 4 or more homers
#Make the vector have only 5 elements
#The resulting data are in Table 3.10 of the book.
HomeTable[5]<-sum(HomeTable[5:6]);HomeTable <- HomeTable[-6]; HomeTable #(top row)

#Generate the expected counts for a 162-game season,
#assuming the data are Poisson with the calculated lambda = 1.3827
Expected<-162*dpois(0:3, lambda);Expected[5]<-162*(1-ppois(3, lambda))
Expected #(bottom row)
ChiPhil<-ChiSq(HomeTable,Expected);ChiPhil 
#How probable is this large a value, given the chi-square distribution?
#Because we estimated one parameter from the data and used the total home runs, 
#there are only three degrees of freedom
#First approach: use our ChiSq function and the built-in pchisq function
Pvalue<- pchisq(ChiPhil,3,lower.tail = FALSE); Pvalue  
#Conclusion -- the observed data are consistent with a Poisson distribution

#Second approach: simulate many seasons and see how often we get a larger chi square
#We simulate 10000 seasons of 162 games each
N = 10^4-1; result <- numeric(N)
for (i in 1:N){
  expData = rpois(162,lambda) #generate 162 random samples
  Counts<-numeric(5)
  Counts[1] <- sum(expData ==0) 
  Counts[2] <- sum(expData ==1)
  Counts[3] <- sum(expData ==2)
  Counts[4] <- sum(expData ==3)
  Counts[5] <- sum(expData >=4)   #4 or more home runs
  result[i] = ChiSq(Counts, Expected)
}
hist(result, breaks = "FD", probability =TRUE)
curve(dchisq(x, df=3), col = "blue", add= TRUE)    #not a great fit
#The problem is that the expected number of home runs per game is so small
abline(v = ChiPhil, col = "red")
mean(result >= ChiPhil)   #value of 0.93 is different from built-in chi square test
#In this case the result of the simulation is unassailable.
#The built-in chi square test is suspect because the number of home runs is small



