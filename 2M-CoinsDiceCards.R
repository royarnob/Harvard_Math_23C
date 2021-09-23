#Math 23c Script 2M-CoinsDiceCards.R

#Last modified: January 23, 2018
#We calculate probabilities by using rows of a data frame as a finite sample space

#Topic 1: Coins -- Flip a fair coin three times
#we can create a data frame as we did for logical variables
X <- c("H","T")
coin3 <- expand.grid(flip1 = X, flip2 = X, flip3 = X); coin3

#There are eight elements in the sample space, each with probability 1/8.
#The number of heads is a random variable on this sample space.
#Add it to the data frame.
coin3$nhead<-(coin3$flip1 == "H") + (coin3$flip2 == "H") + (coin3$flip3 == "H");coin3

#Count the number of rows for each value of this random variable
table(coin3$nhead)

#Convert to a vector of probabilities
probs <- table(coin3$nhead)/length(coin3$nhead)
barplot(probs)

#To use the definition of expectation, sum over the set of values
sum(probs*0:3)

#Alternative: sum over the sample space
mean(coin3$nhead)

#If the coin is unfair and comes up heads with probability 3/5, simulate one flip by
#using three copies of "H" and two of "T".
X <- c(rep("H",3),rep("T",2)); X  #each of five alternatives equally likely

#Everything else works just the same
unfair <- expand.grid(flip1 = X, flip2 = X, flip3 = X); unfair

#There are 125 elements in the sample space, each with probability 1/125
#The number of heads is a random variable on this sample space
#Add it to the data frame.
unfair$nhead = (unfair$flip1 == "H") + (unfair$flip2 == "H") + (unfair$flip3 == "H"); unfair

#Count the number of rows for each value of this random variable
table(unfair$nhead)

#Convert to a vector of probabilities
probs <- table(unfair$nhead)/length(unfair$nhead)
barplot(probs)

#To use the definition of expectation, sum over the set of values
sum(probs*0:3)

#Alternative: sum over the sample space
mean(unfair$nhead)

#This approach only works if the probability of a head is a rational number.


#Topic 2: Dice -- Roll two standard fair dice, one red, one green
#Make a vector that lists all the outcomes of rolling one die.
X <- 1:6; X
mu <- mean(X); mu   #expectation for one die
#For a pair of dice, one red, one green, we need two columns
Red <- rep(X, each = 6); Red   #repeat each value 6 times
Green <- rep(X, 6); Green      #repeat the vector 6 times
Total <- Red + Green           #sum of random variables
S<- data.frame(Red, Green, Total); S  #all 36 outcomes

CrapsWin <- which(Total == 7 | Total == 11)#event "win at Craps,"a subset of the rows
length(CrapsWin)/nrow(S)  #probability of winning

#We set up Red and Green as independent random variables
mean((S$Red==5)&(S$Green==3))
mean(S$Red==5)*mean(S$Green==3)  #same value - a consequence of independence
#For independent variables, expectation of product = product of expectations
mean(S$Red*S$Green); mean(S$Red)*mean(S$Green)
mean(S$Red-mu); mean(S$Green-mu)  #both are zero, and so is the product
mean((S$Red - mu)*(S$Green-mu)) #agreement confirms the die rolls are uncorrelated
cor (S$Red, S$Green)    #built-in function to compute this correlation

#Since each row has the same probability, we can sample from this population
S[4,] #extract one row - note that row index comes first


#Repeat the following block several times to see different samples
selection <- sample(1:36, 10, replace = TRUE); #with replacement, may have duplicates
selection    #A sequence of row indices
sample10 <-S[selection,]  ; head(sample10)  #another data frame
sample10$Red     #extract the first column
mean(sample10$Red)  #a sample mean, probably not 3.5
var(sample10$Red, sample10$Green)   #covariance close to zero suggests independence
cor(sample10$Red, sample10$Green)   #correlation lies in range [-1,1]


#Try again with a much larger sample

selection <- sample(1:36, 3600, replace = TRUE); head(selection) #may have duplicates
sample3600 <-S[selection,]  ; head(sample3600)  #a data frame
head(sample3600$Red)     #extract the first column
mean(sample3600$Red)  #a sample mean, probably very close to 3.5
var(sample3600$Red, sample3600$Green)   #values close to zero suggest independence
cor(sample3600$Red, sample3600$Green)   #correlation lies in range [-1,1]

#It is not hard to add a third die
Red3 <- rep(S$Red, each = 6); Red3
Green3 <- rep(S$Green, each = 6); Green3
White3 <- rep(S$Green, 6); White3
Omega3 <- data.frame(Red3, Green3, White3); Omega3  #216 rows, equally likely
mu3 <- mean(Red3 + Green3 + White3); mu3     #expectation of sum for 3 dice
sigmaSq3 <- mean((Red3 + Green3 + White3 -mu3)^2); sigmaSq3 #variance for 3 dice
probs <-table(Red3 + Green3 + White3)/216; probs  #tally the sums
barplot(probs)      #this does not view the sums as numbers

#Making a nice-looking histogram is slightly tricky - break on half integers
hist(Red3+ Green3+ White3, breaks = seq(from = 2.5,to = 18.5))
#The number of sixes is a random variable
nSix <- (Red3 ==6) + (Green3 == 6) + (White3 ==6) #sum of three indicator functions
tbl <- table(nSix); tbl    #frequencies for the carnival game Chuck-A-Luck
sum(tbl)
names(tbl)      #these are now character strings but can be coerced
as.numeric(names(tbl))
sum(as.numeric(names(tbl))*tbl)   #expected number of sixes is 216/2

#This different sample space leads to a random variable with the same distribution
#You have been given two fair dice. 
#The faces of the red die show 1,2,2,3,3,4
#The faces of the green die show 1,3,4,5,6,8
#Question: can these substitute for a pair of standard dice?

FunnyDice <- data.frame(expand.grid(c(1,2,2,3,3,4), c(1,3,4,5,6,8)))
colnames(FunnyDice) <- c("Red","Green"); FunnyDice
#This is a different sample space -- the event Red=2, Green=2 cannot occur

#Create a vector with the total roll (add random variables)
total<-FunnyDice$Red+FunnyDice$Green; total

#Make a table of the frequency of each total
tbl <-table(total); tbl
barplot(tbl)     #same as for a standard pair of dice!


#Topic 3: Cards -- Draw one card from a well-shuffled deck
Ranks <- c("Ace", "Two", "Three","Four", "Five", "Six", 
           "Seven", "Eight", "Nine","Ten", "Jack", "Queen", "King")
Suits <- c("Clubs","Diamonds","Hearts","Spades")
#this syntax makes a data frame with named columns
Deck <- expand.grid(rank=Ranks, suit=Suits); Deck 


#What is the probability of getting at least one ace when you choose a pair?

#It took me an hour to figure out how to do the next few lines in R!
AllPairs<-combn(1:52,2) #generate a list of all possible subsets of 2 of the 52 cards
N <-ncol(AllPairs); N; #the result is a list of N columns, each a 2-component vector
AllPairs[,1]   #extract the first column
r <-Deck$rank[AllPairs[,1]];r    #vector with the rank of the two cards
HasAce <- (sum(r == c("Ace", "Ace")) >0); HasAce  #one Ace in r makes the sum positive
count <- 0;   #counts the number of pairs that include an ace
for (i in 1:N) {
  r <-Deck$rank[AllPairs[,i]]    #vector with the rank of the two cards
  HasAce <- (sum(r == c("Ace", "Ace")) >0); HasAce
  count <- count+HasAce;   #counts the number of pairs that include an ace
}
count   #number of pairs that include at least one ace
count/N ;2/13  #probability of getting at least one ace is not 2/13.
#Here is the more orthodox approach: there are 48 cards that are not aces
NoAces <- 48*47/2     #number of pairs that do not include an ace
N - NoAces            #number of pairs that include at least one ace

#Topic 4: Using the pigeonhole principle to find rational approximations
#Let's find a good approximation to e using the same strategy as for pi in the lecture notes.
e <- exp(1); e

#Make a vector of the first 101 multiples of e
mults <- (1:101)*e; head(mults)    #It's convenient to start with 1 instead of 0

#Multiply by 100 to move two digits to the left of the decimal point.
mults100 <- 100*mults; head(mults100)

#Keep just the integer part
floors <- floor(mults100); head(floors)

#keep the remainder when divided by 100
rems <- floors %% 100; head(rems)

#Now we have extracted the two digits after the decimal point
#With 101 components in the vector, the pigeonhole principle guarantees a duplicate.
table(rems)

#Choose any remainder that is duplicated, say 2
which(rems==2)    #so 7e and 78e have the same two digits after the decimal point
(78-7)*e;  #guaranteed to be very close to an integer: round it to 193
eapprox <- 193/71; eapprox; e     #good to almost 4 decimal places
#You can try with other duplicates but may not find anything new
which(rems==5)    #again, a difference of 71
which(rems==67)    #again, a difference of 71
which(rems==95)    #yet again, a difference of 71
