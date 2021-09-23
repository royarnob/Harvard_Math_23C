#Math 23c Script 1M-TruthTables.R 
#Last modified: January 19, 2018

#Topic 1: Building truth tables as data frames

#Building a truth table for "NOT"
#Make a vector of the two possible values for Boolean variable p
p <- c(TRUE, FALSE); p   #useful style: save and display
#The ! operator will work on a single logical value
!TRUE; !FALSE
#Apply the NOT operator, which is "vectorized," to the entire vector
NOTp <- !p; NOTp
#Now we can assemble the two vectors into a data frame
NotTable <- data.frame(p,NOTp); NotTable

#Now do the same thing for "AND" and "OR", with four rows:
p <- c(TRUE, TRUE, FALSE, FALSE)
q <- c(T,F,T,F); q
#The second line works because T is a predefined variable with value TRUE
T; F    #careful -- don't assign a new value to T or F!
#It is important to use the "vectorized" operators & and |, not && and ||
pANDq <- p&q; pANDq
p&&q #&& does not produce a vector. It looks just at the first component.
pORq <- p|q; pORq
p||q #|| does not produce a vector
#Now we can assemble the four vectors into a data frame
AndOrTable <- data.frame(p,q,pANDq, pORq); AndOrTable
#Check the DeMorgan's law NOT(p AND q) = (NOT p) OR (NOT q)
!(pANDq); (!p) | (!q)

#With three variables it gets tedious to type all the vectors.
p <- c(rep(TRUE,4), rep(FALSE,4));p
q <- rep(c(T,T,F,F),2);q
r <- rep(c(T,F),4);r

#There is a cute way to do this all in one step. Example with four variables:
grid4 <- expand.grid(c(T,F),c(T,F),c(T,F),c(T,F)); grid4
#We can reorder and rename the columns
p <- grid4$Var4;p
q <- grid4$Var3;q
r <- grid4$Var2;r
s <- grid4$Var1;s
Truth4 <-data.frame(p,q,r,s); Truth4
#Save this for possible use on the homework
write.csv(Truth4,"Truth4.csv")  #if this fails.close and restart RStudio
Reload <- read.csv("Truth4.csv"); Reload  #check that we can load it


#Topic 2 - Working with disjunctive and conjunctive normal form
#Add a fourth column to the three-variable truth table.
p <- c(rep(TRUE,4), rep(FALSE,4));p
q <- rep(c(T,T,F,F),2);q
r <- rep(c(T,F),4);r
z <- c(T,F,F,T,F,F,T,F)
data.frame(p,q,r,z)
#Look at the true rows to set up disjunctive normal form
DNF <- (p&q&r)|(p&!q&!r)|(!p&!q&r); DNF
#Look at the false rows to set up conjunctive normal form
data.frame(p,q,r,z)
CNF <- (!p|!q|r)&(!p|q|!r)&(p|!q|!r)&(p|!q|r)&(p|q|r); CNF

#Topic 3 - Doing logic with finite fields
#Conveniently, TRUE= 1 and FALSE = 0
#Rebuild the truth table for AND using multiplication
p <- c(TRUE, TRUE, FALSE, FALSE)
q <- c(T,F,T,F)
p*q

#For OR we need to define an addition operator for Z2
"%+%" <- function(x,y) (x+y)%%2
0%+%0; 0%+%1; 1%+%0; 1%+%1
#Rebuild the truth table for OR using multiplication
p <- c(TRUE, TRUE, FALSE, FALSE)
q <- c(T,F,T,F)
p %+% q %+% (p*q)   #need the parentheses

#Show that the formula for p implies q is 1 + p + pq
IMPLIES <- 1 %+% p %+% (p*q)    
data.frame(p,q,IMPLIES)    #the truth table for p ===> q
