#Math 23c Script 1D-LogicalData.R
#Last modified: January 19, 2018

#Topic 1 - Building and analyzing a data frame with nothing but logical columns
#Load the General Social Survey data set for 2002 (other years available online)
GSS<-read.csv("GSS2002.csv"); head(GSS)
#Identify the rows with non-empty entries for Gender, Marital, Religion, PolParty, OwnGun, and Pres00
index<-which(!is.na(GSS$Gender) & !is.na(GSS$Marital)& !is.na(GSS$Religion)&
            !is.na(GSS$PolParty)& !is.na(GSS$OwnGun)& !(GSS$OwnGun=="Refused") & 
            !is.na(GSS$Pres00) & ((GSS$Pres00 == "Bush")|(GSS$Pres00 == "Gore")))
#which() returns a vector of the rows that satisfy the given criterion
length(index)    #how many rows did we select?
GSSCleaned <- GSS[index,]
#Extract the six desired columns as vectors, keeping just the selected rows
Gender2<-GSS$Gender[index] 
Marital2<-GSS$Marital[index] 
Religion2<-GSS$Religion[index] 
PolParty2<-GSS$PolParty[index] 
OwnGun2<-GSS$OwnGun[index] 
Pres002<-GSS$Pres00[index] 
#Now we can use the vectorized == operator to create six logical columns
Male <- Gender2=="Male"
head(Gender2); head(Male)  #check that it is working
Married <- Marital2=="Married"
Protestant <- Religion2=="Protestant"
#Extracting party is tricky
PolParty2    #there are three flavors of Republican
Republican <- (PolParty2=="Strong Rep")|(PolParty2=="Ind, Near Rep")|
              (PolParty2=="Not Str Rep")
head(PolParty2); head(Republican)
GunOwner <- OwnGun2=="Yes"
Bush <- Pres002=="Bush"
#Combine the six logical vectors into a data frame
GSSLogical <- data.frame(Male, Married, Protestant, Republican, GunOwner, Bush)
head(GSSLogical)
#Save in a file for possible use on the homework
write.csv(GSSLogical,"GSSLogical.csv")  #look at this in Excel or as a text file
#Now we can use logical operations to search for rows
Guns <- which(GSSLogical$GunOwner); head(Guns); length(Guns)  #all gun owners
#Use AND to find gun owners who voted for Bush
GunsBush <- which(GSSLogical$GunOwner&GSSLogical$Bush);head(GunsBush);length(GunsBush)
#Use AND along with NOT to find gun owners who voted for Gore
GunsGore <- which(GSSLogical$GunOwner&!GSSLogical$Bush);head(GunsGore);length(GunsGore)
#All four alternatives are shown in a contingency table
tbl <- table(GSSLogical$GunOwner,GSSLogical$Bush); tbl
#The first argument corresponds to the row labels.
#We could have done this with the "factor" columns in the cleaned data frame.
table(GSSCleaned$OwnGun,GSSCleaned$Pres00)
#In this case, after we know the one nonzero value
#the row sums and column sums determine the last three entries.
#Compare with the table that would be expected if the factors were independent
tbl
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
#These table look quite different. Is the difference significant?
#R has a mysterious built-in test for doing this.
chisq.test(GSSLogical$GunOwner,GSSLogical$Bush)
#The low p-value means there is about 1 chance in 10,000,000 that it arose by chance.
#Another example: did Republicans vote for Bush?
tbl <- table(GSSLogical$Republican,GSSLogical$Bush); tbl
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
chisq.test(GSSLogical$Republican,GSSLogical$Bush)   #even more extreme

#Perhaps religion is independent of gender
tbl <- table(GSSLogical$Protestant,GSSLogical$Male); tbl
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
chisq.test(GSSLogical$Protestant,GSSLogical$Male)   #14% chance to arise by chance

#Topic 2 - Building and analyzing a data frame with logical and numeric columns
#Load the Red Sox 2013 data set
Sox<-read.csv("RedSox2013.csv"); head(Sox)
#We can extract statistics for the numerical columns
mean(Sox$Attendance)
median(Sox$Duration)
mean(Sox$Duration)    #greater than the median because of extra innings
max(Sox$Duration)     #duration in minutes of the longest game
min(Sox$Duration)     #duration in minutes of the shortest game

#It is useful to exploit the fact that TRUE is converted to 1 if used for arithmetic.
mean(Sox$WonLost == "W")     #proportion of games won
#Calculate the proportion of night games won
sum((Sox$WonLost == "W")*(Sox$DayNight == "N"))/sum(Sox$DayNight == "N")  

#We can also display the numerical vectors in various ways.
#Make a table of values in the vector (works even if not numeric).
#This works nicely if there are a small number of distinct values.
table(Sox$DayNight)
barplot(table(Sox$DayNight))
table(Sox$R)
barplot(table(Sox$R))
#An alternative is a histogram
hist(Sox$R)
#You can provide a vector of breakpoints
hist(Sox$R, breaks=(0:25))

#Barplots work less well for a large number of distinct values
table(Sox$Duration)
barplot(table(Sox$Duration))
#Notice that a value with zero occurrences is not displayed at all
#A histogram solves that problem
hist(Sox$Duration) #default bin size may be too large for your taste
hist(Sox$Duration,breaks= "FD")   #I prefer this to the default
hist(Sox$Duration,breaks=seq(from=90, to = 360, by =5)) #forces labels at 100 and 350
hist(Sox$Attendance,breaks= "FD")   

#Use the vectorized ==, >, >=, < operators to make logical columns from numeric ones.
Won <- Sox$WonLost=="W"; head(Sox$WonLost);head(Won)
ScoreMore4 <- Sox$R > 4; head(Sox$R);head(ScoreMore4)
Gave3OrFewer <- Sox$RA <= 3; head(Sox$RA);head(Gave3OrFewer)
DayGame <- Sox$DayNight=="D"
BigCrowd <- Sox$Attendance >= 40000
QuickGame <- Sox$Duration <= 180
HomeGame <- !Sox$Away
SoxLogical <- data.frame(Won,ScoreMore4,Gave3OrFewer,DayGame,
                       BigCrowd,QuickGame,HomeGame);head(SoxLogical)
write.csv(SoxLogical,"SoxLogical")  #save for possible use on homework

#Search for away games lost in spite of scoring >4 runs
which(SoxLogical$ScoreMore4 & !SoxLogical$HomeGame & !SoxLogical$Won)
#Look at one of these rows as a check
SoxLogical[26,1]      #blank after the comma selects one column
SoxLogical[26,]       #blank after the comma selects all columns
SoxLogical[26,]$Won   #alternative way to select a column
#By exploiting the fact that TRUE = 1, FALSE = 0 we can learn a lot
sum(SoxLogical$HomeGame)   #number of home games
mean(SoxLogical$Won)   #proportion of games won
#Look for away games with big crowds
sum(!SoxLogical$HomeGame&SoxLogical$BigCrowd)    #using logical operators
sum((!SoxLogical$HomeGame)*SoxLogical$BigCrowd)  #using arithmetic
#Find the proportion of games won when scoring > 4 runs
sum(SoxLogical$Won*SoxLogical$ScoreMore4)/sum(SoxLogical$ScoreMore4)




