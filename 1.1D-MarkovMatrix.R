#Math 23 Script 1.1D-MarkovMatrix.R
#Module 1, Week 1, subsection 1.11


#Topic 1 - A game of volleyball
#The first component of vector v is the probability that team 1 is serving.
#The second component of vector v is the probability that team 2 is serving.
#So both components are between 0 and 1,and their sum is 1.
v0 <- c(1,0)  #this is legal; it means team 1 is serving for sure.
pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,1), ylim = c(0,1), xlab = "team 1", ylab= "team 2",
     asp =1,axes = FALSE) #asp=1 gives a 1:1 aspect ratio
axis(1,pos = 0); axis(2,pos = 0) #axes blow and to the left
abline(1,-1, col = "red")
#All legal vectors must go from the origin to the red line segment.
#Function f updates the probability vector for a single point.
#When team 1 serves, it retains the serve with probability 0.8.
#So f (1,0) = (0.8, 0.2)
#When team 2 serves, it retains the serve with probability 0.7.
#So f (0,1) = (0.3, 0.7)
#Here is the matrix for function f.
F <- matrix(c(0.8, 0.2, 0.3, 0.7),2); F
#Here is the starting situation.
arrows(0,0, v0[1], v0[2], col = "blue")
#Now play the first point.
v1 <- F %*% v0; v1
arrows(0,0, v1[1], v1[2], col = "blue")
#Next play the second point.
v2 <- F %*% v1; v2
arrows(0,0, v2[1], v2[2], col = "blue")

#Here is a matrix that updates for two points in a single computation.
F2 <- F %*% F; F2
F2 %*% v0; v2  #it agrees with our previous calculation

#Do two more points.
v4 <- F2 %*% v2
arrows(0,0, v4[1], v4[2], col = "blue")

#Here is a matrix that updates for four points in a single computation.
F4 <- F2 %*% F2; F4
F4 %*% v0; v4  #it agrees with our previous calculation

#Do four more points.
v8 <- F4 %*% v4; v8
arrows(0,0, v8[1], v8[2], col = "blue")

#It looks at though the vectors are converging to a limit.
vLimit <- c(0.6, 0.4)
F %*% vLimit

#Conclusion, late in the game, the probability that team 1 is serving is very close to 0.6.
#In week 4 we will analyze this problem using eigenvectors.

#Topic 2 - traveling around on ferryboats
#Here is a graph that shows the islands and ferry routes:
plot(NULL, xlim = c(-3,3), ylim = c(-3,3), xlab = "", ylab= "",axes = FALSE) 
I1 <- c(-2,2); I2 <- c(-2,-2); I3 <- c(2,2); I4 <- c(2,-2)
points(c(I1[1],I2[1],I3[1],I4[1]),c(I1[2],I2[2],I3[2],I4[2]))
text(I1[1]-0.2,I1[2]+0.2,"1"); text(I2[1]-0.2,I2[2]-0.2,"2");
text(I3[1]+0.2,I3[2]+0.2,"3"); text(I4[1]+0.2,I4[2]-0.2,"4");
#From island 1 there are boats to islands 2 and 3, but not to 4
arrows(I1[1],I1[2],I2[1],I2[2]); arrows(I1[1],I1[2],I3[1],I3[2])
c1 <- c(0,1,1,0)
#From island 2 there are boats to island 4, but not to 1 or 3.
arrows(I2[1],I2[2],I4[1],I4[2]);
c2 <- c(0,0,0,1)
#From island 3 there are boats to islands 1 and 4, but not to 2
arrows(I3[1],I3[2],I1[1],I1[2]); arrows(I3[1],I3[2],I4[1],I4[2])
c3 <- c(1,0,0,1)
#From island 4 there are boats to island 1, but not to 2 or 3.
arrows(I4[1],I4[2],I1[1],I1[2]);
c4 <- c(1,0,0,0)

#Here is the "transition matrix" for this graph.
A <- cbind(c1,c2,c3,c4); A
#If we square the matrix, the entry A^2[i,j] gives the number of ways
#to go from island j(column) to island i(row) in two steps.
B <- A %*% A; B  #do not use A^2 in R
#Square again to get the number of four-step paths
C <- B %*% B; C
#The three 4-step paths from island 3 to island 1 are
#3-4-1-3-1, 3-1-2-4-1, and 3-1-3-4-1

