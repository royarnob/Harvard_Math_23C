#Math 23 Script 1.1B-PointsVectors.R
#Module 1, Week 1, subsection 1.2

#Last modified: August 26, 2014 by Paul Bamberg

pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,5), ylim = c(-1,5), xlab = "", ylab= "", axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom

#Topic 1 - addition of vectors in R^2
#An element of R^2 is just a list of two numbers.
A <- c(1,2)     #c() means "concatenate"

#We can think of this element of R^2 as a point and plot it.
points(A[1], A[2])  
B <- c(4,4)  #another point
points(B[1], B[2])     #add it to the plot

#The difference of two points is a vector.
v <- B - A; v  #difference of two points is a vector
arrows(A[1],A[2],B[1],B[2]) #this function takes pairs of coordinates

#We can also show the same vector with its "tail" at the origin
arrows(0,0,v[1],v[2])

w <- c(1,-1)   #a second vector
arrows(0,0,w[1],w[2])   #plot it from the origin
u <- v + w  #add the two vectors
arrows(0,0,u[1],u[2])    #plot the sum with tail at the origin

#If we put the vectors "head to tail" we get a parallelogram
arrows(v[1],v[2], u[1],u[2])   #the sum completes the parallelogram
arrows(w[1],w[2], u[1],u[2])   #the sum completes the parallelogram

#Topic 2 - A diagram to illustrate the point-vector relationship

plot(NULL, xlim = c(0,5), ylim = c(-1,5), xlab = "", ylab= "", axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom

C = B + w   #add vector w to point B to get point C
#Plot points A, B, and C
#We need to supply a vector of first components and a vector of second components
points(c(A[1],B[1],C[1]), c(A[2],B[2],C[2])) 
 
#Label the points on the diagram
text(A[1],A[2]+0.2,"A"); text(B[1],B[2]+0.2,"B"); text(C[1],C[2]+0.2,"C")

#We can also draw lots of arrows with a single command
arrows(c(A[1],B[1],A[1]), c(A[2],B[2],A[2]), c(B[1],C[1],C[1]), c(B[2],C[2],C[2]))

#Label the vectors on the diagram
text((A[1]+B[1])/2,(A[2]+B[2])/2+0.2,"v")
text((B[1]+C[1])/2,(B[2]+C[2])/2+0.2,"w")
text((A[1]+C[1])/2,(A[2]+C[2])/2+0.2,"v+w")


#Here are the axioms about the point-vector relationship

#1: An ordered pair of points determines a vector.
#2: Given point A and vector v, there is a unique point B
#   such that A and B determine v: B = A + v
#3: If A and B determine v while B and C determine w,
#   then A and C determine the vector sum v + w.

#Topic 3 - Subtraction and scalar multiplication

plot(NULL, xlim = c(-4,4), ylim = c(-4,4), xlab = "", ylab= "", axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

#Make a vector and plot it
points(0,0)
v <- c(1, 2); arrows(0,0, v[1], v[2])
#Negate the vector to get its additive inverse
vInv <- -v; arrows(0,0, vInv[1], vInv[2])

#Multiplication of vectors in R is done component by component.
#Since v has two components we could multiply by a two-component vector.
v2 <- c(2,2)* v; arrows(0,0, v2[1], v2[2], col = "red")

#Fortunately, if one vector is too short, R just replicates it to make the length right.
k <- 2  #this vector with one component will be lengthened to become (2,2) 
vk <- k*v; arrows(0,0, vk[1], vk[2], col = "blue")

