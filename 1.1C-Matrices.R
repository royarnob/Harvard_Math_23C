#Math 23 Script 1.1C-Matrices.R
#Module 1, Week 1, subsections 1.3-1.10

#Topic 1 - Matrices and Matrix Operations in R
#Here are four different ways to create the same 2x2 matrix in R.
c1 <- c(2,4); c2 <- c(1,5)   #the two columns
cbind(c1,c2)   #bind the columns to make a matrix

r1 <- c(2,1); r2 <- c(4,5)
rbind(r1,r2)   #bind the rows to make the same matrix

matrix(c(2,1,4,5),2,2,byrow = TRUE) #fill row by row
matrix(c(2,4,1,5),2,2,byrow = FALSE) #fill column by column
#The second dimension can be calculated, and byrow defaults to FALSE
A <- matrix(c(2,4,1,5),2); A  #now the matrix has a name


#Matrix multiplication is represented by the %*% operator.
v <- c(1,3); v
w <- A %*% v; A; as.matrix(v); w        #matrix A times vector v
is.vector(w); is.matrix(w)
#The result is a matrix with one column, not a vector.
#For our purposes that is good -- it displays vertically.
w + c(5,5)   #vector addition works fine
as.vector(w)   #how to convert back to a vector

#Make a second matrix
B <- cbind(c(3,-1), c(-2, 4)); B
  
A %*% B; B %*% A   #multiplication is not commutative

#To test associativity, we need a third matrix
C <- cbind(c(2,2), c(0,-3)); C

#Associativity means we can parenthesize either way or not at all.
(A %*% B) %*% C; A %*% (B %*% C); A %*% B %*% C

#Matrices with the same shape can be added with the + operator.
A; B; A+B   #R just adds them like vectors

#Multiplication is distributive with respect to addition.
(A+B)%*%C; A%*%C + B%*%C

#Since multiplication is not commutative, we need a second distributive law.
A %*% (B+C); A%*%B + A%*%C

#How to extract pieces of a matrix
A; A[,1]  #extract the first column as a vector
A; A[2,]  #extract the second row as a vector
A; A[2,1] #extract the first entry in the second row

A; A[4]  #extract the fourth entry (bad idea)

#More matrix operations
t(A)     #take the transpose
det(A)   #evaluate the determinant
M <- solve(A); M  #calculate the inverse
M %*% A; A %*% M  #inverse works in either order
round(M %*% A,digits = 6)   #get rid of the tiny entries that should be zero

#Inverting a 2x2 matrix by inspection
T <- matrix(c(3,1,4,2),2); T  
#The rule: swap the entries on the main diagonal; negate the off-diagonal
#Then divide by the determinant
TInv <- matrix(c(2,-1,-4,3),2)/det(T); TInv
round(TInv %*% T, digits = 6) #the inverse was correct

#Something that does not do what you probably wanted
A; A^2     #squares each entry in the matrix
A %*% A    #this is "A squared"


#Topic 2 - Solving equations using matrices
#4x + y  = 11
#2x - y  = 1
#Make the left hand side into a matrix.
S <- rbind(c(4,1),c(2,-1)); S
#Make the right hand side into a column vector.
w <- as.matrix(c(11,1))
v <- solve(S,w); v   #Solve the equation Sv = w for vector v
#Check that the answer is correct
S %*% v

#Topic 3 - Linear functions and matrices
#Define a linear function.
f <- function(v) v[1]*c(1,2) + v[2]*c(-3, 1)

#Try this out on a linear combination of vectors.
v1 <- c(2,3); v2<- c(1,-1); a1 <- 4; a2 <- 2
f(a1*v1 + a2*v2)
a1*f(v1) + a2*f(v2)   #f is linear so we get the same answer

#How to construct the matrix that represents linear function f
F <- cbind(f(c(1,0)),f(c(0,1))); F

#Now we can use matrix multiplication to evaluate the function f
x <- c(2,3)
f(x)
F %*% x    #the same vector; though it displays as a column

#Matrix multiplication repesents composition of linear functions
#Invent a second linear function g
g <- function(v) v[1]*c(-2,-1) + v[2]*c(3, 2)

#Construct the matrix that represents linear function g
G <- cbind(g(c(1,0)),g(c(0,1))); G

#Now we can create the composition of the two functions:
fg <- function(v) f(g(v))

#Construct the matrix that represents the composition fg
FG <- cbind(fg(c(1,0)),fg(c(0,1))); FG

#Matrix multiplication reproduces ths result:
F%*%G

#Topic 4 - Matrices that are not square
M32 <- matrix(c(1,2,4,3,2,5),3); M32 #a 3x2 matrix
M23 <- matrix(c(3,-1,4,-2,3,1),2); M23 #a 2x3 matrix
 
#These matrices have different shapes and cannot be added
M23+M32
#For multiplication to be valid
#the number of columns in the right-hand factor
#must equal the number of rows in the left-hand factor.
M23 %*% M32   #product is a 2x2 matrix
M32 %*% M23   #product is a 3x3 matrix
M32 %*% M32   #multiplication is not valid

#Topic 5 - Properties of the determinant
#The determinant in R is a function of a square matrix.
#Make the matrix by binding two columns, which are vectors.
v1 <- c(3,1); v2 <- c(4,2); M <-cbind(v1,v2); M
det(M)    #3*2 - 4*1
#It is useful to think of the determinant as a function of the columns.
det2 <- function(c1,c2) det(cbind(c1,c2))
det2(v1,v2)  

#Here are the properties of the determinant as a function of the columns.
#1: The function changes sign when you swap the columns.
det2(v2,v1) 

#2: It is a linear function of the first (or second) column.
#Fix the second argument as v2, and we have a function of just one vector.
v3 <- c(1,1)
det2(v1+2*v3,v2); det2(v1,v2)+2*det2(v3,v2)

#3: If the columns are the unit vectors in order, the value is 1.
det2(c(1,0),c(0,1))
#These three properties are sufficient to define the determinant in general.

#An easy way to check for proportional vectors is to use the determinant.
w1 <- c(-2,3); w2 <- (-3)*w1   #clearly proportional
det2(w1,w2)    #as the zero determinant shows

#The magnitude of the determinant is the area of the image of the unit square.
#Start by creating a matrix M.
v1 <- c(2,1); v2 <- c(2,3); M <-cbind(v1,v2); M; det(M) #determinant of M is 4
#Set up the screen with narrow margins and aspect ratio 1:1
pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,5), ylim = c(-1,5), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

arrows(0,0, 1, 0, col = "green")   #first standard basis vector
arrows(0,0, v1[1], v1[2], col = "red") #after applying M
arrows(0,0, 0, 1, col = "green")   #second standard basis vector
arrows(0,0, v2[1], v2[2], col = "red") #after applying M
arrows(v1[1], v1[2], v1[1]+v2[1], v1[2]+v2[2], col = "red")
arrows(v2[1], v2[2], v1[1]+v2[1], v1[2]+v2[2], col = "red")
arrows(1,0, 1, 1, col = "green")  
arrows(0,1, 1, 1, col = "green") 

#The area of the red parallelogram is 4 times the area of the unit square
#A homework problem is to show that if det(M) is positive,
#you must rotate v1 countrclockwise to line it up with v2.
