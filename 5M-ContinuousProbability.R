#Math 23c Script 5M-ContinuousProbability.R

#Last modified: February 19, 2018

#Topic 1: Constructing an element of the power set 2^S
#that is not in the image of f: S ---> 2^S,
#This result is important when S is infinite. A computer can only do the finite case.
n <- 10
S <- 1:n
#Approach 1 to selecting a subset: sample from TRUE and FALSE
smpTF <-sample(c(TRUE,FALSE),n,replace=TRUE);smpTF
#Approach 2: list the elements of the set that correspond to TRUE
which(smpTF == TRUE)
#Select n subsets using approach 1
subs <- vector("list",n)
for(i in 1:n) {
  subs[[i]] <- sample(c(TRUE,FALSE),n,replace=TRUE)
  print(subs[[i]])
}
#Now use the Cantor diagonal approach
#Extract the diagonal elements
diag <- logical()   #empty vector
for(i in 1:n) {
  diag <- c(diag,subs[[i]][i])
}
diag
#Now reverse all the truth values
x <- !diag; x; 
for(i in 1:n) {
  print(subs[[i]])
}
x #The logical vector x cannot appear anywhere on the list

#Now do exactly the same construction using approach 2
subsets <- vector("list",n)
for(i in 1:n) {
  subsets[[i]] <- which(subs[[i]] == TRUE)
  print(subsets[[i]])
}
#For the diagonal construction, select those sets that do not include their own index
xx <- numeric()   #empty vector
for(i in 1:n) {
  if(!is.element(i, subsets[[i]]))
     xx <- c(xx,i)
}
xx    #This vector cannot appear on the list
x     #It's the same as what we constructed earlier

#Topic 2: Discrete and mixed distribution functions
#A discrete density (mass) function is defined only for the integers.
dbinom(3, 7, 0.5); choose(7,3)/2^7
dbinom(3.5,7, 0.5)
#The corresponding distribution function is defined for all real x
pbinom(3, 7, 0.5) 
pbinom(3.5,7, 0.5)   #X <= 3.5 if and only if X <= 3
pbinom(-1, 7, 0.5)  #correct - X cannot be negative
pbinom(10, 7, 0.5)  #correct - X is sure to be less than 10
#We can plot the distribution function
f <- function(x) pbinom(x, 7, 0.5)
curve(f, -2, 9, n = 1000)  #plot a large number n of points to show discontinuity

#Mixing discrete and continuous distributions
#This is easily done by using the distribution function
F <- function(x) {0.2*((x > 0) & (x <= 1)) + (x^2/5)*((x > 1) & (x <= 2)) +
               0.8*((x > 2) & (x <= 3)) + 1*(x > 3)}
curve(F, -1, 4, n= 500)    #use large n to show discontinuities
#The only requirement is that the function value must increase from 0 to 1
#The points of discontinuity are those where P(X = x) is nonzero

#There are two ways to compute the expectation of this variable
#Use the mass function: 0.2 for X = 0 and 0.2 for X = 3 contributes 0.6
#For 1 <= X <= 2 use the density function F'(x) = 0.4x, multiply by x, and integrate.
mux <- function(x) (0.4*x)*x
integrate(mux, 1, 2)$value + 0.6     #this is the expectation of X

#Other approach: use the tail-integral theorem
tail <- function(x)  (1-F(x))     #probability that X > x
curve(tail, -1, 4, n= 500)
integrate(tail, 0, 3)$value

#Topic 3: Dyadic integration in two dimensions
#Integrate the function x^2 + y^2 over the unit circle - answer should be pi/2.
#We subdivide dyadically the four unit squares with -1 <= x,y <= 1
#and define the function of a vector

f <- function(v) (v[1]^2+v[2]^2)*(v[1]^2+v[2]^2 <= 1)   #zero outside the unit circle
N <- 2     #level of dyadic subdivision
#Let's evaluate in the center of each dyadic square
x <- seq(from = (-1+2^(-N)/2), to = (1-2^(-N)/2), by = 2^(-N)); x
y <- seq(from = (-1+2^(-N)/2), to = (1-2^(-N)/2), by = 2^(-N)); y
dyadics <- expand.grid(x,y); dyadics
sum(f(dyadics))*4^(-N)

#Repeat with a large value of N and we should get close to the correct value.
N <- 8     #level of dyadic subdivision
#Again, evaluate in the center of each dyadic square
x <- seq(from = (-1+2^(-N)/2), to = (1-2^(-N)/2), by = 2^(-N))
y <- seq(from = (-1+2^(-N)/2), to = (1-2^(-N)/2), by = 2^(-N))
dyadics <- expand.grid(x,y)
sum(f(dyadics))*4^(-N); pi/2    #comes out close to the correct answer

#Alternative approach: use Fubini's theorem, integrating first over x.
#Choose the limits of integration to lie on the boundary of the circle
g <- function(x,y) x^2 + y^2
integrate(g, -1/2, 1/2, y=sqrt(3)/2)$value  #example for a specific y
#For each y, do the integral over x
xint <- function(y) integrate(g, -sqrt(1-y^2), sqrt(1-y^2), y)$value
integrate(Vectorize(xint), -1, 1)$value; pi/2     #exactly right






