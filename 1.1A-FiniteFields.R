#Math 23 Script 1.1A-Finite Fields.R
#Module 1, Week 1, subsection 1.1

#Last modified: August 26, 2014 by Paul Bamberg

#Topic 1 - Why the real numbers form a field

#In a field there are two operations, addition and multiplication.
#They must obey the usual rules of arithmetic and algebra.
#Here are the axioms for a field.
#Set up the plots pane to display lines of text at the bottom.
pars<-par(mar=c(10,1,2,1)+0.1)  #leave lots of space at bottom
plot(NULL, xlim = c(0,1) , ylim = c(0,1), xlab = "", ylab = "", axes = FALSE,
     main = "Axioms for a field")

#We can check these axioms in R, using numeric quantities.

#A1 - Addition is commutative - order does not matter
mtext("A1: Addition is commutative: a+b=b+a",1,1)
3+ 1.2; 1.2 + 3  

#A2 - Addition is associative - no parentheses are needed
mtext("A2: Addition is associative: (a+b)+c = a+(b+c)",1,2)
(2 + 3) + 4.2; 2 + (3 + 4.2);2 + 3 + 4.2; 

#A3 - There is an additive identity, called zero
mtext("A3: Additive identity 0 exists: a+0 = a",1,3)
3.14 + 0

#A4 Every element has an additive inverse: the inverse of x is called -x.
mtext("A4: Additive inverse -a exists for each element: a+(-a) = 0",1,4)
x <- 3   #assign a value to a variable
y  <- -x ; y      #this is its additive inverse
x + y             #the sum of x and its additive inverse is 0

#M1 - Multiplication is commutative - order does not matter
mtext("M1: Multiplication is commutative: ab = ba",1,5)
3 * 1.2; 1.2 * 3

#M2 - Multiplication is associative - no parentheses needed
mtext("M2: Multiplication is associative: (ab)c = a(bc)",1,6)
(2 * 3) * 4.2; 2 * (3 * 4.2);2 * 3 * 4.2; 

#A3 - There is a multiplicative identity, called one
mtext("M3: Multiplicative identity 1 exists: 1a = a",1,7)
3.14 * 1

#A4 Every element except zero has a multiplicative inverse: the inverse of x is 1/x.
mtext("M4: Multiplicative inverse 1 exists for each nonzero element;a(1/a) = 1",1,8)
x <- 3   #assign a value to a variable
y  <- 1/x ; y      #this is its inverse
x * y           #the product of x and its multiplicative inverse is 1
#The integers do not form a field because the inverse is in general not an integer.

#D1 - Multiplication is distributive with respect to addition
mtext("D1: Multiplication is distributive:a(b+c) = ab+ac",1,9)
a <- 2; b <- 1.5 ; c <- 3.5
a*(b + c); a*b + a*c
#We can use == to test for equality
a*(b + c) == a*b + a*c

#Topic 2 - Making a finite field, with only five elements
#The secret is to do "modular arithmetic": divide by 5 and keep the remainder.
(2 + 4)%% 5   #the remainder is 1
#We can define our own function to accomplish this.
#If the function name begins and ends with % we can use it as a binary operator
"%+5%" <- function(x,y) (x+y)%%5
"%+5%"(2,4)    #it really is a function of two variables
2 %+5% 4       #but we can use it as a binary operator

#Making a table of addition facts using the outer() function in R
#This function takes two vectors and a function as arguments.
#It applies the function to each pair of components
v = 1:5; v     #this is a vector with five components
#In Z_5, 5 is an alternate name for zero.
outer(v,v, "%+5%")     #calculate all possible sums
#Inspect the table to see that 0 (same as 5) is the additive identity 
#and that every element has an additive inverse.

#Define multiplication similarly
"%*5%" <- function(x,y) (x*y)%%5
outer(v,v, "%*5%")     #calculate all possible products
#Inspect the table to see that 1 is the identity 
#and that every nonzero element has a multiplicative inverse.

#Topic 3 - A useful rule for finding multiplicative inverses

#If you ask R to add vectors, it does so component by component
v; v%+5%v

#If you ask R to multiply vectors, it also does so component by component
#This is not a standard vector operation!
v                    #the original vector
v%*5%v               #the square of each component of the vector
v%*5%v%*5%v          #the cube of each component is its inverse  
v%*5%v%*5%v%*5%v     #as we can check by multiplying by v to show that the 4th power is 1

#The general rule is that in the finite field Z_p, where p is a prime,
#the (p-1)st power of each nonzero element is 1 (Fermat's little theorem)
#the (p-2)nd power of each nonzero element is its multiplicative inverse

