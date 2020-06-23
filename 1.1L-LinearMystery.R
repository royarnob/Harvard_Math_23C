#Math S-323 Script 3L-LinearMystery.R

#Last modified: June 21, 2014 by Paul Bamberg
#Include the line
#source("3L-LinearMystery.R") (without the #)
#Every time you execute this line, the function fMyst changes.
#It is guaranteed to be linear and invertible.

#Topic 1 - Define a mystery linear function fMyst: R^2 ---> R^2
E <- new.env()
E$a1 <- runif(2,-3,3)
E$a2 <- c(runif(1,-4,0),runif(1,1,4))
fMyst <- function(v) v[1]*E$a1+v[2]*E$a2
 