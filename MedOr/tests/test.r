# MedOr package for R (http://www.R-project.org)
# Copyright (C) 2012 Adriano Polpo, Carlos A. de B. Pereira.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

## This code is used for testing purposes. The MedOr library does not
## depend on it for any of its functionalities

source("loadlibs.r")
loadlibs()


########################################
if (FALSE) {

# Testing the conf.interval function
# Evaluate coverage proportion
repl  <- 1000
n     <- 100
x.m   <- 0
alpha <- 0.95

j1 <- 0
j2 <- 0
repl2 <- repl
for (i in 1:repl) {
  set.seed(1234+i)
  x   <- rnorm(n,x.m,1)
  aux <- conf.interval(x,alpha,FALSE)
  if ((aux$cint1[1] < x.m) && (x.m < aux$cint1[2]))
    j1 <- j1+1
  if (!is.null(aux$cint2[1])) {
    if ((aux$cint2[1] < x.m) && (x.m < aux$cint2[2]))
      j2 <- j2+1
  } else {
    repl2 <- repl2-1
  }
}
cat("Desired significance level: ",alpha,"\n",sep="")
cat("       Covarage proportion: ",j1/repl,"\n",sep="")

}

if (FALSE) {
#############################################
# comparing conf.interval and an asymptotic 
# confidence interval for median
n <- 100
n.rep <- 1000
# desired significance level
alpha <- 0.95
r1 <- rep(0,n.rep)
r2 <- rep(0,n.rep)
r3 <- rep(0,n.rep)
i <- 1234
k <- 1
repeat {
  set.seed(i)
  x <- sort(rnorm(n,0,1))
  aux <- conf.interval(x,alpha,FALSE)

  # exactly confidence interval
  r1[k] <- (aux$cint1[2] - aux$cint1[1])
  a   <- attr(aux$cint1,"conf.level")

  # asymptotic confidence interval
  lim <- round(c(n/2-qnorm(a+(1-a)/2)*sqrt(n)/2,1+n/2+qnorm(a+(1-a)/2)*sqrt(n)/2))
  a1 <- x[lim[1]]
  a2 <- x[lim[2]]

  r2[k] <- (a2-a1)
  if (r2[k] > r1[k]) {
    r3[k] <- 1
  }
  k <- k+1
  i <- i+1
  if (k > n.rep) {
    break
  }
}
cat("exactly better than asymptotic: ",sum(r3)/n.rep,"\n",sep="")
}

############################################
# Evaluateing confidence statement for more 
# than 2 groups.
if (FALSE) {
set.seed(1235)
n <- c(100,100,100,100)
x.m <- c(1,2,3,4)
data <- NULL
data$x1 <- sort(rnorm(n[1],x.m[1],1))
data$x2 <- sort(rnorm(n[2],x.m[2],1))
data$x3 <- sort(rnorm(n[2],x.m[3],1))
data$x4 <- sort(rnorm(n[2],x.m[4],1))

conf.statement(data)
}



