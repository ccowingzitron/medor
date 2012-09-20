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

library("MedOr")

initial.time <- proc.time()[3]
cat("Loading data... ",sep="")
data(schizophrenia)
a <- schizophrenia
cat("Data loaded... \n",sep="")
run.time <- (proc.time()[3]-initial.time)/60
cat("Time spent: ", sprintf("%.2f",run.time)," min.\n",sep="")
cat("\n",sep="")

aux1 <- rep(NA,length(a[1,]))
aux2 <- rep(NA,length(a[1,]))
aux3 <- rep(0,length(a[1,]))
aux4 <- rep(" ",length(a[1,]))

cat("Analysis started at ",date(),".\n",sep="")
initial.time <- proc.time()[3]
it <- 3
for (i in 2:length(a[1,])) {
  data <- NULL
  data$x1 <- sort(a[a[,1] == 1,i])
  data$x2 <- sort(a[a[,1] == 2,i])
  t1  <- conf.statement(data,FALSE)
  aux1[i] <- t1$statement.level
  data <- NULL
  data$x1 <- sort(a[a[,1] == 2,i])
  data$x2 <- sort(a[a[,1] == 1,i])
  t2  <- conf.statement(data,FALSE)
  aux2[i] <- t2$statement.level
  aux3[i] <- max(aux1[i],aux2[i])
  if (aux1[i] > aux2[i]) {
    aux4[i] <- paste("M1 < X1(",sprintf("%.0f",t1$stat.order.1),") = ",sprintf("%.4f",t1$conf.statement.1)," < ",
                     sprintf("%.4f",t1$conf.statement.2)," = X2(",sprintf("%.0f",t1$stat.order.2),") < M2",sep="")
  } else {
    aux4[i] <- paste("M1 > X1(",sprintf("%.0f",t2$stat.order.2),") = ",sprintf("%.4f",t2$conf.statement.2)," > ",
                     sprintf("%.4f",t2$conf.statement.1)," = X2(",sprintf("%.0f",t2$stat.order.1),") > M2",sep="")
  }
  
  run.time <- (proc.time()[3]-initial.time)/60
  if (run.time > it) {
    it <- it+3
    cat("i: ",sprintf("%.0f",i)," of ",sprintf("%.0f",length(a[1,]))," -- Time elapsed: ",sprintf("%.0f",floor(run.time))," min, so far...\n",sep="")
  }
}
result <- rbind(a,aux4,aux3)
result <- result[,order(result[length(result[,1]),])]
result <- result[,c(1,seq(length(result[1,]),2,-1))]

run.time <- (proc.time()[3]-initial.time)/60
cat("Time spent: ", sprintf("%.2f",run.time)," min.\n",sep="")
cat("Analysis finished... \n",sep="")

# The most differents
if (TRUE) { #change this to TRUE to print and save results.
  print(t(result[67:68,2:11]))
  write.csv(t(result[67:68,2:11]),"result.csv")
}
