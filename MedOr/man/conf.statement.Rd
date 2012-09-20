% File MedOr/man/conf.statement.Rd
% Part of the MedOr package for R (http://www.R-project.org)
% Copyright (C) 2012, Adriano Polpo, Carlos A. de B. Pereira.
% Distributed under GPL 3 or later

\name{conf.statement}

\alias{conf.statement}
\alias{print.conf.statement}

\title{Confidence Statement for Ordered Population Median}
\description{
  Evaluates the Confidence Statement for Ordered Population Median.
}
\usage{
 conf.statement(data,verbose=TRUE)
}
\arguments{
  \item{data}{is a list of observed vectors/samples.}
  \item{verbose}{print the results (TRUE).}
}
\details{
  The confidence statement is evaluated for the population median consedering:
  The population median of the group 1 is lower than 
  the population median of the group 2, and this is lower than
  the population median of the group 3, and so...
  That is, M1 < M2 < ... < Mk, where
  Mj is the population median of the group j.

  The group j is the j-th vector in the data (a list object).
}
\value{
  A list with components:
  \item{            call}{command evaluated.}
  \item{ statement.level}{the confidence statement level.}
  \item{    stat.order.i}{the numbers of the order statistics of i-th group.}
  \item{conf.statement.i}{the values of the order statistics  of i-th group.}
  \item{    total.groups}{the total number of groups.}
  \item{        run.time}{Time spent.}
}
\examples{
  set.seed(1234)
  # Statement: M1 < M2 < M3, where Mj is the population
  #   median of the group j.
  data <- NULL
  data$x1 <- rnorm(10,0,1)
  data$x2 <- rnorm(10,1,1)
  data$x3 <- rnorm(10,2,1)
  conf.statement(data)

  # Statement: M2 < M3 < M1, where Mj is the population
  #   median of the group j.
  data2 <- NULL
  data2$g1 <- data$x2
  data2$g2 <- data$x3
  data2$g3 <- data$x1
  conf.statement(data2)
}
