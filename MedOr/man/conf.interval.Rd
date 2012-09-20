% File MedOr/man/conf.interval.Rd
% Part of the MedOr package for R (http://www.R-project.org)
% Copyright (C) 2012, Adriano Polpo, Carlos A. de B. Pereira.
% Distributed under GPL 3 or later

\name{conf.interval}

\alias{conf.interval}
\alias{print.conf.interval}

\title{Confidence Interval for Population Median}
\description{
  Evaluates a confidence interval for population median.
}
\usage{
 conf.interval(x,alpha=0.95,verbose=TRUE)
}
\arguments{
  \item{      x}{observed vector/sample.}
  \item{  alpha}{confidence level.}
  \item{verbose}{print time elapsed (TRUE).}
}
\details{
  Evaluates a confidence interval for population median based in the order
  statistics. It is considered the distribution of order statistics to evaluate
  the confidence level of the interval. It is not assumed any asymptotic
  distribution.
}
\value{
  A list with components:
  \item{   cint1}{A vector with the confidence interval limits, given by the
                  biggest confidence level lower than or equal to alpha.}
  \item{   cint2}{A vector with the confidence interval limits, given by the
                  lowest confidence level bigger than or equal to alpha. If
                  there is not interval with confidence level bigger than or
                  equal to alpha, then cint2 is NULL.}
  \item{   alpha}{Desired confidence level.}
  \item{run.time}{Time spent.}
  \item{    call}{command evaluated.}
}
\examples{
  x <- rnorm(10,0,1)
  conf.interval(x)
}
