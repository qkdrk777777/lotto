% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5 sampling.R
\name{lottosample}
\alias{lottosample}
\title{sample}
\usage{
lottosample(n, keep = NULL, drop = NULL, hold = NULL, a = NULL,
  write = F)
}
\arguments{
\item{n}{= number of sample}
}
\description{
sample
}
\examples{
lottosample(5)
(k<-sort(num_per()[as.numeric(data[1,1:6])],dec=T))
lottosample(n=5,keep=k[1])
}
