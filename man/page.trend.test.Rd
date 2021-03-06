\name{page.trend.test}
\alias{page.trend.test}
\title{Page test for ordered alternatives}
\description{
 calculates the Page test for ordered alternatives.
}
\usage{
 page.trend.test(x,ranks=TRUE)
}
\arguments{
  \item{x}{a 2D matrix of ranks or observations.}
  \item{ranks}{Whether the values in x are ranks or observations.}
}
\details{
 \samp{page.trend.test} will accept a matrix of ranks where the rows 
 represent methods (usually raters) and the columns represent related
 data objects. It apparently handles ties, but not missing values.
 For small values of k (methods) or N (data objects), \samp{page.trend.test} 
 will try to look up the tabled values (as in Siegel & Castellan (1988) for 
 significance. For \samp{k,N > 3,20} or \samp{k,N > 4-10,12}, a normal 
 approximation is returned. Only one of these values will be returned.

 If \samp{ranks} is FALSE, the function ranks the values in \samp{x} and then
 calculates the test. If the values are already ranks, it usually makes
 no difference.
}
\value{
 \item{ranks}{matrix of ranks}
 \item{mean.ranks}{mean ranks of data objects}
 \item{L}{value of the L statistic}
 \item{p.table}{whether the obtained L exceeded the table value for small k,N}
 \item{Z}{The normal approximation for larger k,N}
 \item{pZ}{the probability of the obtained normal value for larger k,N}
}
\references{Siegel, S. & Castellan, N.J.Jr. (1988) Nonparametric
 statistics for the behavioral sciences. Boston, MA: McGraw-Hill.}
\author{Jim Lemon - thanks to Mikhail Trofimov and Michael Kirchhof
 for discovering major errors in the function and supplying the 
 corrections}
\note{The Page test for ordered alternatives is slightly more powerful than
 the Friedman analysis of variance by ranks.}
\examples{
 # Craig's data from Siegel & Castellan, p 186
 soa.mat<-matrix(c(.797,.873,.888,.923,.942,.956,
  .794,.772,.908,.982,.946,.913,
  .838,.801,.853,.951,.883,.837,
  .815,.801,.747,.859,.887,.902),nrow=4,byrow=TRUE)
 page.trend.test(soa.mat)
}
\keyword{misc}
