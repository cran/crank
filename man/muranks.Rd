\name{muranks}
\alias{muranks}
\title{Complete a matrix of rankings}
\description{
 Fills an incomplete matrix of rankings with means of
 unallocated ranks}
\usage{
 muranks(x,allranks=NULL,rankx=FALSE)
}
\arguments{
 \item{x}{A vector or matrix of rankings that may contain ties and NAs.
  Objects ranked are assumed to be columns and ranking methods rows.}
 \item{allranks}{An optional list of all ranks that might have been
  allocated. Defaults to the unique values in \code{x}.}
 \item{rankx}{Whether to apply the \code{rank} function (see Details).}
}
\value{
 A matrix similar to \code{x} in which any NAs are replaced by the mean of
 unallocated ranks for each row.
}
\details{
 \code{muranks} assumes that the values in \code{x} are rankings with
 values in the set \code{allranks} or if that is NULL, between 1 and the
 number of columns or values in \code{x}. If any values in \code{x} are
 outside this range, or if the missing ranks are not sequential, the
 function will drop that row with a warning.
 
 For each row, the function finds the mean of those ranks in \code{allranks}
 that were not allocated and substitutes that value for any missing values
 in the row.

 If \code{rankx} is TRUE, each row is passed to \code{rank}. This will
 convert competition ranks or any set of numbers to the usual mean rankings.
 This will also override the rejection of rows in which the missing ranks
 are not sequential, and may produce counterintuitive imputed ranks.
}
\note{
 \code{muranks} will impute ranks for "best/worst" ranking, where the 
 method (rater) allocates the highest ranks to the most preferred data 
 objects and the lowest ranks to the least preferred. The mean of all 
 unallocated ranks is imputed for unranked data objects. It is assumed that 
 unranked data objects are considered less preferred than those allocated 
 high ranks, more preferred than those allocated low ranks, and not 
 differentiated from each other. If this assumption is not satisfied, 
 \code{muranks} will warn the operator that one or more rows have been 
 dropped. To explain this behavior, consider the case in which a method 
 allocates the ranks 1,2,3,5,7,8 to eight data objects. Two ranks have not 
 been allocated, 4 and 6. It would be possible to impute the mean, 5, to 
 both, but this ignores the implicit information that the two data objects 
 were differentiated by the rank 5, which is "between" them. Only in the 
 unlikely case that both were considered equivalent to the object ranked 5 
 would this be correct, as there is no way to establish which was more or 
 less preferred. The operator should be aware that if \code{rankx} is TRUE, 
 the unranked objects will be allocated the lowest ranks, which is unlikely 
 to be correct.
}
\author{Jim Lemon}
\seealso{\code{\link{meanranks}},\code{\link{rank}}}
\examples{
 # simulate ranking from the top with variable completion
 x<-matrix(NA,nrow=10,ncol=10)
 for(i in 1:10) {
  nx<-sample(2:10,1)
  xx<-sample(1:10,nx)
  x[i,xx]<-1:nx
 }
 x
 muranks(x)
}
\keyword{misc}
