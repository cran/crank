\name{meanranks}
\alias{meanranks}
\title{Calculate mean ranks with possible missing values}
\description{Calculates mean ranks where some ranks may be missing}
\usage{
 meanranks(x,allranks=NULL,labels=NULL,rankx=FALSE)
}
\arguments{
 \item{x}{A matrix of ranks that may contain ties and NAs. Objects
  ranked are assumed to be columns and ranking methods rows.}
 \item{allranks}{An optional list of all ranks that might have been
  made.}
 \item{labels}{Optional labels for the ranks.}
 \item{rankx}{Whether to convert competition ranks, or any other set of
  numeric values, into the usual mean rankings for ties.}
}
\value{
 A list with the following components:
 \item{ranks}{\code{x} with any NAs replaced by the mean of unallocated ranks
  for each row.}
 \item{labels}{The vector of labels, defaulting to the integers 1:allranks.}
 \item{mean.ranks}{A vector of mean ranks for each value of allranks.}
}
\details{
 \code{meanranks} calls \code{muranks} to complete the rank matrix
 before calculating the mean ranks for each column if there are any
 NAs in \code{x}.
  
 Note that \code{cats2ranks} interprets each value in \code{x} as a nominal
 level variable and its index as the rank, while \code{meanranks} interprets
 values as ordinal level (ranks). Thus if a matrix or data frame of category
 labels is passed to \code{meanranks}, it will not give the correct mean
 ranks.
}
\author{Jim Lemon}
\seealso{\code{\link{muranks}},\code{\link{rank}},
 \code{\link{cats2ranks}}}
\examples{
 # simulate "best/worst" ranking
 x<-matrix(NA,nrow=10,ncol=10)
 for(i in 1:10) {
  nbest<-sample(2:5,1)
  best<-1:nbest
  nworst<-sample(1:5,1)
  worst<-(11-nworst):10
  rankpos<-sample(1:10,nbest+nworst)
  x[i,rankpos]<-c(best,worst)
 }
 x
 meanranks(x)
}
\keyword{misc}
