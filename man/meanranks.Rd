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
 \item{ranks}{\samp{x} with any NAs replaced by the mean of unallocated ranks
  for each row.}
 \item{labels}{The vector of labels, defaulting to the integers 1:allranks.}
 \item{mean.ranks}{A vector of mean ranks for each value of allranks.}
}
\details{
 \samp{meanranks} calls \samp{muranks} to complete the rank matrix
 before calculating the mean ranks for each column if there are any
 NAs in \samp{x}.
  
 Note that \samp{cats2ranks} interprets each value in \samp{x} as a nominal
 level variable and its index as the rank, while \samp{meanranks} interprets
 values as ordinal level (ranks). Thus if a matrix or data frame of category
 labels is passed to \samp{meanranks}, it will not give the correct mean
 ranks.
}
\author{Jim Lemon}
\seealso{\link{muranks}, \link{rank}, \link{cats2ranks}}
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
