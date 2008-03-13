\name{getLWargs}
\alias{getLWargs}
\title{Get the information about a matrix of ranks.}
\description{Get the information required for imputing missing ranks.}
\usage{
 getLWargs(x)
}
\arguments{
 \item{x}{A matrix of ranks, usually containing missing values.}
}
\value{
 A list containing the following:
 \item{ranksums}{The column sums of the complete rows of the matrix.}
 \item{Arows}{The indices of the row(s) with the minimal number of missing
  values.}
 \item{nArows}{The number of Arows.}
 \item{Brows}{The indices of the complete rows.}
}
\details{
 \code{getLWargs} calculates the information required for \code{fillArows}
 and \code{fillArow} to impute the missing ranks in a matrix.
}
\author{Jim Lemon}
\seealso{\code{\link{listBuilder}}}
\examples{
 # The first example matrix from Lim and Wolfe (2002)
 lwmat<-matrix(c(3,1,2,4,NA,2,1,NA,2,NA,1,NA),nrow=3,byrow=TRUE) 
 getLWargs(lwmat)
}
\keyword{misc}
