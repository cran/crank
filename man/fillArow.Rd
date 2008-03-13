\name{fillArow}
\alias{fillArow}
\title{Impute a row of ranks using the existing values of rankings}
\description{Imputes a row of missing ranks using the Lim-Wolfe procedure}
\usage{
 fillArow(x,ranksums=NA,Arow,maxcon=TRUE)
}
\arguments{
 \item{x}{A matrix of ranks that may contain ties and NAs. Columns
  represent objects ranked and rows represent ranking methods.}
 \item{ranksums}{The sums of ranks of all complete rows in \code{x}.}
 \item{Arow}{The row of \code{x} that is to be completed.}
 \item{maxcon}{Whether to impute rankings maximally consistent with
  the existing ones (TRUE) or minimally consistent (FALSE).}
}
\value{
 The matrix \code{x} with row \code{Arow} completed or a list of such
 matrices, possibly nested.
}
\details{
 \code{fillArow} imputes missing ranks in the row designated by \code{Arow}
 using the information in \code{ranksums}. If the ranks already completed
 provide information on the order of imputation, that is used directly for
 imputed ranks of maximal consistency or inversely for imputed ranks of
 minimal consistency. If the existing ranks do not provide such information,
 the missing ranks are permuted, and a list of matrices with all the
 permutations is substituted. This may involve a recursive call to 
 \code{fillArow} and produce a nested list of matrices. See Lim and Wolfe 
 (2002) for details of this process.
}
\references{
 Lim, D.H. & Wolfe, D.A. (2002) An efficient alternative to average
 ranks for testing with incomplete ranking data. Biometrical Journal,
 43(2): 187-206.
}
\author{Jim Lemon}
\seealso{\code{\link{lwscreen}},\code{\link{listBuilder}},
 \code{\link{fillArows}}}
\examples{
 # The first example matrix from Lim and Wolfe (2002)
 lwmat<-matrix(c(3,1,2,4,NA,2,1,NA,2,NA,1,NA),nrow=3,byrow=TRUE)
 # complete the second row with maximal consistency
 fillArow(lwmat,lwmat[1,],2)
 # now with minimal consistency
 fillArow(lwmat,lwmat[1,],2,maxcon=FALSE)
}
\keyword{misc}
