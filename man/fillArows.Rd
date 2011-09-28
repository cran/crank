\name{fillArows}
\alias{fillArows}
\title{Impute ranks using the existing values of rankings}
\description{Imputes missing ranks using the Lim-Wolfe procedure}
\usage{
 fillArows(x,maxcon=TRUE)
}
\arguments{
 \item{x}{A matrix of ranks that may contain ties and NAs. Columns
  represent objects ranked and rows represent ranking methods.}
 \item{maxcon}{Whether to impute rankings maximally consistent with
  the existing ones (TRUE) or minimally consistent (FALSE).}
}
\value{
 A list of one or more completed matrices of ranks, possibly nested.
}
\details{
 \samp{fillArows} imputes missing ranks by examining the completed ranks for
 each set of rows that have the same number of missing ranks. If more than one
 row has the minimum number of missing values, the order of these rows is
 permuted and the matrix \samp{x} becomes a list of matrices in which the
 values in the rows will be imputed in different orders. Another level of
 permutation and multiplication of matrices may occur in \samp{fillArow} to
 which the matrices are passed for the actual imputation. The function
 \samp{getLWargs} is called to get the arguments for \samp{fillArow}. 
 See Lim and Wolfe (2002) for details of this process.
}
\references{
 Lim, D.H. & Wolfe, D.A. (2002) An efficient alternative to average
 ranks for testing with incomplete ranking data. Biometrical Journal,
 43(2): 187-206.
}
\author{Jim Lemon}
\seealso{\link{lwscreen}, \link{getLWargs}, \link{fillArow}}
\examples{
 # The first example matrix from Lim and Wolfe (2002)
 lwmat<-matrix(c(3,1,2,4,NA,2,1,NA,2,NA,1,NA),nrow=3,byrow=TRUE)
 # complete with maximal consistency, permuting row order
 fillArows(lwmat)
 # now with minimal consistency as above
 fillArows(lwmat,maxcon=FALSE)
}
\keyword{misc}
