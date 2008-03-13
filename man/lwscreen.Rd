\name{lwscreen}
\alias{lwscreen}
\title{Impute ranks using the existing values of rankings}
\description{Completes a matrix with missing ranks for the values maximally
 and minimally consistent with existing values using the Lim-Wolfe procedure}
\usage{
 lwscreen(x,scrtest="lw.FriedmanTest")
}
\arguments{
 \item{x}{A matrix of ranks that may contain ties and NAs. Columns
  represent objects ranked and rows represent ranking methods.}
 \item{scrtest}{What test to use to determine the maximally and
  minimally consistent imputed values.}
}
\value{
 The maximal and minimal statistics and p values for the list of completed
 rank matrices obtained.
}
\details{
 \code{lwscreen} calls \code{fillArows} to impute the missing ranks in the
 matrix \code{x}. It then applies \code{scrtest} to all the matrices returned
 and finds the minimum and maximum values. See Lim and Wolfe (2002) for
 details of the algorithm.
 
 The algorithm for finding the maximally consistent and inconsistent rank
 imputations is extremely computer intensive, creating large numbers of
 permuted matrices when tied ranksums or multiple rows with the same number
 of missing values are encountered. The APA election example in Lim and Wolfe
 (2002) is beyond the capability of the average PC in the present
 implementation.
}
\references{
 Lim, D.H. & Wolfe, D.A. (2002) An efficient alternative to average
 ranks for testing with incomplete ranking data. Biometrical Journal,
 43(2): 187-206.
}
\author{Jim Lemon}
\seealso{\code{\link{lw.FriedmanTest}},\code{\link{listBuilder}},
 \code{\link{fillArows}}}
\examples{
 # The first example matrix from Lim and Wolfe (2002)
 lwmat<-matrix(c(3,1,2,4,NA,2,1,NA,2,NA,1,NA),nrow=3,byrow=TRUE)
 lwscreen(lwmat)
}
\keyword{misc}
