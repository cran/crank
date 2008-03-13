\name{permute}
\alias{permute}
\title{Permute a vector.}
\description{Permute the values contained in a vector.}
\usage{
 permute(x)
}
\arguments{
 \item{x}{The vector of values that are to be permuted.}
}
\value{
 A matrix in which each row is a permutation of the values in \code{x}.
}
\details{
 \code{permute} calculates the number of permutations and creates a matrix
 with that number of rows. It fills the first column with the elements of
 \code{x} in groups large enough to cover the permutations of a vector with
 one less value. It then fills the remaining columns by calling itself with
 all values except the one in the first row of the current block. If
 \code{x} has only two values, it returns the trivial permutation of \code{x}
 and its reverse.
}
\author{Jim Lemon}
\seealso{\code{\link{fillArows}}, \code{\link{fillArow}}}
\examples{
 permute(c(5,8,3,9))
}
\keyword{misc}
