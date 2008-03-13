\name{lw.FriedmanTest}
\alias{lw.FriedmanTest}
\title{Wrapper for the Friedman test function.}
\description{Wrapper for the Friedman test function.}
\usage{
 lw.FriedmanTest(x)
}
\arguments{
 \item{x}{A matrix of ranks.}
}
\details{
 Calls \code{friedman.test} and returns a vector containing the statistic
 and p value.
}
\value{The statistic and p value returned by \code{friedman.test}.}
\author{Jim Lemon}
\seealso{\code{\link{friedman.test}}}
\keyword{misc}
