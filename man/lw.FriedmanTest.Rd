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
 Calls \samp{friedman.test} and returns a vector containing the statistic
 and p value.
}
\value{The statistic and p value returned by \samp{friedman.test}.}
\author{Jim Lemon}
\seealso{\link{friedman.test}}
\keyword{misc}
