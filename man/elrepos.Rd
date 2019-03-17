\name{elrepos}
\alias{elrepos}
\title{Move the position of an element in a vector.}
\description{Move the position of an element in a vector.}
\usage{elrepos(x,i1,i2)}
\arguments{
 \item{x}{A vector of unique values.}
 \item{i1,i2}{The position (i1) in the vector of an element that should be ahead of 
  the element in position i2.}
}
\details{
 \samp{elrepos} saves the element in position i1 of the vector x and removes
 that element from x. It then inserts the element that was in position i1 just 
 before the element in position i2.
}
\value{The vector \samp{x} with the position of one element changed as above.}
\note{Currently this function is only useful to perform the position changing
 for the function \samp{spsort}.}
\seealso{spsort}
\author{Jim Lemon}
\examples{
 x<-unlist(strsplit("lemon",""))
 y<-elrepos(x,3,1)
 z<-elrepos(y,2,1)
 paste0(z,collapse="")
}
\keyword{misc}
