\name{listBuilder}
\alias{listBuilder}
\title{Build a possibly nested list.}
\description{Build a possibly nested list using the result of a function.}
\usage{
 listBuilder(x,FUN=NULL,fargs=NULL)
}
\arguments{
 \item{x}{The object that will be the first argument of \samp{FUN}, or a
  possibly nested list of such objects.}
 \item{FUN}{A function that can accept \samp{x} as its first argument.}
 \item{fargs}{A list of the remaining arguments to \samp{FUN}.}
}
\value{
 If \samp{x} is not a list and \samp{FUN} is NULL, \samp{x} is returned.
 If \samp{FUN} creates a list from one or more elements of \samp{x}, a
 list or nested list will be returned. Successive calls to \samp{listBuilder}
 can rapidly create very large, deeply nested list structures.
}
\details{
 \samp{listBuilder} descends the list structure of \samp{x} if it is a
 list until it encounters a non-list element. It then passes that element
 as the first argument to \samp{FUN} and returns the value of \samp{FUN}.
 This may be a list of elements, replacing the original element, hence the
 name.
}
\author{Jim Lemon}
\seealso{\link{list}}
\examples{
 # define a function that splits a vector into a list
 splitvec<-function(x) {
  xlen<-length(x)
  if(xlen > 1) {
   newx<-vector("list",xlen)
   for(newlist in 1:xlen) newx[[newlist]]<-x[newlist]
   return(newx)
  }
  return(x)
 }
 testlist<-list(c(9,16),list(25,c(36,49)))
 listBuilder(testlist,splitvec)
}
\keyword{misc}
