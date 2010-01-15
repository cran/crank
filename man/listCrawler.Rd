\name{listCrawler}
\alias{listCrawler}
\title{Descend a list, applying a function to each element.}
\description{Descend a possibly nested list, seeking the element that has the
 extreme value of a function.}
\usage{
 listCrawler(x,FUN=NULL,maxval=TRUE,
  retval=list(indx=vector("numeric",0),element=NULL,value=NA))
}
\arguments{
 \item{x}{The object that will be the first argument of \samp{FUN}, or a
  possibly nested list of such objects.}
 \item{FUN}{A function that can accept \samp{x} as its first argument.}
 \item{maxval}{Whether to look for maximal (TRUE) or minimal (FALSE) values of
  the function \samp{FUN}.}
 \item{retval}{The list that is eventually returned.}
}
\value{
 A list containing:
  \item{indx}{the indices of the element producing the extreme value of
   \samp{FUN}.}
  \item{element}{The element that produced the extremum.}
  \item{value}{The extreme value of \samp{FUN}.}
}
\details{
 \samp{listCrawler} descends the list structure of \samp{x} applying \samp{FUN}
 to any non-list elements it encounters. If the value of \samp{FUN} is larger or
 smaller than the current extremum (depending upon the value of \samp{maxval}),
 the new value becomes the current extremum.
}
\author{Jim Lemon}
\seealso{\link{list}, \link{listBuilder}}
\examples{
 # a simple example using the square root function
 testlist<-list(list(9,16),list(25,list(36,49)))
 # first get the default maximum
 listCrawler(testlist,sqrt)
 # then the minimum
 listCrawler(testlist,sqrt,maxval=FALSE)
}
\keyword{misc}
