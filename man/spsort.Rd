\name{spsort}
\alias{spsort}
\title{Simple partial sorting of a vector of elements.}
\description{Sort the elements in a vector according to a set of precedence
 rules.}
\usage{spsort(x,L=NULL)}
\arguments{
 \item{x}{A matrix or data frame with at least two columns. The first two 
  columns are interpreted as precedence pairs, meaning that the element in
  column 1 should appear before the one in column 2.}
 \item{L}{The vector of elements to be sorted. If NULL, it becomes all of the
  unique elements in \samp{x[1:2,]}.}
}
\details{
 \samp{spsort} steps through rows of \samp{x} identifying the positions of the 
 leading and trailing elements in each rule. If the leading element in the rule
 does not precede the trailing element in L, its position in L is moved to just
 ahead of the trailing element. If all of the possible precedence rules for the
 vector L are specified, the sorting will be unique. In most cases, the order
 of the result will depend upon the initial order of L and the order of the
 precendence rules.
}
\value{The vector \samp{L} sorted by the rules in \samp{x}.}
\author{ Jim Lemon }
\examples{
 # Pedro's example
 Smaller<-c("ASD", "DFE", "ASD", "SDR", "EDF", "ASD")
 Larger<-c("SDR", "EDF", "KLM", "KLM", "SDR", "EDF")
 matComp<-cbind(Smaller,Larger)
 spsort(matComp)
 # scramble the order of rules
 nmatrows<-nrow(matComp)
 spsort(matComp[sample(1:nmatrows,nmatrows),])
 # David Urbina's example
 priors<-c("A","B","C","C","D","E","E","F","G")
 posts<-c("E","H","A","D","E","B","F","G","H")
 dinnerMat<-cbind(priors,posts)
 spsort(dinnerMat)
 # add the condition that the taquitos must precede the guacamole
 dinnerMat<-rbind(dinnerMat,c("G","B"))
 spsort(dinnerMat)
 # scramble the rows
 nmatrows<-nrow(dinnerMat)
 spsort(dinnerMat[sample(1:nmatrows,nmatrows),])
}
\keyword{misc}
