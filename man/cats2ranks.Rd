\name{cats2ranks}
\alias{cats2ranks}
\title{Ordered option selections to ranks}
\description{
 Convert ordered option selections to ranks, assigning the mean of unused
 ranks to unselected options.
}
\usage{
 cats2ranks(x,cats=NULL)
}
\arguments{
 \item{x}{A matrix or data frame of numeric or characters labels for options.
  Rows are interpreted as cases or respondents and columns are interpreted as
  the order of option selections, beginning with the highest ranking
  (usually something like "Most important") and descending.}
 \item{cats}{The range of numbers that represent options. The default is the
  vector of unique entries in \samp{x}.}
}
\value{
 A list with four components:
 \item{ranks}{The matrix of completed ranks.}
 \item{cats}{The vector of options as passed or calculated.}
 \item{ranksum}{The sum of ranks for each option.}
 \item{rankcount}{The number of times each option was selected.}
}
\details{
 \samp{cats2ranks} converts ordered option selections to mean ranks. It is
 useful in the situation where a respondent is asked to select one of a number
 of options as the most important, another as the second most important, and
 so on. It counts the number of times each option code appears in each column
 and calculates the mean ranking of options. It is expected that there will be
 fewer selections available than there are options, thus creating the opportunity
 for biased rankings. This can occur when one or more options are not commonly
 chosen, but are given extreme (usually high) ranks when they are. The function
 calculates the mean of unallocated ranks and assigns this to all options not
 chosen by each respondent, correcting for this bias. The correction assumes
 that the respondent does not differentiate between unranked options, but these
 are all ranked lower than the options selected.

 \samp{cats2ranks} is especially useful when respondents do not select the same
 number of options. The mean of unallocated ranks is calculated for each
 respondent so that all options are entered into the calculation of mean ranks.

 Note that \samp{cats2ranks} interprets each value in \samp{x} as a nominal
 level variable and its column index as the rank, while \samp{meanranks}
 interprets values as ordinal level (ranks). Thus if a matrix or data frame
 of ranks is passed to \samp{cats2ranks}, it will not give the correct mean
 ranks or relative positions.
}
\author{Jim Lemon}
\seealso{\link{muranks},\link{meanranks}}
\examples{
 # first a standard 1:m numerically coded selection
 opchoice<-matrix(NA,nrow=40,ncol=5)
 for(i in 1:40) opchoice[i,]<-sample(1:10,5)
 opchoice
 cats2ranks(opchoice)
 # now a messy character choice with missing values
 opchoice<-matrix(NA,nrow=40,ncol=5)
 tencolors<-c("red","green","blue","yellow","magenta","cyan",
  "purple","orange","brown","pink")
 for(i in 1:40) {
  nchoices<-sample(3:5,1)
  opchoice[i,1:nchoices]<-sample(tencolors,nchoices)
 }
 opchoice
 cats2ranks(opchoice)
}
\keyword{misc}
