# reposition an element out of place just before the subsequent element
# in a precedence pair
elrepos<-function(x,i1,i2) {
 tmp<-x[i2]
 x<-x[-i2]
 if(i1 > 1) return(c(x[1:(i1-1)],tmp,x[i1:length(x)]))
 else return(c(tmp,x))
}

# sorts the elements in L with respecte to the precedence rules listed in x
spsort<-function(x,L=NULL) {
 # if L is not present, assume it is composed of all the elements in x
 if(is.null(L)) L<-unique(as.vector(x))
 nrep<-0
 for(i in 1:nrow(x)) {
  i1<-which(L==x[i,1])
  i2<-which(L==x[i,2])
  if(i2<i1) {
   L<-elrepos(L,i1,i2)
   nrep<-nrep+1
  }
 }
 cat(nrep,"elements repositioned\n")
 return(L)
}

