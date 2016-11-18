\name{symmetricOuter}
\alias{symmetricOuter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Efficiently compute outer products for symmetric functions
}
\description{
Computes \code{FUN(x,y,...)} for each combination of \code{x} and \code{y} in \code{V}, where \code{FUN(x,y,...)} is guaranteed to be equal to \code{FUN(y,x,...)}. For reasonably complicated \code{FUN}, should be twice as quick as \code{outer}.
}
\usage{
symmetricOuter(V, FUN, ...)
}
\arguments{
  \item{V}{
a vector of input values. Array- and matrix-type inputs are not supported.
}
  \item{FUN}{
a function of at least two arguments, or the name of one. Must be vectorised in its first two arguments (see \code{\link{Vectorize}}).
}
  \item{\dots}{
further arguments to \code{FUN}.
}
}
\details{
Since the implementation is pure R, there will be some additional computational overhead not present in ordinary \code{outer}. This may make \code{symmetricOuter} actually perform slower than \code{outer} for very simple funtions, such as the basic arithmetic operators. If \code{FUN} is either "+" or "*", then a call is made to \code{outer}, with a warning, as that will be faster.
}
\value{
A matrix of dimension \code{c(length(V),length(V))} where the (i,j)^{th} element is equal to \code{FUN(V[i],V[j])}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
James Lawrence
}
\note{
If any arguments to \code{FUN} partially match either "V" or "FUN", then you will probably need to explicitly name the first two arguments.
}

\seealso{
\code{\link{outer}}, \code{\link{Vectorize}}, \code{\link{kronecker}}.
}
\examples{
## some timings and example usage
testSymmetricFun <- Vectorize(function(i,j) mean(i+j+1:100))
## for a complicated function like this, 
## symmetricOuter should take half the time
system.time(x <- symmetricOuter(1:250,testSymmetricFun))
system.time(x2 <- outer(1:250,1:250,testSymmetricFun))
identical(x,x2) ## TRUE
## for a very simple function,
## symmetric outer might be quite a bit slower
## also note that no check is made to see
## if FUN is *actually* symmetric
system.time(x <- symmetricOuter(1:2500,"-"))
system.time(x2 <- outer(1:2500,1:2500,"-"))
identical(x,x2) ## FALSE, "-" is not symmetric!
rm(x)
rm(x2)
}
