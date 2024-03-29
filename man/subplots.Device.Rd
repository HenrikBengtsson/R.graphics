%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{subplots.Device}
\alias{subplots.Device}
\alias{Device.subplots}
\alias{subplots.Device}
\alias{subplots,Device-method}


\title{Creates a grid of subplots}

\description{
  Creates a grid of subplots in the current figure. If arguments
  \code{nrow} and \code{ncol} are given a \code{nrow}-by-\code{ncol}
  grid of subplots are created. If only argument \code{n} is given
  then a r-by-s grid is created where |r-s| <= 1, i.e. a square or almost
  a square of subplots is created. If \code{n} and \code{nrow} is
  given then a grid with \code{nrow} rows and at least \code{n} subplots
  are created. Similar if \code{n} and \code{ncol} is given.
  The argument \code{byrow} specifies if the order of the subplots
  should be rowwise (\code{byrow=TRUE}) or columnwise.
}

\synopsis{subplots.Device(this, n=1, nrow=NULL, ncol=NULL, byrow=TRUE, ...)}
\usage{Device$subplots(n=1, nrow=NULL, ncol=NULL, byrow=TRUE, ...)}

\arguments{
  \item{n}{If given, the minimum number of subplots.}
  \item{nrow}{If given, the number of rows the grid of subplots should
              contain.}
  \item{ncol}{If given, the number of columns the grid of subplots should
              contain.}
  \item{...}{Not used.}
}

\value{Returns the \code{\link[base]{matrix}} containing the order of plots.}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
   Device$subplots(nrow=2, ncol=3)  # 2-by-3 grid of subplots
   Device$subplots(n=6, nrow=2)     # 2-by-3 grid of subplots
   Device$subplots(n=5, ncol=2)     # 3-by-2 grid of subplots
   Device$subplots(1)               # (Reset) to a 1-by-1 grid of subplots
   Device$subplots(2)               # 1-by-2 grid of subplots
   Device$subplots(3)               # 2-by-2 grid of subplots
   l <- Device$subplots(8)          # 3-by-3 grid of subplots
   layout.show(length(l))
}

\seealso{
  \code{\link[graphics]{layout}} and \code{layout.show}().
}
\keyword{internal}
\keyword{methods}
