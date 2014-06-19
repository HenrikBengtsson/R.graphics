%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.1D.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getTopoColors.Colors}
\alias{getTopoColors.Colors}
\alias{Colors.getTopoColors}
\alias{getTopoColors.Colors}
\alias{getTopoColors,Colors-method}


\title{Creates topo colors from data}

\synopsis{getTopoColors.Colors(this, x, x.range=NULL, dim.range=NULL, ncolors=256, ...)}
\usage{Colors$getTopoColors(x, x.range=NULL, dim.range=NULL, ncolors=256, ...)}

\description{
  Generated topo colors from a vector of values specified by argument
  \code{x}. The interval the values can span can be specified by the
  argument \code{x.range}. Everything outside the range is safely
  thresholded. If \code{x.range} is \code{\link[base]{NULL}}, the range of \code{x}
  will be used. The interval of the grayscale generated can be specified
  by the argument \code{dim.range}. By default this is the full grayscale
  range, i.e. [0,1].

  The returned colors has the same dimensions as argument \code{x}.
}

\arguments{
  \item{x}{A matrix of size N, where N is the number of data values.}
  \item{x.range}{A vector of length two.}
  \item{dim.range}{A vector of length two.}
  \item{ncolors}{Maximum number of unique colors to generate.
   Default value is \code{256}.}
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{vector}} of \code{\link[base]{character}} strings of length equal to the number of
 rows (or the length) of \code{x}. The resulting vector contains character
 strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{\dontrun{See help(Color.getGray) for an example.}}

\keyword{color}

\seealso{
  \code{\link[R.graphics:getGray.Colors]{*getGray}()},
  \code{\link[R.graphics:getHSV.Colors]{*getHSV}()},
  \code{\link[R.graphics:getRGB.Colors]{*getRGB}()}.
  \code{\link[grDevices]{palette}}.
  For more information see \code{\link{Colors}}.
}
\keyword{internal}
\keyword{methods}
