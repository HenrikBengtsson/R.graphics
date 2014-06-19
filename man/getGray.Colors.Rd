%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.1D.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{getGray.Colors}
\alias{getGray.Colors}
\alias{Colors.getGray}
\alias{getGray.Colors}
\alias{getGray,Colors-method}


\title{Creates gray scales from data}

\synopsis{getGray.Colors(this, x, x.range=NULL, dim.range=NULL, ...)}
\usage{Colors$getGray(x, x.range=NULL, dim.range=NULL, ...)}

\description{
  Generated grayscales from a vector of values specified by argument
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
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{vector}} of \code{\link[base]{character}} strings of length equal to the number of
 rows (or the length) of \code{x}. The resulting vector contains character
 strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
data(volcano)
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

phi=40
theta=40

opar <- par(mar=c(2,1,2,1)+0.1)
layout(matrix(1:6, nrow=2, ncol=3))
persp(x,y,z, col=Colors$getGray(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getGray()")
persp(x,y,z, col=Colors$getTopoColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getTopoColors()")
persp(x,y,z, col=Colors$getTerrainColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getTerrainColors()")
persp(x,y,z, col=Colors$getHeatColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getHeatColors()")
persp(x,y,z, col=Colors$getRainbowColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getRainbowColors()")
persp(x,y,z, col=Colors$getCyanMagentaColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getCyanMagentaColors()")

par(opar)
}

\keyword{color}

\seealso{
  \code{\link[R.graphics:getHSV.Colors]{*getHSV}()},
  \code{\link[R.graphics:getRGB.Colors]{*getRGB}()}.
  \code{\link[grDevices]{gray}}.
  For more information see \code{\link{Colors}}.
}
\keyword{internal}
\keyword{methods}
