%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getHSV.Colors}
\alias{getHSV.Colors}
\alias{Colors.getHSV}
\alias{getHSV.Colors}
\alias{getHSV,Colors-method}


\title{Create HSV colors from data}

\synopsis{getHSV.Colors(this, x, x.range=NULL, dim=c("h", "s", "v"), dim.range=NULL, ...)}
\usage{Colors$getHSV(x, x.range=NULL, dim=c("h", "s", "v"), dim.range=NULL, ...)}

\description{
  Generated colors in HSV units using either a one, two or three columns
  matrix of values specified by argument \code{x}. Each row in the matrix
  corresponds to one color and each column to either H (hue),
  S (saturation) or V (value). Which column that corresponds to which
  channel can be controlled by the argument \code{dim}. The intervals the
  values in each column span can be specified by the argument
  \code{x.range}. Everything outside the range is safely thresholded. If
  \code{x.range} is \code{\link[base]{NULL}}, the range of each column in \code{x} will
  be used. The interval of each channel the input values should map into
  can be specified by the argument \code{dim.range}. By default this is
  the full range of each channel, i.e. [0,1].
}

\arguments{
  \item{x}{A matrix of size Nx1, Nx2 or Nx3, where N is the number of
           data values.}
  \item{x.range}{A matrix of size 2x1, 2x2 or 2x3.}
  \item{dim}{A vector specifying what channel each columns controls.}
  \item{dim.range}{A matrix of size 2x1, 2x2 or 2x3.}
  \item{...}{Not used.}
}


\value{
 Returns a \code{\link[base]{vector}} of \code{\link[base]{character}} strings of length equal to the number of
 rows (or the length) of \code{x}. The resulting vector contains character
 strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
h <- 1:8;
s <- 1:8;
v <- 1:8;

x <- 1:8; y <- rep(1,8);
plot(NA, xlim=c(1,8), ylim=c(1,4), xlab="", ylab="");

# Generate colors in the full hue range
colors <- Colors$getHSV(h);
# [1] "#FFFFFF" "#FFFADB" "#CBFFB6" "#92FFD0"
# [4] "#6DC1FF" "#7D49FF" "#FF24E0" "#FF0000"
points(x,1*y, col=colors, pch=19, cex=5);

# Generate colors in the full saturation range with hue=0 (default)
colors <- Colors$getHSV(s, dim="s");
# [1] "#FFFFFF" "#FFDBDB" "#FFB6B6" "#FF9292"
# [4] "#FF6D6D" "#FF4949" "#FF2424" "#FF0000"
points(x,2*y, col=colors, pch=19, cex=5);

# Generate colors in the full value range with hue=0 (default)
colors <- Colors$getHSV(v, dim="v");
# [1] "#000000" "#240000" "#490000" "#6D0000"
# [4] "#920000" "#B60000" "#DB0000" "#FF0000"
points(x,3*y, col=colors, pch=19, cex=5);

# Generates red to green colors in the saturation range [0.3,0.8].
data <- matrix(1:8, nrow=8, ncol=2);
dim.range <- matrix(c(Colors$RED.HUE,Colors$GREEN.HUE, 0.3,0.8), nrow=2);
colors <- Colors$getHSV(data, dim.range=dim.range);
# [1] "#FFB3B3" "#FFBBA0" "#FFCF8E" "#FFEC7C"
# [4] "#EAFF6A" "#B7FF57" "#7AFF45" "#33FF33"
points(x,4*y, col=colors, pch=19, cex=5);
}

\keyword{color}

\seealso{
  \code{\link[R.graphics:getRGB.Colors]{*getRGB}()},
  \code{\link[R.graphics:getGray.Colors]{*getGray}()}.
  For more information see \code{\link{Colors}}.
}
\keyword{internal}
\keyword{methods}
