%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getRGB.Colors}
\alias{getRGB.Colors}
\alias{Colors.getRGB}
\alias{getRGB.Colors}
\alias{getRGB,Colors-method}


\title{Create RGB colors from data}

\synopsis{getRGB.Colors(this, x, x.range=NULL, dim=c("r", "g", "b"), dim.range=NULL, ...)}
\usage{Colors$getRGB(x, x.range=NULL, dim=c("r", "g", "b"), dim.range=NULL, ...)}

\description{
  Generated colors in RGB units using either a one, two or three columns
  matrix of values specified by argument \code{x}. Each row in the matrix
  corresponds to one color and each column to either R (red), G (green) or
  B (blue). Which column that corresponds to which channel can be
  controlled by the argument \code{dim}. The intervals the values in each
  column span can be specified by the argument \code{x.range}. Everything
  outside the range is safely thresholded. If \code{x.range} is
  \code{\link[base]{NULL}}, the range of each column in \code{x} will be used.
  The interval of each channel the input values should map into can be
  specified by the argument \code{dim.range}. By default this is the full
  range of each channel, i.e. [0,1].
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
r <- 1:5;
g <- 1:5;
b <- 1:5;

x <- 1:5; y <- rep(1,5);
plot(NA, xlim=c(1,5), ylim=c(1,5), xlab="", ylab="");

# Generate full-range red colors
colors <- Colors$getRGB(r);
# "#000000" "#400000" "#800000" "#BF0000" "#FF0000"
points(x,1*y, col=colors, pch=19, cex=5);

colors <- Colors$getRGB(matrix(c(r,g), ncol=2));
# "#000000" "#404000" "#808000" "#BFBF00" "#FFFF00"
points(x,2*y, col=colors, pch=19, cex=5);

# Threshold the "g" channel to the interval [2,4].
data <- matrix(c(r,b,g), ncol=3);
data.range <- matrix(c(range(r), range(b), 2,4), ncol=2);
colors <- Colors$getRGB(data, data.range, dim=c("r","b","g"));
# "#000000" "#404000" "#808080" "#BFBFFF" "#FFFFFF"
points(x,3*y, col=colors, pch=19, cex=5);

# Generate light red colors
colors <- Colors$getRGB(r, dim.range=c(0.5,1));
# "#800000" "#9F0000" "#BF0000" "#DF0000" "#FF0000"
points(x,4*y, col=colors, pch=19, cex=5);

# Generate dark red colors
colors <- Colors$getRGB(r, dim.range=c(0.2, 0.7));
# "#330000" "#530000" "#730000" "#930000" "#B30000"
points(x,5*y, col=colors, pch=19, cex=5);
}

\keyword{color}

\seealso{
  \code{\link[R.graphics:getHSV.Colors]{*getHSV}()},
  \code{\link[R.graphics:getGray.Colors]{*getGray}()}.
  For more information see \code{\link{Colors}}.
}
\keyword{internal}
\keyword{methods}
