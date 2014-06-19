%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.builtin.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{rgbToColors.Colors}
\alias{rgbToColors.Colors}
\alias{Colors.rgbToColors}
\alias{rgbToColors.Colors}
\alias{rgbToColors,Colors-method}


\title{Converts RGB values to colors}

\synopsis{rgbToColors.Colors(this, col, ...)}
\usage{Colors$rgbToColors(col, ...)}

\description{
 Convert any (R,G,B) values in to color string of form \code{"#rrggbb"}.
}

\arguments{
  \item{x}{(integer) matrix with three rows; red, green and blue.}
  \item{...}{Not used.}
}

\value{
 Returns (hexadecimal) color strings.
}

\details{
  This method is just a wrapper for \code{rgb2col()} in the R.base
  package.
}

\examples{
  rgb <- Colors$colorsToRGB(c(6, "blue", "#3366ff"))
  print(rgb)

  #        [,1] [,2] [,3]
  #  red    255    0   51
  #  green    0    0  102
  #  blue   255  255  255

  col <- Colors$rgbToColors(rgb)
  print(col)
  # [1] "#FF00FF" "#0000FF" "#3366FF"
}

\keyword{color}

\seealso{
  \code{\link[R.graphics:colorsToRGB.Colors]{*colorsToRGB}()}.
  \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}.
  For more information see \code{\link{Colors}}.
}
\keyword{internal}
\keyword{methods}
