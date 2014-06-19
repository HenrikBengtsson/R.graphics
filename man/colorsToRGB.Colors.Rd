%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.builtin.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{colorsToRGB.Colors}
\alias{colorsToRGB.Colors}
\alias{Colors.colorsToRGB}
\alias{colorsToRGB.Colors}
\alias{colorsToRGB,Colors-method}


\title{Converts colors to RGB values}

\synopsis{colorsToRGB.Colors(this, col, ...)}
\usage{Colors$colorsToRGB(col, ...)}

\description{
 Convert any [R] color to RGB (red, green, blue) values. Accepted color
 values are 1) a color name (an element of \code{colors()}), 2) a
 a hexadecimal string of the form \code{"#rrggbb"}, or 3) an integer
 \code{i} meaning \code{palette()[i]}.
}

\arguments{
  \item{col}{vector of any of the three kind of [R] colors described
  in the description section.}
  \item{...}{Not used.}
}

\value{
 Returns an \code{\link[base]{integer}} \code{\link[base]{matrix}} with the three rows \code{red}, \code{green},
 and \code{blue}. The columns contains the RGB value for each color.
}

\details{
  This method is just a wrapper for \code{col2rgb()} in the [R] base
  package and included to collect color functions at one place.
}

\examples{
  Colors$colorsToRGB(c(6, "blue", "#3366ff"))

  #        [,1] [,2] [,3]
  #  red    255    0   51
  #  green    0    0  102
  #  blue   255  255  255
}

\keyword{color}

\seealso{
  \code{\link[R.graphics:rgbToColors.Colors]{*rgbToColors}()}.
  \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}.
  For more information see \code{\link{Colors}}.
}
\keyword{internal}
\keyword{methods}
