%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.1D.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getRgbFromWavelength.Colors}
\alias{getRgbFromWavelength.Colors}
\alias{Colors.getRgbFromWavelength}
\alias{getRgbFromWavelength.Colors}
\alias{getRgbFromWavelength,Colors-method}


\title{Create RGB colors from data}

\synopsis{getRgbFromWavelength.Colors(static, wavelength, maxValue=255, ...)}
\usage{Colors$getRgbFromWavelength(wavelength, maxValue=255, ...)}

\description{
  Because the concept of colors is all about spectroscopy, the biology and
  physics of the human eye and the brain's perception of the neurological
  signals, there is no unique one-to-one mapping between wavelength and
  (R,G,B) values.
}

\arguments{
  \item{wavelength}{Vector of wavelengths to be converted into
    (R,G,B) values.}
  \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{matrix}} with the three rows R, G and B corresponding to the
  red, green and blue intensities.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{\dontrun{# See help(WavelengthColor) in the R.classes package}}

\seealso{
  For more information see \code{\link{Colors}}.
}

\references{
  Dan Bruton, Color Science,
    \url{http://www.physics.sfasu.edu/astro/color.html}, 2002.\cr

  efg's Computer Lab,
    \url{http://www.efg2.com/Lab/ScienceAndEngineering/Spectra.htm}, 2002.
}
\keyword{internal}
\keyword{methods}
