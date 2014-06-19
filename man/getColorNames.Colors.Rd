%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.builtin.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{getColorNames.Colors}
\alias{getColorNames.Colors}
\alias{Colors.getColorNames}
\alias{getColorNames.Colors}
\alias{getColorNames,Colors-method}


\title{Returns the built-in color names}

\synopsis{getColorNames.Colors(this, ...)}
\usage{Colors$getColorNames(...)}

\description{
  Returns the built-in color names. Returns the same as
  \code{\link[grDevices]{colors}}.
}

\arguments{
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} \code{\link[base]{vector}} of known color names.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  Colors$getColorNames()
}

\seealso{
  \code{\link[R.graphics:isColorName.Colors]{*isColorName}()}.
  \code{\link[grDevices]{colors}}.
  For more information see \code{\link{Colors}}.
}

\keyword{color}
\keyword{internal}
\keyword{methods}
