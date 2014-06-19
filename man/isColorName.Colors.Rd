%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Colors.builtin.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{isColorName.Colors}
\alias{isColorName.Colors}
\alias{Colors.isColorName}
\alias{isColorName.Colors}
\alias{isColorName,Colors-method}


\title{Checks if a color name is known or not}

\synopsis{isColorName.Colors(this, name, ...)}
\usage{Colors$isColorName(name, ...)}

\description{
  Checks if a color name is known or not.
}

\arguments{
  \item{name}{A \code{\link[base]{vector}} a color names as \code{\link[base]{character}} strings.}
  \item{...}{Not used.}
}

\value{
 Returns a boolean \code{\link[base]{vector}} of length \code{length(name)} specifying for
 each color name if it is a known name or not.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  Colors$isColorName("red")              # TRUE
  Colors$isColorName(c("blue", "gren"))  # c(TRUE, FALSE)
}

\keyword{color}

\seealso{
  \code{\link[R.graphics:getColorNames.Colors]{*getColorNames}()}.
  \code{\link[grDevices]{colors}}.
  For more information see \code{\link{Colors}}.
}
\keyword{internal}
\keyword{methods}
