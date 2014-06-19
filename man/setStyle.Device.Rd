%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setStyle.Device}
\alias{setStyle.Device}
\alias{Device.setStyle}
\alias{setStyle.Device}
\alias{setStyle,Device-method}


\title{Sets plot style for the current device}

\description{
 Sets plot style for the current device from a set of predefined plot styles.
 Available styles are \code{default} and \code{PowerPoint}.
 The style \code{default} is the default parameter settings used by [R]
 and the style \code{PowerPoint} is a style for generating plots to be
 used in the presentations, which requires fatter lines, larges fonts etc.
}

\synopsis{setStyle.Device(this, style=c("default", "PowerPoint"), ...)}
\usage{Device$setStyle(style=c("default", "PowerPoint"), ...)}

\arguments{
  \item{style}{The name of the predefined plot style to be applied.}
  \item{...}{Not used.}
}

\value{Returns nothing.}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  Device$setStyle("PowerPoint")
  plot(sin(0:1000))
  Device$setStyle()            # Reset to default [R] settings.
}

\seealso{
  \code{\link[graphics]{par}}

  For more information see \code{\link{Device}}.
}
\keyword{internal}
\keyword{methods}
