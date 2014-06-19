%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{plotSymbols.Device}
\alias{plotSymbols.Device}
\alias{Device.plotSymbols}
\alias{plotSymbols.Device}
\alias{plotSymbols,Device-method}


\title{Display available plot symbols}

\description{
 Generates a plot with all available point styles (0-255) that
 can be specified by the argument \code{pch} in almost all plot functions
 in [R]. \code{pch} can be a \code{\link[base]{vector}} of \code{\link[base]{integer}}s specifying a symbol and
 \code{\link[base]{character}}s.}

\synopsis{plotSymbols.Device(this, interactive=TRUE, ...)}
\usage{Device$plotSymbols(interactive=TRUE, ...)}

\arguments{
  \item{interactive}{If \code{\link[base:logical]{TRUE}} the user is expected to click on symbols
    whose \code{\link[base]{character}} value, ASCII value as an \code{\link[base]{integer}}, an octal decimal and
    as an hexadecimal.}
  \item{...}{Not used.}
}

\value{Returns nothing. Generates a plot.}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  Device$plotSymbols()
}

\seealso{
  \code{\link[grDevices]{plotmath}},
  \code{\link[graphics]{par}}

  For more information see \code{\link{Device}}.
}
\keyword{internal}
\keyword{methods}
