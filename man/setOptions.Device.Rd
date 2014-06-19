%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setOptions.Device}
\alias{setOptions.Device}
\alias{Device.setOptions}
\alias{setOptions.Device}
\alias{setOptions,Device-method}


 \title{Set the current default options for a given device type}

 \description{
  Set the current default options for a given device type.

  Sets options such as \code{width}, \code{height} etc for a given
  device type.
 }

 \synopsis{setOptions.Device(static, deviceType=c("bitmap", "pictex", "postscript"), ...)}
\usage{Device$setOptions(deviceType=c("bitmap", "pictex", "postscript"), ...)}

 \arguments{
  \item{deviceType}{A \code{\link[base]{character}} string.}
  \item{...}{Name arguments of format \code{<name>=<value>}.}
 }

 \value{Returns nothing.}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[R.graphics:getOptions.Device]{*getOptions}()} and \code{\link[R.graphics:resetOptions.Device]{*resetOptions}()}.
 }

 \keyword{dplot}
\keyword{internal}
\keyword{methods}
