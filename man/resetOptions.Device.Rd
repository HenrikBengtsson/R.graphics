%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{resetOptions.Device}
\alias{resetOptions.Device}
\alias{Device.resetOptions}
\alias{resetOptions.Device}
\alias{resetOptions,Device-method}


 \title{Reset the current default options for a given device type}

 \description{
  Reset the current default options for a given device type.

  Resets the options for a given device type,
  e.g. bitmap, pictex and postscript.
 }

 \synopsis{resetOptions.Device(static, deviceType=c("bitmap", "pictex", "postscript"), ...)}
\usage{Device$resetOptions(deviceType=c("bitmap", "pictex", "postscript"), ...)}

 \arguments{
  \item{deviceType}{A \code{\link[base]{character}} string.}
  \item{...}{Not used.}
 }

 \value{Returns nothing.}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[R.graphics:getOptions.Device]{*getOptions}()} and \code{\link[R.graphics:setOptions.Device]{*setOptions}()}.
 }

 \keyword{dplot}
\keyword{internal}
\keyword{methods}
