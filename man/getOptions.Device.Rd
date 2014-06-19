%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getOptions.Device}
\alias{getOptions.Device}
\alias{Device.getOptions}
\alias{getOptions.Device}
\alias{getOptions,Device-method}


 \title{Get the current default options for a given device type}

 \description{
  Get the current default options for a given device type.

  Retrieves options such as \code{width}, \code{height} etc for
  bitmap devices (bmp, jpg/jpeg, png and gif),
  pixtex devices (tex, pictex),
  and postscript devices (ps, eps), cf. \code{\link[grDevices]{ps.options}}.
 }

 \synopsis{getOptions.Device(static, deviceType=c("bitmap", "pictex", "postscript"), names=NULL, ...)}
\usage{Device$getOptions(deviceType=c("bitmap", "pictex", "postscript"), names=NULL, ...)}

 \arguments{
   \item{deviceType}{A \code{\link[base]{character}} string.}
   \item{names}{A \code{\link[base]{vector}} of names of options to be retrieved.
              If \code{\link[base]{NULL}}, all options are returned.}
   \item{...}{Not used.}
 }

 \value{Returns nothing.}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[R.graphics:setOptions.Device]{*setOptions}()} and \code{\link[R.graphics:resetOptions.Device]{*resetOptions}()}.
   \code{\link[grDevices]{ps.options}}.
 }

 \keyword{dplot}
\keyword{internal}
\keyword{methods}
