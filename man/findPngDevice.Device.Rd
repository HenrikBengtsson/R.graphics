%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{findPngDevice.Device}
\alias{findPngDevice.Device}
\alias{Device.findPngDevice}
\alias{findPngDevice.Device}
\alias{findPngDevice,Device-method}


 \title{Searches for a working PNG device}

 \description{
  Searches for a working PNG device.

  On Unix, the png device requires that X11 is available, which it is not
  when running batch scripts or running \R remotely.

  This method tries to create a simple png file first using the
  \code{png()} device, and if that is not working it tries the
  \code{bitmap()} device, which does not require X11 etc, because it
  utilizes Ghostscript.
 }

 \synopsis{findPngDevice.Device(static, ...)}
\usage{Device$findPngDevice(...)}

 \arguments{
   \item{...}{Not used.}
 }

 \value{
  Returns the \code{png} \code{\link[base]{function}}, the \code{bitmap} \code{\link[base]{function}}, or \code{\link[base]{NULL}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \examples{
   fcn <- Device$findPngDevice();
   if (identical(fcn, png)) {
     cat("PNG device found: png()");
   } else if (identical(fcn, bitmap)) {
     cat("PNG device found: bitmap()");
   } else {
     cat("PNG device not found.");
   }
 }

 \seealso{
   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
