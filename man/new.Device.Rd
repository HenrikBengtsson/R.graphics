%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{new.Device}
\alias{new.Device}
\alias{Device.new}
\alias{new.Device}
\alias{new,Device-method}


 \title{Creates a new device}

 \description{
  Static method that creates a new device.
 }

 \synopsis{new.Device(this, device=x11, ...)}
\usage{Device$new(device=x11, ...)}

 \arguments{
   \item{device}{Specifies the device driver devices that might be opened.}
   \item{...}{Not used.}
 }

 \value{Returns the id number of the opened device.}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[grDevices:dev]{dev.set}}.

   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
