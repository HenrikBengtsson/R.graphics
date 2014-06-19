%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{set.Device}
\alias{set.Device}
\alias{Device.set}
\alias{set.Device}
\alias{set,Device-method}


 \title{Sets the current device index}

 \description{
  Static method that sets the current device index. If there exists no
  device with that index a device with that index will be created. This
  might result in several intermediate device will have to be opened, but
  they will be closed before returned.
  Similar to Matlab's \code{figure} function.
 }

 \synopsis{set.Device(this, index, device=x11, width=NULL, height=NULL, ...)}
\usage{Device$set(index, device=x11, width=NULL, height=NULL, ...)}

 \arguments{
   \item{index}{The index to which the current device will be set to.}
   \item{device}{Specifies the device driver devices that might be opened.}
   \item{...}{Not used.}
 }

 \value{Returns the number of opened devices.}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[grDevices:dev]{dev.set}}.

   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
