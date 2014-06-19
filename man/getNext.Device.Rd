%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getNext.Device}
\alias{getNext.Device}
\alias{Device.getNext}
\alias{getNext.Device}
\alias{getNext,Device-method}


 \title{Opens the next device}

 \description{
  Opens the next device. If no such device exists, the a new device is opened.
 }

 \synopsis{getNext.Device(this, index=dev.cur(), device=x11, ...)}
\usage{Device$getNext(index=dev.cur(), device=x11, ...)}

 \arguments{
   \item{index}{The index of the \emph{current device}.}
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
