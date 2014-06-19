%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{closeAll.Device}
\alias{closeAll.Device}
\alias{Device.closeAll}
\alias{closeAll.Device}
\alias{closeAll,Device-method}


 \title{Closes all open devices}

 \description{
  Static method that closes all open devices.
 }

 \synopsis{closeAll.Device(this, ...)}
\usage{Device$closeAll(...)}

 \arguments{
   \item{...}{Not used.}
 }

 \value{Returns the number of closed devices.}

 \examples{\dontrun{
   Device("myplot.eps")
   plot(1,1)
   Device("myplot.jpg")
   plot(1,1)
   Device$closeAll()
 }}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[grDevices:dev]{graphics.off}}.

   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
