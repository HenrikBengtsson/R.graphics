%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{close.Device}
\alias{close.Device}
\alias{Device.close}
\alias{close.Device}
\alias{close,Device-method}


 \title{Closes the device}

 \description{
  Closes the device.
 }

 \usage{\method{close}{Device}(con, ...)}

 \arguments{
   \item{...}{Not used.}
 }

 \value{Returns nothing.}

 \examples{\dontrun{
   dev <- Device("myplot.eps")
   plot(1,1)
   dev$close()
 }}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[grDevices:dev]{dev.off}}.

   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
