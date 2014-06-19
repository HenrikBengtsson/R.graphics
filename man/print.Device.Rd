%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{print.Device}
\alias{print.Device}
\alias{Device.print}
\alias{print.Device}
\alias{print,Device-method}


 \title{Prints device to file}

 \description{
  Static method for direct printing of the current plot to a device,
  where the device type is infered from the filename.
 }

 \synopsis{print.Device(x, ...)}
\usage{Device$print(...)}

 \arguments{
   \item{...}{Arguments passed to \code{\link[R.graphics:getParameters.Device]{*getParameters}()}.}
 }

 \value{Returns nothing.}

 \examples{
   plot(1,1)
   \dontrun{Device$print("myplot.jpg")}
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[R.graphics:getParameters.Device]{*getParameters}()}.
   Internally \code{\link[grDevices]{dev.print}} is used.
   Default values of dimensions etc. are specified by
   \code{\link{.Bitmap.Options}}, \code{\link{.PicTeX.Options}} and
   \code{.PostScript.Options} (see \code{\link[grDevices]{postscript}}).
   \code{\link[R.graphics:open.Device]{*open}()}.

   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
