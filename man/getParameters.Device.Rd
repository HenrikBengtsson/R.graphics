%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getParameters.Device}
\alias{getParameters.Device}
\alias{Device.getParameters}
\alias{getParameters.Device}
\alias{getParameters,Device-method}


 \title{Gets parameters specific to a certain type of device}

 \description{
  Gets parameters specific to a certain type of device.
  This method is used by \code{\link[R.graphics:print.Device]{*print}()} and \code{\link[R.graphics:open.Device]{*open}()}.
 }

 \synopsis{getParameters.Device(this, filename=this$filename, path=getPrintPath(this), width=NULL, height=NULL, aspectRatio=NULL, pointsize=NULL, quality=NULL, bg=NULL, fg=NULL, debug=NULL, ...)}
\usage{Device$getParameters(filename=this$filename, path=getPrintPath(this), width=NULL, height=NULL, aspectRatio=NULL, pointsize=NULL, quality=NULL, bg=NULL, fg=NULL, debug=NULL, ...)}

 \arguments{
   \item{filename}{A filename where to save the resulting plot. The
      filename suffix decides what graphical device driver will used.}
   \item{path}{The output path where the figure is saved.}
   \item{width, height}{The width and height of the image. If both are
      \code{\link[base]{NULL}} the graphical device driver's default settings are used.
      If one of them are missing, the other one are set to the default
      value according to the default settings and the missing argument
      is assumed to be of size \code{"100\%"}. If any of the two are given
      as a percentage string, e.g. \code{"75\%"}, its value is calculated
      to of that percentage of the other value, which must be absolute.}
   \item{aspectRatio}{If \code{\link[base:logical]{TRUE}}, the height will be defined by the
      aspect ratio of the current device. If \code{\link[base]{numeric}}, height will be
      the width times the aspect ratio. Otherwise aspectRatio is ignored.}
   \item{pointsize}{The font pointsize to be used. If \code{\link[base]{NULL}} the
      graphical device driver's default settings are used.}
   \item{quality}{The quality in [0,100]\% if applicable. This argument is
      currently only used by the jpeg device driver. If \code{\link[base]{NULL}} the
      graphical device driver's default settings are used.}
   \item{bg, fg}{Background and foreground colors used by the PicTeX
      driver. If \code{\link[base]{NULL}} the graphical device driver's default settings
      are used.}
   \item{debug}{Used by the PicTeX driver. If \code{\link[base]{NULL}} the graphical
      device driver's default settings are used.}
   \item{...}{Not used.}
 }

 \value{
   Returns a named \code{\link[base]{list}} structure with elements
    \code{pathname}, \code{imgName}, \code{imgType},
    \code{height}, \code{width}, \code{aspectRatio},
    \code{quality}, \code{pointsize},
    \code{bg}, \code{fg}, and \code{debug}.

    Value \code{pathname} is the pathname to the image file validate to
    writable.
    Value \code{imgName} is the basename of the pathname without the
    filename extension, which is returned (in lower case) by \code{imgType}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
    \code{\link[R.graphics:print.Device]{*print}()} and \code{\link[R.graphics:open.Device]{*open}()}.
   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
