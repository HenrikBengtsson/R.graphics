%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Device}
\docType{class}
\alias{Device}


 \title{The Device class}

 \usage{Device(filename=NA, restore.parameters=FALSE)}

 \arguments{
   \item{filename}{A filename where to save the resulting plot. The
      filename suffix decides what graphical device driver will used.
      By default the screen will be used.}
   \item{restore.parameters}{
      Some graphical device drivers might change the global graphical
      parameters (see \code{par()}). \code{dev.open()} can restore them
      automatically if this argument is \code{\link[base:logical]{TRUE}}. Default value is \code{\link[base:logical]{FALSE}}
      since querying for graphical parameters opens up an annoying empty
      window if no window is opened.}
 }

 \description{
  Package:  R.graphics \cr
\bold{Class Device}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{Device}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{Device}\cr
extends \link[R.oo]{Object}\cr



  A class for creating graphical devices, plotting to them etc.
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.graphics:bringToTop.Device]{bringToTop}} \tab Bring a device to top.\cr
 \tab \code{callFunction} \tab  -\cr
 \tab \code{\link[R.graphics:choose.Device]{choose}} \tab GUI figure selector.\cr
 \tab \code{\link[R.graphics:close.Device]{close}} \tab Closes the device.\cr
 \tab \code{\link[R.graphics:closeAll.Device]{closeAll}} \tab Closes all open devices.\cr
 \tab \code{constructor} \tab  -\cr
 \tab \code{finalize} \tab  -\cr
 \tab \code{\link[R.graphics:findPngDevice.Device]{findPngDevice}} \tab Searches for a working PNG device.\cr
 \tab \code{getAspectRatio} \tab  -\cr
 \tab \code{getDimensions} \tab  -\cr
 \tab \code{getMouseClick} \tab  -\cr
 \tab \code{\link[R.graphics:getNext.Device]{getNext}} \tab Opens the next device.\cr
 \tab \code{\link[R.graphics:getOptions.Device]{getOptions}} \tab Get the current default options for a given device type.\cr
 \tab \code{\link[R.graphics:getParameters.Device]{getParameters}} \tab Gets parameters specific to a certain type of device.\cr
 \tab \code{getPlotParameters} \tab  -\cr
 \tab \code{\link[R.graphics:getPrintPath.Device]{getPrintPath}} \tab Gets the current path where figures are saved.\cr
 \tab \code{\link[R.graphics:isOpen.Device]{isOpen}} \tab Checks if a device is open or not.\cr
 \tab \code{\link[R.graphics:new.Device]{new}} \tab Creates a new device.\cr
 \tab \code{\link[R.graphics:open.Device]{open}} \tab Opens a device.\cr
 \tab \code{\link[R.graphics:plotRegions.Device]{plotRegions}} \tab Static method plotting information about the different regions of a device.\cr
 \tab \code{\link[R.graphics:plotSymbols.Device]{plotSymbols}} \tab Display available plot symbols.\cr
 \tab \code{\link[R.graphics:print.Device]{print}} \tab Prints device to file.\cr
 \tab \code{putAuthor} \tab  -\cr
 \tab \code{putDataset} \tab  -\cr
 \tab \code{putTimestamp} \tab  -\cr
 \tab \code{\link[R.graphics:resetOptions.Device]{resetOptions}} \tab Reset the current default options for a given device type.\cr
 \tab \code{\link[R.graphics:set.Device]{set}} \tab Sets the current device index.\cr
 \tab \code{\link[R.graphics:setOptions.Device]{setOptions}} \tab Set the current default options for a given device type.\cr
 \tab \code{setPlotParameters} \tab  -\cr
 \tab \code{\link[R.graphics:setPrintPath.Device]{setPrintPath}} \tab Sets the current path where figures are to be saved.\cr
 \tab \code{\link[R.graphics:setStyle.Device]{setStyle}} \tab Sets plot style for the current device.\cr
 \tab \code{\link[R.graphics:subplots.Device]{subplots}} \tab Creates a grid of subplots.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \value{
  The constructor returns a Device object.
 }

 \details{
  To set eps/ps or xfig options use .Postscript.Options
   (see \code{help(.Postscript.Options)}).
  To set bitmap arguments (bmp, jpg/jpeg, png and gif) use
   \code{\link{.Bitmap.Options}}.
  To set default pictex arguments (tex/pictex) use
   \code{\link{.PicTeX.Options}}.

  Supported devices are
  \code{screen}, \emph{eps, ps, bmp, jpg/jpeg, png, tex/pictex, xfig} and
  \emph{gif}. The gif device requires that ImageMagick
  (\url{http://www.imagemagick.org/}) is installed on the system path.
  To test if it is installed try \code{system("convert")}.
 }

 \examples{\dontrun{
   dev <- Device("myplot.eps")
   plot(1,1)
   dev$close()
 
   # and similar...
   plot(1,1)
   Device$print("myplot.eps")
 }}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{Internally the \code{bmp},
   \code{jpeg}, \code{\link[grDevices]{pictex}},
   \code{\link[grDevices]{png}}, \code{\link[grDevices]{postscript}},
   \code{\link[grDevices]{xfig}} in the \pkg{grDevices} package,
   and the \code{\link{gif}}() in this package, and \code{\link{eps}}().
 }

 \keyword{device}
\keyword{classes}
