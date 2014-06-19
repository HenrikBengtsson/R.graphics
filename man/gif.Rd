%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  gif.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{gif}
\alias{gif}


 \title{GIF graphics device}

 \description{
  Device driver for GIF images.
  Internally \code{png()} and ImageMagick's convert command is used.
 }

 \usage{gif(filename="Rplot.gif", width=480, height=480, pointsize=12)}

 \arguments{
  \item{filename}{filename (\code{\link[base]{character}} string).}
  \item{width}{width (in pixels) of the saved image (\code{\link[base]{integer}}).}
  \item{height}{height (in pixels) of the saved image (\code{\link[base]{integer}}).}
  \item{pointsize}{font size (\code{\link[base]{integer}}).}
 }

 \value{A plot device is opened; nothing is returned to the \R interpreter.}

 \note{
 Requires: ImageMagick  (\url{http://www.imagemagick.org}) has to be
 installed on the system. To test whether it is installed or not, try
 \code{system("convert")}.
 }

 \examples{\dontrun{
  options(imagemagick=c(convert="c:/Program Files/ImageMagick/convert"))
  logo <- BitmapImage$read("logosm.ppm", path = R.graphics$dataPath)
  gif(filename="logosm.gif")
  image(logo)
  dev.off()
 }}

 \keyword{device}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

