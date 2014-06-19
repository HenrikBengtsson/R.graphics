%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  X.Options.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{.Bitmap.Options}
\alias{.Bitmap.Options}

 \title{Data structure for default arguments to bitmap devices}

 \description{
  Structure for default values for the arguments to the bitmap
  graphical devices (bmp, jpg/jpeg, png and gif), \emph{when}
  \code{dev.open()} is used. Note that the bmp, jpg, png and gif devices
  do \emph{not} use this, only \code{dev.open()}. Similar to
  \code{.PostScript.Options} in the [R] base package.
 }

 \section{Elements}{
  \code{.Bitmap.Options:}
  \itemize{
   \item{width}{The default width (in pixels) of the graphical region.
                  The default is 480 pixels.}
   \item{height}{The default height (in pixels) of the graphical region.
                   The default is 480 pixels.}
   \item{pointsize}{The default point size (in pt) of text.
                      The default is 12 points.}
   \item{quality}{The default compression ratio (in percentage).
                    The default is 75\%.
                    Currently only used by the \code{jpg()} device.}
  }
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link{.PicTeX.Options}}, \code{.PostScript.Options}
   (see \code{\link[grDevices]{postscript}}).
 }

 \keyword{dplot}

