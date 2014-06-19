#########################################################################/**
# @RdocObject .Bitmap.Options
#
# @title "Data structure for default arguments to bitmap devices"
# 
# \description{
#  Structure for default values for the arguments to the bitmap
#  graphical devices (bmp, jpg/jpeg, png and gif), \emph{when} 
#  \code{dev.open()} is used. Note that the bmp, jpg, png and gif devices 
#  do \emph{not} use this, only \code{dev.open()}. Similar to 
#  \code{.PostScript.Options} in the [R] base package.
# }
#
# \section{Elements}{
#  \code{.Bitmap.Options:}
#  \itemize{
#   \item{width}{The default width (in pixels) of the graphical region.
#                  The default is 480 pixels.}
#   \item{height}{The default height (in pixels) of the graphical region.
#                   The default is 480 pixels.}
#   \item{pointsize}{The default point size (in pt) of text.
#                      The default is 12 points.}
#   \item{quality}{The default compression ratio (in percentage).
#                    The default is 75\%.
#                    Currently only used by the \code{jpg()} device.}
#  }
# }
#
# @author
#
# \seealso{
#   @see ".PicTeX.Options", \code{.PostScript.Options} 
#   (see @see "grDevices::postscript").
# }
#
# \keyword{dplot}
#*/#########################################################################
.Bitmap.Options <- list(width=480,height=480,pointsize=12,quality=75);






#########################################################################/**
# @RdocObject .PicTeX.Options
#
# @title "Data structure for default arguments to the pictex device"
# 
# \description{
#  Structure for default values for the arguments to the pictex
#  graphical device, \emph{when} \code{dev.open()} is used. Note that the 
#  pictex device itself does \emph{not} use this, only \code{dev.open()}.
# }
#
# \section{Elements}{
#  \code{.PicTeX.Options:}
#  \itemize{
#   \item{width}{The default width (in inches) of the graphical region.
#                  The default is 5 pixels.}
#   \item{height}{The default height (in inches) of the graphical region.
#                   The default is 4 pixels.}
#   \item{debug}{The default if debugging information should be printed 
#                  or not. The default is @FALSE.}
#   \item{bg}{The default background color of the plot.
#               The default color is white.}
#   \item{fg}{The default foregound color of the plot.
#               The default color is black.}
#  }
# }
#
# @author
#
# \seealso{
#   @see ".Bitmap.Options", \code{.PostScript.Options} 
#   (see @see "grDevices::postscript").
# }
#
# \keyword{dplot}
#*/#########################################################################
# Set default value for .PicTeX.Options
.PicTeX.Options <- list(width=5,height=4,debug=FALSE,bg="white",fg="black");


# Set the default width of a PS/EPS image to 5 inches. To support "50%".
# ps.options(width=5.0);         # From R v2.0.0 this must be in .First.lib.

