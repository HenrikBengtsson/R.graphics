setMethodS3("rescale1D", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ...) {
  # If x.range is not given, calculate x.range for each column of data,
  # else make sure it has three columns.
  if (is.null(x.range)) {
    ok <- !is.na(x) & !is.infinite(x);
    x.range <- range(x[ok]);
  }
  if (is.null(dim.range))
    dim.range <- c(0,1);

  # For each column, threshhold everything outside the range and then
  # normalize x into [0,1] using "width" of range.
  min <- x.range[1];
  max <- x.range[2];
  x[x < min] <- min;
  x[x > max] <- max;
  x <- (x-min) / (max-min);

  # Map into dimension ranges according to dim.range.
  range <- dim.range[2]-dim.range[1];
  offset <- dim.range[1];
  x*range + offset;
}, protected=TRUE, static=TRUE)



#########################################################################/**
# @set "class=Colors"
# @RdocMethod getGray
#
# @title "Creates gray scales from data"
#
# @synopsis
#
# \description{
#   Generated grayscales from a vector of values specified by argument
#   \code{x}. The interval the values can span can be specified by the
#   argument \code{x.range}. Everything outside the range is safely
#   thresholded. If \code{x.range} is @NULL, the range of \code{x} 
#   will be used. The interval of the grayscale generated can be specified
#   by the argument \code{dim.range}. By default this is the full grayscale 
#   range, i.e. [0,1].
#
#   The returned colors has the same dimensions as argument \code{x}.
# }
#
# \arguments{
#   \item{x}{A matrix of size N, where N is the number of data values.}
#   \item{x.range}{A vector of length two.}
#   \item{dim.range}{A vector of length two.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  rows (or the length) of \code{x}. The resulting vector contains character
#  strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
# }
#
# @author
#
# @examples "../incl/Colors.getXXXColors.Rex"
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getHSV", 
#   @seemethod "getRGB".
#   @see "grDevices::gray".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getGray", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(GrayColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)



setMethodS3("getRed", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(RedColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)


setMethodS3("getGreen", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(GreenColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)


setMethodS3("getBlue", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(BlueColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)




#########################################################################/**
# @RdocMethod getHeatColors
#
# @title "Creates heat colors from data"
#
# @synopsis
#
# \description{
#   Generated heat colors from a vector of values specified by argument
#   \code{x}. The interval the values can span can be specified by the
#   argument \code{x.range}. Everything outside the range is safely
#   thresholded. If \code{x.range} is @NULL, the range of \code{x} 
#   will be used. The interval of the grayscale generated can be specified
#   by the argument \code{dim.range}. By default this is the full grayscale 
#   range, i.e. [0,1].
#
#   The returned colors has the same dimensions as argument \code{x}.
# }
#
# \arguments{
#   \item{x}{A matrix of size N, where N is the number of data values.}
#   \item{x.range}{A vector of length two.}
#   \item{dim.range}{A vector of length two.}
#   \item{ncolors}{Maximum number of unique colors to generate.
#    Default value is \code{256}.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  rows (or the length) of \code{x}. The resulting vector contains character
#  strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
# }
#
# @author
#
# \examples{\dontrun{See help(Color.getGray) for an example.}}
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getGray", 
#   @seemethod "getHSV", 
#   @seemethod "getRGB".
#   @see "grDevices::palette".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getHeatColors", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ncolors=256, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(HeatColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)





#########################################################################/**
# @RdocMethod getRainbowColors
#
# @title "Creates rainbow colors from data"
#
# @synopsis
#
# \description{
#   Generated rainbow colors from a vector of values specified by argument
#   \code{x}. The interval the values can span can be specified by the
#   argument \code{x.range}. Everything outside the range is safely
#   thresholded. If \code{x.range} is @NULL, the range of \code{x} 
#   will be used. The interval of the grayscale generated can be specified
#   by the argument \code{dim.range}. By default this is the full grayscale 
#   range, i.e. [0,1].
#
#   The returned colors has the same dimensions as argument \code{x}.
# }
#
# \arguments{
#   \item{x}{A matrix of size N, where N is the number of data values.}
#   \item{x.range}{A vector of length two.}
#   \item{dim.range}{A vector of length two.}
#   \item{ncolors}{Maximum number of unique colors to generate.
#    Default value is \code{256}.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  rows (or the length) of \code{x}. The resulting vector contains character
#  strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
# }
#
# @author
#
# \examples{\dontrun{See help(Color.getGray) for an example.}}
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getGray", 
#   @seemethod "getHSV", 
#   @seemethod "getRGB".
#   @see "grDevices::palette".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getRainbowColors", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ncolors=256, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(RainbowColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)




#########################################################################/**
# @RdocMethod getTopoColors
#
# @title "Creates topo colors from data"
#
# @synopsis
#
# \description{
#   Generated topo colors from a vector of values specified by argument
#   \code{x}. The interval the values can span can be specified by the
#   argument \code{x.range}. Everything outside the range is safely
#   thresholded. If \code{x.range} is @NULL, the range of \code{x} 
#   will be used. The interval of the grayscale generated can be specified
#   by the argument \code{dim.range}. By default this is the full grayscale 
#   range, i.e. [0,1].
#
#   The returned colors has the same dimensions as argument \code{x}.
# }
#
# \arguments{
#   \item{x}{A matrix of size N, where N is the number of data values.}
#   \item{x.range}{A vector of length two.}
#   \item{dim.range}{A vector of length two.}
#   \item{ncolors}{Maximum number of unique colors to generate.
#    Default value is \code{256}.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  rows (or the length) of \code{x}. The resulting vector contains character
#  strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
# }
#
# @author
#
# \examples{\dontrun{See help(Color.getGray) for an example.}}
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getGray", 
#   @seemethod "getHSV", 
#   @seemethod "getRGB".
#   @see "grDevices::palette".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getTopoColors", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ncolors=256, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(TopologyColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)



#########################################################################/**
# @RdocMethod getTerrainColors
#
# @title "Creates terrain colors from data"
#
# @synopsis
#
# \description{
#   Generated terrain colors from a vector of values specified by argument
#   \code{x}. The interval the values can span can be specified by the
#   argument \code{x.range}. Everything outside the range is safely
#   thresholded. If \code{x.range} is @NULL, the range of \code{x} 
#   will be used. The interval of the grayscale generated can be specified
#   by the argument \code{dim.range}. By default this is the full grayscale 
#   range, i.e. [0,1].
#
#   The returned colors has the same dimensions as argument \code{x}.
# }
#
# \arguments{
#   \item{x}{A matrix of size N, where N is the number of data values.}
#   \item{x.range}{A vector of length two.}
#   \item{dim.range}{A vector of length two.}
#   \item{ncolors}{Maximum number of unique colors to generate.
#    Default value is \code{256}.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  rows (or the length) of \code{x}. The resulting vector contains character
#  strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
# }
#
# @author
#
# \examples{\dontrun{See help(Color.getGray) for an example.}}
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getGray", 
#   @seemethod "getHSV", 
#   @seemethod "getRGB".
#   @see "grDevices::palette".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getTerrainColors", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ncolors=256, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(TerrainColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)





#########################################################################/**
# @RdocMethod getCyanMagentaColors
#
# @title "Creates Cleveland-style cyan-magenta colors from data"
#
# @synopsis
#
# \description{
#   Generated Cleveland-style cyan-magenta colors from a vector of values
#   specified by argument
#   \code{x}. The interval the values can span can be specified by the
#   argument \code{x.range}. Everything outside the range is safely
#   thresholded. If \code{x.range} is @NULL, the range of \code{x} 
#   will be used. The interval of the grayscale generated can be specified
#   by the argument \code{dim.range}. By default this is the full grayscale 
#   range, i.e. [0,1].
#
#   The returned colors has the same dimensions as argument \code{x}.
# }
#
# \arguments{
#   \item{x}{A matrix of size N, where N is the number of data values.}
#   \item{x.range}{A vector of length two.}
#   \item{dim.range}{A vector of length two.}
#   \item{ncolors}{Maximum number of unique colors to generate.
#    Default value is \code{256}.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  rows (or the length) of \code{x}. The resulting vector contains character
#  strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
# }
#
# @author
#
# \examples{\dontrun{See help(Color.getGray) for an example.}}
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getGray", 
#   @seemethod "getHSV", 
#   @seemethod "getRGB".
#   @see "grDevices::palette".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getCyanMagentaColors", "Colors", function(this, x, x.range=NULL, dim.range=NULL, ncolors=256, ...) {
  dim <- dim(x);
  x <- Colors$rescale1D(x, x.range=x.range, dim.range=dim.range);
  x <- getColors(CyanMagentaColor(x));
  dim(x) <- dim;
  x;
}, static=TRUE)





#########################################################################/**
# @RdocMethod getRgbFromWavelength
#
# @title "Create RGB colors from data"
#
# @synopsis
#
# \description{
#   Because the concept of colors is all about spectroscopy, the biology and
#   physics of the human eye and the brain's perception of the neurological
#   signals, there is no unique one-to-one mapping between wavelength and 
#   (R,G,B) values.
# }
#
# \arguments{
#   \item{wavelength}{Vector of wavelengths to be converted into 
#     (R,G,B) values.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns a @matrix with the three rows R, G and B corresponding to the 
#   red, green and blue intensities.
# }
#
# @author
#
# \examples{\dontrun{# See help(WavelengthColor) in the R.classes package}}
#
# \seealso{
#   @seeclass
# }
#
# \references{
#   Dan Bruton, Color Science, 
#     \url{http://www.physics.sfasu.edu/astro/color.html}, 2002.\cr
#
#   efg's Computer Lab, 
#     \url{http://www.efg2.com/Lab/ScienceAndEngineering/Spectra.htm}, 2002.
# }
#*/#########################################################################
setMethodS3("getRgbFromWavelength", "Colors", function(static, wavelength, maxValue=255, ...) {
  if (maxValue != 255)
    warning("Argument 'maxValue' was ignored.");  
  getColors(WavelengthColor(wavelength));
}, static=TRUE)  # getRgbFromWavelength()





############################################################################
# HISTORY:
# 2002-12-27
# o Added getRgbFromWavelength().
# 2002-10-09
# o Added toGrayScale() and toMonochrome().
# 2002-08-23
# * getRGBorHSV(), which is used by getRGB() and getHSV(), now return NA's
#   where its corresponding input values are NA's.
# 2002-05-31
# * Added invert(), rgbToColors() and getMonochrome().
# 2002-05-11
# * BUG FIX: getHeatColors(), getTopoColors(), getTerrainColors(),
#   getCyanMagentaColors() and getRainbowColors() always returned the last
#   value as NA. This was due to an internal out-of-range array index.
# 2002-05-07
# * Added getRed(), getGreen() and getBlue() because they are simplier than
#   using getRGB() if one just want red, green or blue.
# 2002-04-21
# * getGray() now supports NA values by setting the corresponding color to
#   NA also. In previous versions gray() would generate an error if one
#   tried to generate colors where some values where NA.
# * BUG FIX: Forgot to exclude Inf's and NA's from x.range in rescale1D().
# 2002-04-03
# * Added colorToRGB().
# * Added getHeatColors(), getTopoColors(), getTerrainColors(),
#   getCyanMagentaColors(), getRainbowColors().
# 2002-03-30
# * Replaced demo() with example code.
# * Recoded with setMethodS3's etc.
# 2001-08-09
# * BUG FIX: getRGBorHSV() sometimes generated x == 0, which would give
#   an error. Added x[is.na(x)] <- 0 to solve it.
# 2001-07-02
# * getRGBorHSV() supports both upper and lower case dim argument.
# 2001-06-23
# * Added some Rdoc comments with examples.
# 2001-05-14
# * Added getInternalReferences() for improving gco() performance.
# 2001-05-04
# * Now supports formal attributes.
# 2001-04-11
# * Created from old ColorFactory, GreenColorFactory etc.
# 2001-04-03
# * Created from old DotStyler.
############################################################################
