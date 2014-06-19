#########################################################################/**
# @set "class=Colors"
# @RdocMethod getColorNames
#
# @title "Returns the built-in color names"
#
# @synopsis
#
# \description{
#   Returns the built-in color names. Returns the same as 
#   @see "grDevices::colors".
# }
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character @vector of known color names.
# }
#
# @author
#
# \examples{
#   Colors$getColorNames()
# }
#
# \seealso{
#   @seemethod "isColorName".
#   @see "grDevices::colors".
#   @seeclass
# }
#
# \keyword{color}
#*/#########################################################################
setMethodS3("getColorNames", "Colors", function(this, ...) {
  colors();
}, static=TRUE)




#########################################################################/**
# @RdocMethod isColorName
#
# @title "Checks if a color name is known or not"
#
# @synopsis
#
# \description{
#   Checks if a color name is known or not.
# }
#
# \arguments{
#   \item{name}{A @vector a color names as @character strings.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a boolean @vector of length \code{length(name)} specifying for
#  each color name if it is a known name or not.
# }
#
# @author
#
# \examples{
#   Colors$isColorName("red")              # TRUE
#   Colors$isColorName(c("blue", "gren"))  # c(TRUE, FALSE)
# }
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getColorNames".
#   @see "grDevices::colors".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("isColorName", "Colors", function(this, name, ...) {
  is.element(name, colors());
}, static=TRUE)




#########################################################################/**
# @RdocMethod colorsToRGB
#
# @title "Converts colors to RGB values"
#
# @synopsis
#
# \description{
#  Convert any [R] color to RGB (red, green, blue) values. Accepted color
#  values are 1) a color name (an element of \code{colors()}), 2) a
#  a hexadecimal string of the form \code{"#rrggbb"}, or 3) an integer
#  \code{i} meaning \code{palette()[i]}.
# }
#
# \arguments{
#   \item{col}{vector of any of the three kind of [R] colors described
#   in the description section.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns an @integer @matrix with the three rows \code{red}, \code{green},
#  and \code{blue}. The columns contains the RGB value for each color.
# }
#
# \details{
#   This method is just a wrapper for \code{col2rgb()} in the [R] base
#   package and included to collect color functions at one place.
# }
#
# \examples{
#   Colors$colorsToRGB(c(6, "blue", "#3366ff"))
#
#   #        [,1] [,2] [,3]
#   #  red    255    0   51
#   #  green    0    0  102
#   #  blue   255  255  255
# }
#
# \keyword{color}
#
# \seealso{
#   @seemethod "rgbToColors".
#   @see "grDevices::col2rgb", @see "grDevices::rgb".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("colorsToRGB", "Colors", function(this, col, ...) {
  col2rgb(col);
}, static=TRUE)




#########################################################################/**
# @RdocMethod rgbToColors
#
# @title "Converts RGB values to colors"
#
# @synopsis
#
# \description{
#  Convert any (R,G,B) values in to color string of form \code{"#rrggbb"}.
# }
#
# \arguments{
#   \item{x}{(integer) matrix with three rows; red, green and blue.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns (hexadecimal) color strings.
# }
#
# \details{
#   This method is just a wrapper for \code{rgb2col()} in the R.base
#   package.
# }
#
# \examples{
#   rgb <- Colors$colorsToRGB(c(6, "blue", "#3366ff"))
#   print(rgb)
#
#   #        [,1] [,2] [,3]
#   #  red    255    0   51
#   #  green    0    0  102
#   #  blue   255  255  255
#
#   col <- Colors$rgbToColors(rgb)
#   print(col)
#   # [1] "#FF00FF" "#0000FF" "#3366FF"
# }
#
# \keyword{color}
#
# \seealso{
#   @seemethod "colorsToRGB".
#   @see "grDevices::col2rgb", @see "grDevices::rgb".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("rgbToColors", "Colors", function(this, col, ...) {
  rgb2col(col);
}, static=TRUE)




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
