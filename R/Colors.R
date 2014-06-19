########################################################################/**
# @RdocClass Colors 
#
# @title "The Colors class"
#
# \description{
#
#  \emph{This class is obsolete. Use the Color classes in the R.colors
#        package instead.}
#
#  @classhierarchy
#
#  A static class providing several methods for producing colors.
# }
#
# \section{Fields and Methods}{
#  @allmethods
#
# }
#
# @examples "../incl/Colors.Rex"
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @see "grDevices::colors".
# }
#*/#########################################################################
setConstructorS3("Colors", function(nbrOfColors=16) {
  extend(Object(), "Colors",
    RED.HUE    = 0,
    YELLOW.HUE = 1/6,
    GREEN.HUE  = 2/6,
    CYAN.HUE   = 3/6,
    BLUE.HUE   = 4/6,
    PURPLE.HUE = 5/6,
    nbrOfColors = nbrOfColors
  )
}, static=TRUE)





setMethodS3("getRGBorHSV", "Colors", function(this, x, x.range=NULL, dim, dim.range=NULL, what, ...) {
  dim <- tolower(dim);

  # Translate "r", "g", "b" into dimension 1, 2 and 3.
  if (what == "rgb") {
    dim[dim == "r"] <- 1;
    dim[dim == "g"] <- 2;
    dim[dim == "b"] <- 3;
  } else {
    dim[dim == "h"] <- 1;
    dim[dim == "s"] <- 2;
    dim[dim == "v"] <- 3;
  }
  dim <- as.integer(dim);

  for (k in 1:3) {
    if (length(dim[dim == k]) > 1)
      warning("Same color dimension specified more than once.");
  }

  # Making x an matrix and make sure it has three columns by padding with
  # zeros.
  x <- as.matrix(x);
  if (ncol(x) < 3) {
    x.new <- matrix(0, ncol=3, nrow=nrow(x));
    for (k in 1:ncol(x))
      x.new[,dim[k]] <- x[,k];
    x <- x.new;
  }

  # If x.range is not given, calculate x.range for each column of data,
  # else make sure it has three columns.
  if (is.null(x.range)) {
    x.range <- apply(x, MARGIN=2, FUN=range, na.rm=TRUE);
  } else {
    x.range <- matrix(x.range, nrow=2);
    if (ncol(x.range) < 3) {
      x.range.new <- matrix(0, ncol=3, nrow=2);
      for (k in 1:ncol(x.range)) {
        if (!is.na(dim[k]))
          x.range.new[,dim[k]] <- x.range[,k];
      }
      x.range <- x.range.new;
    }
  }
  if (is.null(dim.range)) {
    dim.range <- matrix(c(0,1), ncol=3, nrow=2);
  } else {
    dim.range <- matrix(dim.range, nrow=2);
    if (ncol(dim.range) < 3) {
      dim.range.new <- matrix(c(0,1), ncol=3, nrow=2);
      for (k in 1:ncol(dim.range)) {
        if (!is.na(dim[k]))
          dim.range.new[,dim[k]] <- dim.range[,k];
      }
      dim.range <- dim.range.new;
    }
  }

  if (what == "rgb")
    x.default <- c(0,0,0)
  else
    x.default <- c(0,1,1);

  nas <- is.na(x);
  
  # For each column, threshhold everything outside the range and then
  # normalize x into [0,1] using "width" of range.
  for (k in 1:3) {
    # Normalize x into [0,1]
    xcol <- x[,k];
    min <- x.range[1,k];
    max <- x.range[2,k];
    if (min != max) {
      xcol[xcol < min] <- min;
      xcol[xcol > max] <- max;
      xcol <- (xcol-min) / (max-min);
      x[,k] <- xcol;
    } else
      x[,k] <- x.default[k];

    # Map into dimension ranges according to dim.range.
    range <- dim.range[2,k]-dim.range[1,k];
    offset <- dim.range[1,k];
    x[,k] <- x[,k]*range + offset;
  }
  
  if (what == "rgb") {
    color <- RgbColor(red=x[,1], green=x[,2], blue=x[,3]);
  } else {
    color <- HsvgColor(hue=x[,1], saturation=x[,2], value=x[,3]);
  }

  getColors(color);
}, private=TRUE, static=TRUE)



#########################################################################/**
# @RdocMethod getRGB
#
# @title "Create RGB colors from data"
#
# @synopsis
#
# \description{
#   Generated colors in RGB units using either a one, two or three columns
#   matrix of values specified by argument \code{x}. Each row in the matrix
#   corresponds to one color and each column to either R (red), G (green) or
#   B (blue). Which column that corresponds to which channel can be 
#   controlled by the argument \code{dim}. The intervals the values in each
#   column span can be specified by the argument \code{x.range}. Everything
#   outside the range is safely thresholded. If \code{x.range} is
#   @NULL, the range of each column in \code{x} will be used.
#   The interval of each channel the input values should map into can be
#   specified by the argument \code{dim.range}. By default this is the full
#   range of each channel, i.e. [0,1].
# }
#
# \arguments{
#   \item{x}{A matrix of size Nx1, Nx2 or Nx3, where N is the number of
#            data values.}
#   \item{x.range}{A matrix of size 2x1, 2x2 or 2x3.}
#   \item{dim}{A vector specifying what channel each columns controls.}
#   \item{dim.range}{A matrix of size 2x1, 2x2 or 2x3.}
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
# @examples "../incl/Colors.getRGB.Rex"
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getHSV", 
#   @seemethod "getGray".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getRGB", "Colors", function(this, x, x.range=NULL, 
                                dim=c("r", "g", "b"), dim.range=NULL, ...) {
  Colors$getRGBorHSV(x, x.range=x.range, dim=dim, dim.range=dim.range,
                                                                what="rgb");
}, static=TRUE)






#########################################################################/**
# @RdocMethod getHSV
#
# @title "Create HSV colors from data"
#
# @synopsis
#
# \description{
#   Generated colors in HSV units using either a one, two or three columns
#   matrix of values specified by argument \code{x}. Each row in the matrix
#   corresponds to one color and each column to either H (hue), 
#   S (saturation) or V (value). Which column that corresponds to which 
#   channel can be controlled by the argument \code{dim}. The intervals the
#   values in each column span can be specified by the argument
#   \code{x.range}. Everything outside the range is safely thresholded. If
#   \code{x.range} is @NULL, the range of each column in \code{x} will
#   be used. The interval of each channel the input values should map into 
#   can be specified by the argument \code{dim.range}. By default this is 
#   the full range of each channel, i.e. [0,1].
# }
#
# \arguments{
#   \item{x}{A matrix of size Nx1, Nx2 or Nx3, where N is the number of
#            data values.}
#   \item{x.range}{A matrix of size 2x1, 2x2 or 2x3.}
#   \item{dim}{A vector specifying what channel each columns controls.}
#   \item{dim.range}{A matrix of size 2x1, 2x2 or 2x3.}
#   \item{...}{Not used.}
# }
#
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  rows (or the length) of \code{x}. The resulting vector contains character
#  strings of format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
# }
#
# @author
#
# @examples "../incl/Colors.getHSV.Rex"
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getRGB", 
#   @seemethod "getGray".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getHSV", "Colors", function(this, x, x.range=NULL, 
                                dim=c("h", "s", "v"), dim.range=NULL, ...) {
  Colors$getRGBorHSV(x, x.range=x.range, dim=dim, dim.range=dim.range, 
                                                               what="hsv");
}, static=TRUE)



setMethodS3("invert", "Colors", function(this, col, ...) {
  rgb <- RgbColor(col);
  filter <- NegativeColorFilter();
  getColors(filter, rgb);
}, static=TRUE)




setMethodS3("getMonochrome", "Colors", function(this, col, threshold=128, ...) {
  rgb <- RgbColor(col);
  filter <- MonochromeColorFilter(threshold=threshold/256, coefficients=c(1,1,1));
  getColors(filter, rgb);
}, static=TRUE)



setMethodS3("toGrayScale", "Colors", function(this, color, coefs = c(0.3, 0.59, 0.11), ...) {
  rgb <- RgbColor(col);
  filter <- GrayColorFilter(coefficients=coefs);
  getColors(filter, rgb);
}, static=TRUE);



setMethodS3("toMonochrome", "Colors", function(this, color, threshold=0.5, monocolors=c("#FFFFFF", "#000000"), coefs = c(0.3, 0.59, 0.11), ...) {
  rgb <- RgbColor(color);
  filter <- MonochromeColorFilter(threshold=threshold, coefficients=coefs, monocolors=monocolors);
  getColors(filter, rgb);
}, static=TRUE);


############################################################################
# HISTORY:
# 2004-10-22
# o getRGBorHSV() in Colors generated "NAs are not allowed in subscripted 
#   assignments" error in R v2.0.0. Corrected.
# 2003-11-21
# o Made obsolete. The R.colors package is much better.
# o The following methods now all make use of Color and ColorFilter classes:
#   getMonochrome, getRGBorHSV(), invert(), toGrayScale(), toMonochrome()
#   By doing this we can (i) phase out this class and (ii) at the same time
#   test the R.colors pacakge.
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
