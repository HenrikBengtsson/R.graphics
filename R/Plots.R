#########################################################################/**
# @RdocClass Plots
#
# @title "The Plots class"
#
# \description{
#  @classhierarchy
#
#  A static class providing several methods for producing and setting 
#  default parameters for plots.
# }
#
# \section{Fields and Methods}{
#  @allmethods
#
# }
#
# @author
#
# \seealso{
#   @see "base::par".
# }
#*/#########################################################################
setConstructorS3("Plots", function() {
  extend(Object(), "Plots"
  )
}, deprecated=TRUE)




setMethodS3("getAbundantClosePoints", "Plots", function(this, points, nclass=5, density=2/3, threshold=NULL, ...) {
  dense.points <- Plots$getClosePoints(points, nclass=nclass, 
                                                threshold=threshold);
  len <- length(dense.points);
  rnd <- runif(len*(1-density), 1, len);
  dense.points[rnd];
}, protected=TRUE, static=TRUE);


#########################################################################/**
# @RdocMethod getClosePoints
#
# @title "Indentifies data points that are \"close\""
#
# \description{
#   Indentifies data points that are "close". This could be used to exclude
#   high density areas in scatter plots etc where the points are "on top
#   of each other". This becomes a speed/memory issue plots that are
#   printed as vector graphics, e.g. eps.
# }
#
# @synopsis
#
# \arguments{
#   \item{points}{-}
#   \item{nclass}{-}
#   \item{threshold}{-}
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
#
# @author
#*/#########################################################################
setMethodS3("getClosePoints", "Plots", function(this, points, nclass=5, threshold=NULL, ...) {
  points <- as.matrix(points);
  len  <- nrow(points);
  ndim <- ncol(points);

  int <- 1:len;

  nclass <- rep(nclass, length.out=ndim);

  if (is.null(threshold))
    threshold <- (len/nclass)/3
  else
    threshold  <- rep(threshold, length.out=ndim);

  close.points <- int;
  for(k in 1:ndim) {
    x <- points[,k];
    # Normalize to [0,1] in each dimension.
    x <- x-min(x, na.rm=TRUE);
    x <- x/max(x, na.rm=TRUE);

    # Make a histogram along each dimension.
    h <- hist(x, nclass=nclass[k], plot=FALSE);
    dense.bins <- (1:nclass[k])[h$counts > threshold[k]];

    if (length(dense.bins) == 0)
      return(c());

    close <- 1/(2*nclass[k]);
    dense.points <- c();
    for (dense.bin in dense.bins) {
      bin.points <- int[abs(x-h$mids[dense.bin]) < close];
      dense.points <- c(dense.points, bin.points);
    }
    close.points <- intersect(close.points, dense.points);
  }

  close.points;
}, protected=TRUE, static=TRUE);


setMethodS3("subplots", "Plots", function(static, ...) {
  Device$subplots(...);
}, static=TRUE, deprecated=TRUE);

setMethodS3("setStyle", "Plots", function(static, ...) {
  Device$setStyle(...);
}, static=TRUE, deprecated=TRUE);


setMethodS3("plotSymbols", "Plots", function(static, ...) {
  Device$plotSymbols();
}, static=TRUE, deprecated=TRUE);


############################################################################
# HISTORY:
# 2002-10-31
# o Move plotSymbols() to the Device class and made it deprecated for this.
# o Same for setStyle() and subplots().
# 2002-04-07
# * Updated plotSymbols() to produce nicer numbering on the axis.
# 2002-01-25
# * Added subplots.default().
# 2002-01-24
# * Rewritten to make use of setMethodS3.
# 2002-01-18
# * Update subplots.Plots(n) to "interpret a vector n as length(n)".
# * Made subplots.default(...) call subplots.Plots(...).
# 2001-09-28
# * Bug fix: Default .Bitmap.Options$width and $height was strings!
# * Added the style "html". Currently the same as powerpoint.
# 2001-07-02
# * Updated the Rdoc comments.
# 2001-06-23
# * Added some Rdoc comments.
# 2001-05-14
# * Added getInternalReferences() for improving gco() performance.
# 2001-05-04
# * Now supports formal attributes.
# 2001-04-29
# * Added seeAlso().
# 2001-04-02
# * Created!
############################################################################
