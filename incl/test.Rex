flip.Matrix <- function(this, MARGIN=1) {
  flip <- this;
  if (MARGIN == 1) {
    for (k in 1:ncol(this))
      flip[,k] <- rev(this[,k]);
  } else if (MARGIN == 2) {
    for (k in 1:nrow(this))
      flip[k,] <- rev(this[k,]);
  }
  flip;
}

if (!exists("flip.default")) {
  if (exists("flip") && is.function(flip))
    flip.default <- flip
  else
    flip.default <- function() stop("Function not defined!");
}
flip <- function(...) UseMethod("flip");


rotate.Matrix <- function(this, angle=0) {
  if (diff(dim(this)) != 0)
    stop("Only square matrices can be rotated.");

  if ( (angle %% 90) != 0 )
    stop("Only rotations of 90*n degrees, where n is an integer, is supported.");

  angle <- angle %/% 90;
  angle <- angle %% 4;

  if (angle == 1)
    return(flip(t(this)))
  else if (angle == 2)
    return(flip(t(flip(t(this)))))
  else if (angle == 3)
    return(t(flip(this)));

  this;
}

if (!exists("rotate.default")) {
  if (exists("rotate") && is.function(rotate))
    rotate.default <- rotate
  else
    rotate.default <- function() stop("Function not defined!");
}
rotate <- function(...) UseMethod("rotate");


