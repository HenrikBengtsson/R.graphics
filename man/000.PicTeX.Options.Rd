%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  X.Options.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{.PicTeX.Options}
\alias{.PicTeX.Options}

 \title{Data structure for default arguments to the pictex device}

 \description{
  Structure for default values for the arguments to the pictex
  graphical device, \emph{when} \code{dev.open()} is used. Note that the
  pictex device itself does \emph{not} use this, only \code{dev.open()}.
 }

 \section{Elements}{
  \code{.PicTeX.Options:}
  \itemize{
   \item{width}{The default width (in inches) of the graphical region.
                  The default is 5 pixels.}
   \item{height}{The default height (in inches) of the graphical region.
                   The default is 4 pixels.}
   \item{debug}{The default if debugging information should be printed
                  or not. The default is \code{\link[base:logical]{FALSE}}.}
   \item{bg}{The default background color of the plot.
               The default color is white.}
   \item{fg}{The default foregound color of the plot.
               The default color is black.}
  }
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link{.Bitmap.Options}}, \code{.PostScript.Options}
   (see \code{\link[grDevices]{postscript}}).
 }

 \keyword{dplot}

