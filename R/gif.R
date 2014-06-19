#########################################################################/**
# @RdocFunction gif
# 
# @title "GIF graphics device"
# 
# \description{
#  Device driver for GIF images. 
#  Internally \code{png()} and ImageMagick's convert command is used.
# }
# 
# @synopsis
#
# \arguments{
#  \item{filename}{filename (@character string).}
#  \item{width}{width (in pixels) of the saved image (@integer).}
#  \item{height}{height (in pixels) of the saved image (@integer).}
#  \item{pointsize}{font size (@integer).}
# }
# 
# \value{A plot device is opened; nothing is returned to the \R interpreter.}
# 
# \note{
# Requires: ImageMagick  (\url{http://www.imagemagick.org}) has to be 
# installed on the system. To test whether it is installed or not, try 
# \code{system("convert")}.
# }
#
# \examples{\dontrun{
#  options(imagemagick=c(convert="c:/Program Files/ImageMagick/convert"))
#  logo <- BitmapImage$read("logosm.ppm", path = R.graphics$dataPath)
#  gif(filename="logosm.gif")
#  image(logo)
#  dev.off()
# }}
#
# \keyword{device}
#
# @author
#*/#########################################################################
gif <- function(filename="Rplot.gif", width=480, height=480, pointsize=12) {
  base.pos <- which(search() == "package:base");
  pos      <- which(search() == "package:R.graphics");

  # Just in case something went wrong last time, remove any overridden
  # dev.off().
  if (exists("dev.off", where=pos, inherits=FALSE))
    rm("dev.off", pos=pos);

  # Create the png file.
  tmpfile <- tempfile();
  png.filename <- paste(tmpfile,".png",sep="");
  png(png.filename, width, height, pointsize);

  # Call my dev.off() instead (for now).
  assign("dev.off", function() {
    # Call the original dev.off() in the base package.
    fcn <- get("dev.off", mode="function", pos=base.pos);
    rm("dev.off", pos=pos);
    fcn();

    # Get the path to ImageMagick's convert command.
    convert <- getOption("imagemagick")["convert"];
    if (is.null(convert))
      convert <- "convert";
 
    if (regexpr("^[^\"].* .*", convert) != -1)
      convert <- paste("\"", convert, "\"", sep="");

    # Call 'convert' to convert from temporary png to temporary gif.
    gif.filename <- paste(tmpfile, ".gif", sep="");
    cmd <- paste(convert, png.filename, gif.filename, sep=" ");
    if (.Platform$OS.type == "windows")
      system(cmd, invisible=TRUE)
    else
      system(cmd);

    # Renaming the temporary gif file to the wanted filename
    file.rename(gif.filename, filename);

    # Remove the temporary png file.
    file.remove(png.filename);

    invisible(filename);
  }, pos=pos);
} # gif()




############################################################################
# HISTORY:
# 2001-10-29
# o Updated the gif() command to 1) not create temporary variables in 
#   .GlobalEnv. It now also convert to a tempory gif file and first then
#   rename that to the wanted filename. This means that if the filename
#   does not have a *.gif extension a gif file will still be generated.
#   Finally, the option "imagemagick" contains named field where one of 
#   them can be "convert" for specifying the path to the convert command.
#   Note that on WinXP Pro there is already a convert command in the path.
# 2001-04-11
# * Created from old com.braju.graphics and made into a static class.
# 2001-03-08 [version 0.2.1]
# * Added close.screen argument to dev.close().
# 2001-03-07 [version 0.2]
# * Added restore.par argument to dev.open().
# * Added dev.close().
# 2001-02-28 [version 0.1]
# * Added Rdoc comments.
# 2001-02-12
# * Added quotaion marks around filenames in 'convert' system call.
# 2001-02-08
# * Created the gif device driver. Removed old eps2gif(); no need for it!
# * Added support for gif outputs.
# 2001-02-07
# * Created.
############################################################################

