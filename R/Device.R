#########################################################################/**
# @RdocClass Device 
#
# @title "The Device class"
# 
# @synopsis
#
# \arguments{
#   \item{filename}{A filename where to save the resulting plot. The
#      filename suffix decides what graphical device driver will used. 
#      By default the screen will be used.}
#   \item{restore.parameters}{
#      Some graphical device drivers might change the global graphical
#      parameters (see \code{par()}). \code{dev.open()} can restore them
#      automatically if this argument is @TRUE. Default value is @FALSE
#      since querying for graphical parameters opens up an annoying empty
#      window if no window is opened.}
# }
#
# \description{
#  @classhierarchy
#
#  A class for creating graphical devices, plotting to them etc.
# }
#
# \section{Fields and Methods}{
#  @allmethods
#
# }
#
# \value{
#  The constructor returns a Device object.
# }
# 
# \details{
#  To set eps/ps or xfig options use .Postscript.Options
#   (see \code{help(.Postscript.Options)}). 
#  To set bitmap arguments (bmp, jpg/jpeg, png and gif) use 
#   @see ".Bitmap.Options".
#  To set default pictex arguments (tex/pictex) use 
#   @see ".PicTeX.Options".
#
#  Supported devices are
#  \code{screen}, \emph{eps, ps, bmp, jpg/jpeg, png, tex/pictex, xfig} and 
#  \emph{gif}. The gif device requires that ImageMagick 
#  (\url{http://www.imagemagick.org/}) is installed on the system path.
#  To test if it is installed try \code{system("convert")}.
# }
#
# \examples{\dontrun{
#   dev <- Device("myplot.eps")
#   plot(1,1)
#   dev$close()
#   
#   # and similar...
#   plot(1,1)
#   Device$print("myplot.eps")
# }}
#
# @author
#
# \seealso{Internally the \code{bmp},
#   \code{jpeg}, @see "grDevices::pictex", 
#   @see "grDevices::png", @see "grDevices::postscript",
#   @see "grDevices::xfig" in the \pkg{grDevices} package,
#   and the @see "gif" in this package, and @see "eps".
# }
#
# \keyword{device}
#*/#########################################################################
setConstructorS3("Device", function(filename=NA, restore.parameters=FALSE) {
  this <- extend(Object(), "Device",
    filename = filename,
    printPath = NULL
  )
#  constructor.Device(this, filename, restore.parameters);
  this;
})




#########################################################################/**
# @RdocMethod getPrintPath
#
# @title "Gets the current path where figures are saved"
# 
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
#
# @author
#
# \seealso{
#   @seemethod "setPrintPath".
# }
#
# \keyword{dplot}
#*/#########################################################################
setMethodS3("getPrintPath", "Device", function(this, ...) {
  this$printPath;
})

#########################################################################/**
# @RdocMethod setPrintPath
#
# @title "Sets the current path where figures are to be saved"
# 
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{path}{Filename path where figures are saved.}
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
#
# @author
#
# \seealso{
#   @seemethod "setPrintPath".
# }
#
# \keyword{dplot}
#*/#########################################################################
setMethodS3("setPrintPath", "Device", function(this, path, ...) {
  this$printPath <- path;
})



setMethodS3("constructor", "Device", function(this, filename, restore.par=FALSE, ...) {
  if (is.na(filename))
    return(this);

  filename <- this$filename;
  if (is.null(filename) || filename == "screen") {
    x11();
    return(this);
  } else if (inherits(filename, "File")) {
    require(R.io) || throw("Package R.io was not found.");
    filename <- getAbsolutePath(filename);
  }

  # For everything except "screen" call a device driver.
  if (restore.par)
    par.use <- par(no.readonly=TRUE);

  require(R.basic) || throw("Package R.basic was not found.");
  baseext <- baseAndExtension(filename)  # From R.base
  filename.base <- baseext[1];
  filename.ext <- baseext[2];

  if (filename.ext == "eps") {
    postscript(filename, onefile=FALSE, horizontal=FALSE, paper="special");
  } else if (filename.ext == "ps") {
    postscript(filename, horizontal=FALSE);
  } else if (filename.ext == "xfig") {
    xfig(filename);
  } else if (filename.ext == "png") {
    opt <- getOptions(this, "bitmap");
    png(filename, width=opt$width, height=opt$height, pointsize=opt$pointsize);
  } else if (filename.ext == "gif") {
    opt <- getOptions(this, "bitmap");
    gif(filename, width=opt$width, height=opt$height, pointsize=opt$pointsize);
  } else if (filename.ext == "bmp") {
    opt <- getOptions(this, "bitmap");
    bmp(filename, width=opt$width, height=opt$height, pointsize=opt$pointsize);
  } else if (filename.ext == "jpeg" || filename.ext == "jpg") {
    opt <- getOptions(this, "bitmap");
    jpeg(filename, width=opt$width, height=opt$height, pointsize=opt$pointsize, quality=opt$quality);
  } else if (filename.ext == "tex" || filename.ext == "pictex") {
    opt <- getOptions(this, "pictex");
    pictex(filename, width=opt$width, height=opt$height, debug=opt$debug, bg=opt$bg, fg=opt$fg);
  }

  if (restore.par) {
    if (length(par.use) > 0) {
      warn.old <- options("warn")[[1]];
      options(warn=-1);
      par(par.use);
      options(warn=warn.old);
    }
  }

  this;
}, private=TRUE, createGeneric=FALSE);




#########################################################################/**
# @RdocMethod getOptions
#
# @title "Get the current default options for a given device type"
# 
# \description{
#  @get "title".
#
#  Retrieves options such as \code{width}, \code{height} etc for
#  bitmap devices (bmp, jpg/jpeg, png and gif),
#  pixtex devices (tex, pictex),
#  and postscript devices (ps, eps), cf. @see "grDevices::ps.options".
# }
#
# @synopsis
#
# \arguments{
#   \item{deviceType}{A @character string.}
#   \item{names}{A @vector of names of options to be retrieved.
#              If @NULL, all options are returned.}
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
#
# @author
#
# \seealso{
#   @seemethod "setOptions" and @seemethod "resetOptions".
#   @see "grDevices::ps.options".
# }
#
# \keyword{dplot}
#*/#########################################################################
setMethodS3("getOptions", "Device", function(static, deviceType=c("bitmap", "pictex", "postscript"), names=NULL, ...) {
  if (deviceType %in% c("postscript")) {
    opt <- ps.options();
  } else if (deviceType %in% c("bitmap")) {
    if (!exists(".Bitmap.Options", mode="list"))
      resetOptions(static, deviceType=deviceType);
    opt <- .Bitmap.Options;
  } else if (deviceType %in% c("pictex")) {
    if (!exists(".PicTeX.Options", mode="list"))
      resetOptions(static, deviceType=deviceType);
    opt <- .PicTeX.Options;
  } else {
    throw("Unknown device type: ", deviceType);
  }
  if (!is.null(names))
    opt <- opt[names];
  opt;
}, static=TRUE)




#########################################################################/**
# @RdocMethod setOptions
#
# @title "Set the current default options for a given device type"
# 
# \description{
#  @get "title".
#
#  Sets options such as \code{width}, \code{height} etc for a given
#  device type.
# }
#
# @synopsis
#
# \arguments{
#  \item{deviceType}{A @character string.}
#  \item{...}{Name arguments of format \code{<name>=<value>}.}
# }
#
# \value{Returns nothing.}
#
# @author
#
# \seealso{
#   @seemethod "getOptions" and @seemethod "resetOptions".
# }
#
# \keyword{dplot}
#*/#########################################################################
setMethodS3("setOptions", "Device", function(static, deviceType=c("bitmap", "pictex", "postscript"), ...) {
  args <- list(...);
  if (length(args) == 0)
    return(getOptions(static, deviceType=deviceType));
  
  if (deviceType %in% c("postscript")) {
    old <- ps.options(...);
  } else if (deviceType %in% c("bitmap")) {
    optionName <- ".Bitmap.Options";
    envir <- globalenv();
    if (!exists(optionName, mode="list", envir=envir))
      resetOptions(static, deviceType=deviceType);
    new <- old <- get(optionName, envir=envir);
    for (kk in seq(args)) {
      name <- names(args)[kk];
      new[name] <- args[kk];
    }
    assign(optionName, new, envir=envir);
  } else if (deviceType %in% c("pictex")) {
    optionName <- ".PicTeX.Options";
    envir <- globalenv();
    if (!exists(optionName, mode="list", envir=envir))
      resetOptions(static, deviceType=deviceType);
    new <- old <- get(optionName, envir=envir);
    for (kk in seq(args)) {
      name <- names(args)[kk];
      new[name] <- args[kk];
    }
    assign(optionName, new, envir=envir);
  } else {
    throw("Unknown device type: ", deviceType);
  }
  
  invisible(old);
}, static=TRUE)



#########################################################################/**
# @RdocMethod resetOptions
#
# @title "Reset the current default options for a given device type"
# 
# \description{
#  @get "title".
#
#  Resets the options for a given device type,
#  e.g. bitmap, pictex and postscript.
# }
#
# @synopsis
#
# \arguments{
#  \item{deviceType}{A @character string.}
#  \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
#
# @author
#
# \seealso{
#   @seemethod "getOptions" and @seemethod "setOptions".
# }
#
# \keyword{dplot}
#*/#########################################################################
setMethodS3("resetOptions", "Device", function(static, deviceType=c("bitmap", "pictex", "postscript"), ...) {
  if (deviceType %in% c("postscript")) {
    ps.options(reset=TRUE);
    Device$setOptions("postscript", width=5.0);
  } else if (deviceType %in% c("bitmap")) {
    optionName <- ".Bitmap.Options";
    envir <- globalenv();
    if (!exists(optionName, mode="list", envir=envir)) {
      assign(optionName, list(), envir=envir);
    }
    Device$setOptions("bitmap", width=480,height=480,pointsize=12,quality=75);
  } else if (deviceType %in% c("pictex")) {
    optionName <- ".PicTeX.Options";
    envir <- globalenv();
    if (!exists(optionName, mode="list", envir=envir)) {
      assign(optionName, list(), envir=envir);
    }
    Device$setOptions("pictex", width=5,height=4,debug=FALSE,bg="white",fg="black");
  } else {
    throw("Unknown device type: ", deviceType);
  }
}, static=TRUE)


#########################################################################/**
# @RdocMethod open
#
# @title "Opens a device"
# 
# \description{
#  Static method to open a device, where the type is infered from the
#  filename.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @seemethod "getParameters".}
# }
#
# \value{Returns nothing.}
# 
# @author
#
# \seealso{
#   @seemethod "getParameters".
#   @seemethod "print".
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("open", "Device", function(con, ...) {
  # To please R CMD check...
  this <- con;

  params <- getParameters(this, ...);
  Device$callFunction(function(dev, ...) dev(...), params=params);
}, static=TRUE);



#########################################################################/**
# @RdocMethod print
#
# @title "Prints device to file"
# 
# \description{
#  Static method for direct printing of the current plot to a device,
#  where the device type is infered from the filename.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @seemethod "getParameters".}
# }
#
# \value{Returns nothing.}
# 
# \examples{
#   plot(1,1)
#   \dontrun{Device$print("myplot.jpg")}
# }
#
# @author
#
# \seealso{
#   @seemethod "getParameters".
#   Internally @see "grDevices::dev.print" is used. 
#   Default values of dimensions etc. are specified by 
#   @see ".Bitmap.Options", @see ".PicTeX.Options" and
#   \code{.PostScript.Options} (see @see "grDevices::postscript").
#   @seemethod "open".
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("print", "Device", function(x, ...) {
  # To please R CMD check...
  this <- x;

  params <- getParameters(this, ...);
  callFunction(this, fcn=dev.print, params=params);
}, static=TRUE);



setMethodS3("callFunction", "Device", function(static, fcn, params, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validating arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!is.function(fcn))
    throw("Argument 'fcn' is not a function: ", mode(fcn));

  if (!is.list(params))
    throw("Argument 'params' is not a list: ", mode(list));
 

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Calling device function
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # To please R CMD check
  imgType <- pathname <- width <- height <- pointsize <- NULL;
  rm(imgType, pathname, width, height, pointsize);

  attachLocally(params);
  if (imgType == "eps") {
    fcn(postscript, pathname, onefile=FALSE, horizontal=FALSE, 
                              paper="special", width=width, height=height);
  } else if (imgType == "ps") {
    fcn(postscript, pathname, horizontal=FALSE, width=width, height=height)
  } else if (imgType == "xfig") {
      xfig <- get("xfig", mode="function")
      fcn(xfig, pathname)
  } else if (imgType == "png") {
      png <- get("png", mode="function")
      fcn(png, pathname, width=width, height=height, 
          pointsize=pointsize)
  } else if (imgType == "gif") {
      gif <- get("gif", mode="function")
      fcn(gif, pathname, width=width, height=height, pointsize=pointsize)
  } else if (imgType == "bmp") {
      bmp <- get("bmp", mode="function")
      fcn(bmp, pathname, width=width, height=height, 
          pointsize=pointsize)
  } else if (imgType == "jpeg" || imgType == "jpg") {
      if (is.null(quality)) {
        opt <- getOptions(static, "bitmap");
        quality <- opt$quality;
      }
      jpeg <- get("jpeg", mode="function")
      fcn(jpeg, pathname, width=width, height=height, 
          pointsize=pointsize, quality=quality)
  } else if (imgType == "tex" || imgType == "pictex") {
      opt <- getOptions(static, "pictex");
      if (is.null(debug)) 
          debug <- opt$debug
      if (is.null(fg)) 
          fg <- opt$fg
      if (is.null(bg)) 
          bg <- opt$bg
      pictex <- get("pictex", mode="function")
      fcn(pictex, pathname, width=width, height=height, 
          debug=debug, bg=bg, fg=fg)
  }
}, protected=TRUE, static=TRUE);




#########################################################################/**
# @RdocMethod getParameters
#
# @title "Gets parameters specific to a certain type of device"
# 
# \description{
#  @get "title".
#  This method is used by @seemethod "print" and @seemethod "open".
# }
#
# @synopsis
#
# \arguments{
#   \item{filename}{A filename where to save the resulting plot. The
#      filename suffix decides what graphical device driver will used.}
#   \item{path}{The output path where the figure is saved.}
#   \item{width, height}{The width and height of the image. If both are
#      @NULL the graphical device driver's default settings are used. 
#      If one of them are missing, the other one are set to the default
#      value according to the default settings and the missing argument
#      is assumed to be of size \code{"100\%"}. If any of the two are given
#      as a percentage string, e.g. \code{"75\%"}, its value is calculated
#      to of that percentage of the other value, which must be absolute.}
#   \item{aspectRatio}{If @TRUE, the height will be defined by the
#      aspect ratio of the current device. If @numeric, height will be
#      the width times the aspect ratio. Otherwise aspectRatio is ignored.}
#   \item{pointsize}{The font pointsize to be used. If @NULL the
#      graphical device driver's default settings are used.}
#   \item{quality}{The quality in [0,100]\% if applicable. This argument is
#      currently only used by the jpeg device driver. If @NULL the 
#      graphical device driver's default settings are used.}
#   \item{bg, fg}{Background and foreground colors used by the PicTeX
#      driver. If @NULL the graphical device driver's default settings
#      are used.}
#   \item{debug}{Used by the PicTeX driver. If @NULL the graphical 
#      device driver's default settings are used.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns a named @list structure with elements
#    \code{pathname}, \code{imgName}, \code{imgType},
#    \code{height}, \code{width}, \code{aspectRatio},
#    \code{quality}, \code{pointsize},
#    \code{bg}, \code{fg}, and \code{debug}.
#
#    Value \code{pathname} is the pathname to the image file validate to 
#    writable.
#    Value \code{imgName} is the basename of the pathname without the
#    filename extension, which is returned (in lower case) by \code{imgType}.
# }
# 
# @author
#
# \seealso{
#    @seemethod "print" and @seemethod "open".
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getParameters", "Device", function(this, filename=this$filename, path=getPrintPath(this), width=NULL, height=NULL, aspectRatio=NULL, pointsize=NULL, quality=NULL, bg=NULL, fg=NULL, debug=NULL, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validating arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'filename' and 'path'.
  pathname <- Arguments$getWritablePathname(filename, path=path);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Extracting image name and image extension
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pattern <- "(.*)[.]([^.]*)$";
  imgName <- gsub(pattern, "\\1", basename(pathname));
  imgType <- gsub(pattern, "\\2", basename(pathname));
  imgType <- tolower(imgType);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setting the default width and the height depending on the device.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.element(imgType, c("png", "gif", "bmp", "jpg", "jpeg"))) {
    opt <- getOptions(this, "bitmap");
    defHeight    <- opt$height
    defWidth     <- opt$width;
    defPointsize <- opt$pointsize;
  } else if (is.element(imgType, c("ps", "eps"))) {
    opt <- getOptions(this, "postscript");
    defWidth     <- opt$width;
    defHeight    <- opt$height;
    defPointsize <- opt$pointsize;
  } else if (is.element(imgType, c("tex", "pictex"))) {
    opt <- getOptions(this, "pictex");
    defWidth     <- opt$width;
    defHeight    <- opt$height;
    defPointsize <- opt$pointsize;
  }

  # If the default width is zero, set it to 100%! of the device
  if (defWidth == 0)
    defWidth <- "100%!"
  else if (defHeight == 0)
    defHeight <- "100%!";


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Calculate the width
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.null(width))
    width <- defWidth;

  if (is.character(width)) {
    if (regexpr("^[0-9][0-9]*%$", width) != -1) {
      width <- as.numeric(gsub("%", "", width)) / 100 * defWidth;
    } else if (regexpr("^[0-9][0-9]*%!$", width) != -1) {
      if (is.null(height))
        height <- defHeight;
      if (!is.numeric(height))
        throw("Can not calculate relative size of the width since numeric value of argument 'height' is missing: ", width);
      if (isZero(height))
        throw("Can not calculate relative size of the width since 'height'is zero (see ?setOptions.Device): ", width);
      width <- as.numeric(gsub("%!", "", width)) / 100 * height;
    }
  } else if (!is.numeric(width)) {
    width <- defWidth;
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Calculate the height
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.null(height)) {
    # Assure that the aspect ratio is the same as on the device
    if (identical(aspectRatio, TRUE)) {
      height <- width * getAspectRatio(this, what="plot");
    } else if (is.numeric(aspectRatio)) {
      height <- width * aspectRatio;
    } else {
      height <- defHeight;
    }
  } # if (is.null(height))

  if (is.character(height)) {
    if (regexpr("^plot:", height) != -1) {
      height <- as.numeric(gsub("^plot:", "", height));
      devAspectRatio <- getAspectRatio(this, what="device");
      plotAspectRatio <- getAspectRatio(this, what="plot");
      effAspectRatio <- plotAspectRatio/devAspectRatio;
      print(c(height=height, effAspectRatio=effAspectRatio));
      height <- height * width / effAspectRatio;
#      height <- 0.84 * height;
      print(c(width=width, height=height));
    } else if (regexpr("^[0-9][0-9]*%$", height) != -1) {
      height <- as.numeric(gsub("%", "", height)) / 100 * defWidth;
    } else if (regexpr("^[0-9][0-9]*%!$", height) != -1) {
      if (is.null(width))
        width <- defWidth;
      if (!is.numeric(width))
        throw("Can not calculate relative size of the height since numeric value of argument 'width' is missing: ", height);
      height <- as.numeric(gsub("%!", "", height)) / 100 * width;
    }
  } else if (!is.numeric(height)) {
    height <- defHeight;
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Calculate the pointsize
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.character(pointsize)) {
    pointsize <- as.numeric(gsub("%", "", pointsize)) / 100 * defPointsize;
  } else if (!is.numeric(pointsize)) {
    pointsize <- defPointsize;
  }

  
  list(
    pathname = pathname,
    imgName = imgName,
    imgType = imgType,
    height = height,
    width = width,
    aspectRatio = aspectRatio,
    quality = quality,
    pointsize = pointsize,
    bg = bg,
    fg = fg,
    debug = debug
  );
}, static=TRUE);



#########################################################################/**
# @RdocMethod close
#
# @title "Closes the device"
# 
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
# 
# \examples{\dontrun{
#   dev <- Device("myplot.eps")
#   plot(1,1)
#   dev$close()
# }}
#
# @author
#
# \seealso{
#   \code{\link[grDevices:dev]{dev.off}}.
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("close", "Device", function(con, ...) {
  # To please R CMD check...
  this <- con;

  if (!is.na(this$filename))
    dev.off();
  this$filename <- NA;
})


#########################################################################/**
# @RdocMethod closeAll
#
# @title "Closes all open devices"
# 
# \description{
#  Static method that closes all open devices.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{Returns the number of closed devices.}
# 
# \examples{\dontrun{
#   Device("myplot.eps")
#   plot(1,1)
#   Device("myplot.jpg")
#   plot(1,1)
#   Device$closeAll()
# }}
#
# @author
#
# \seealso{
#   \code{\link[grDevices:dev]{graphics.off}}.
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("closeAll", "Device", function(this, ...) {
  graphics.off();
}, static=TRUE);





#########################################################################/**
# @RdocMethod set
#
# @title "Sets the current device index"
# 
# \description{
#  Static method that sets the current device index. If there exists no 
#  device with that index a device with that index will be created. This 
#  might result in several intermediate device will have to be opened, but
#  they will be closed before returned.
#  Similar to Matlab's \code{figure} function.
# }
#
# @synopsis
#
# \arguments{
#   \item{index}{The index to which the current device will be set to.}
#   \item{device}{Specifies the device driver devices that might be opened.}
#   \item{...}{Not used.}
# }
#
# \value{Returns the number of opened devices.}
# 
# @author
#
# \seealso{
#   \code{\link[grDevices:dev]{dev.set}}.
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("set", "Device", function(this, index, device=x11, width=NULL, height=NULL, ...) {
  defOptions <- check.options(new=list(), envir=grDevices:::.WindowsEnv, 
                                               name.opt=".Windows.Options");
  defWidth <- defOptions$width;
  defHeight <- defOptions$height;

  if (defWidth == 0)
    defWidth <- "100%!"
  else if (defHeight == 0)
    defHeight <- "100%!";

  if (is.null(width))
    width <- defWidth;

  if (is.character(width)) {
    if (regexpr("^[0-9][0-9]*%$", width) != -1) {
      width <- as.numeric(gsub("%", "", width)) / 100 * defWidth;
    } else if (regexpr("^[0-9][0-9]*%!$", width) != -1) {
      if (is.null(height))
        height <- defHeight;
      if (!is.numeric(height))
        throw("Can not calculate relative size of the width since numeric value of argument 'height' is missing: ", width);
      width <- as.numeric(gsub("%!", "", width)) / 100 * height;
    }
  } else if (!is.numeric(width)) {
    width <- defWidth;
  }

  if (is.null(height))
    height <- defHeight;

  if (is.character(height)) {
    if (regexpr("^[0-9][0-9]*%$", height) != -1) {
      height <- as.numeric(gsub("%", "", height)) / 100 * defWidth;
    } else if (regexpr("^[0-9][0-9]*%!$", height) != -1) {
      if (is.null(width))
        width <- defWidth;
      if (!is.numeric(width))
        throw("Can not calculate relative size of the height since numeric value of argument 'width' is missing: ", height);
      height <- as.numeric(gsub("%!", "", height)) / 100 * width;
    }
  } else if (!is.numeric(height)) {
    height <- defHeight;
  }

  if (index < 2)
    stop("Argument 'index' must be two or larger.");

  # If the device is already opened just move to it.
  if (Device$isOpen(index)) {
    dev.set(index);
    return(invisible(0));
  }

  toBeOpened <- setdiff(2:index, dev.list());
  len <- length(toBeOpened);
  if (len > 0) {
    for (idx in toBeOpened) device(width=width, height=height);
    toBeClosed <- setdiff(toBeOpened, index);
    for (idx in toBeClosed) {
      dev.set(idx); dev.off();
    }
  }
  dev.set(index);
  return(invisible(len));
}, static=TRUE);



#########################################################################/**
# @RdocMethod isOpen
#
# @title "Checks if a device is open or not"
# 
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Use 'index' as the index of the device.}
# }
#
# \value{
#   Returns @TRUE if the device is open, otherwise @FALSE.
# }
# 
# @author
#
# \seealso{
#   \code{\link[grDevices:dev]{dev.list}}.
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("isOpen", "Device", function(con, ...) {
  # To please R CMD check
  this <- con;

  args <- list(...);
  index <- args$index;
  if (is.null(index)) index <- args[[1]];
  stopifnot(!is.null(index));

  if (index < 2)
    stop("Argument 'index' must be two or larger.");
  is.element(index, dev.list())
}, static=TRUE);



#########################################################################/**
# @RdocMethod getNext
#
# @title "Opens the next device"
# 
# \description{
#  @get "title". If no such device exists, the a new device is opened.
# }
#
# @synopsis
#
# \arguments{
#   \item{index}{The index of the \emph{current device}.}
#   \item{device}{Specifies the device driver devices that might be opened.}
#   \item{...}{Not used.}
# }
#
# \value{Returns the id number of the opened device.}
# 
# @author
#
# \seealso{
#   \code{\link[grDevices:dev]{dev.set}}.
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("getNext", "Device", function(this, index=dev.cur(), device=x11, ...) {
  Device$set(index+1);
}, static=TRUE);





#########################################################################/**
# @RdocMethod new
#
# @title "Creates a new device"
# 
# \description{
#  Static method that creates a new device. 
# }
#
# @synopsis
#
# \arguments{
#   \item{device}{Specifies the device driver devices that might be opened.}
#   \item{...}{Not used.}
# }
#
# \value{Returns the id number of the opened device.}
# 
# @author
#
# \seealso{
#   \code{\link[grDevices:dev]{dev.set}}.
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("new", "Device", function(this, device=x11, ...) {
  device();
  dev.cur();
}, static=TRUE);


setMethodS3("finalize", "Device", function(this, ...) {
  close(this);
})



#########################################################################/**
# @RdocMethod bringToTop
#
# @title "Bring a device to top"
# 
# \description{
#  @get "title". By default it is the current active device that will
#  be brought to top.
# }
#
# @synopsis
#
# \arguments{
#   \item{which}{Index (an @integer) of device.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns nothing.
# }
# 
# @author
#
# \seealso{
#   @seeclass
# }
#*/#########################################################################
setMethodS3("bringToTop", "Device", function(static, which=dev.cur(), ...) {
  devs <- dev.list();
  if (!is.element(which, devs))
    throw("Can not bring device to top. No such (windows) device: ", which);
  wins <- devs[names(devs) == "windows"];
  if (!is.element(which, wins))
    throw("Can not bring device to top. The device is not a windows device: ", which, " (", names(devs)[which-1], ")");
  bringToTop(which=which);
}, static=TRUE)



setMethodS3("setPlotParameters", "Device", function(this, ..., new=FALSE, dev=NULL) {
  if (is.null(dev)) {
    dev <- dev.cur();
    if (new || length(dev.list()) == 0) dev <- dev+1;
  } else if (new)
    dev <- dev+1;

  if (dev == 1)
    throw("There is no current plot available.");

  envir <- globalenv();
  if (exists(".plotParameters", envir=envir, inherits=FALSE)) {
    .plotParameters <- get(".plotParameters", envir=envir);
  } else {
    .plotParameters <- list();
  }
  .plotParameters[[dev]] <- list(...);
  assign(".plotParameters", .plotParameters, envir=envir);
}, static=TRUE);


setMethodS3("getPlotParameters", "Device", function(this, dev=NULL, ...) {
  if (is.null(dev))
    dev <- dev.cur();
 
  envir <- globalenv();
  if (exists(".plotParameters", envir=envir, inherits=FALSE)) {
    .plotParameters <- get(".plotParameters", envir=envir);
    if (length(.plotParameters) >= dev)
      return(.plotParameters[[dev]]);
  }
  list();
}, static=TRUE);



##############################################################################
# @RdocMethod putTimestamp
#
# \description{
#  The 'put.timestamp' option is used to set timestamps on plots. By
#  default the date is printed automatically (if the plot function supports
#  this option) on the right hand side of the plot (side=4) and right 
#  adjusted (adj=1). The default font size is 80% (cex=0.6) and the default
#  font color is gray (col="darkgray"). The timestamp is plotted on either
#  the inner (outer=FALSE) or the outer margin (outer=TRUE). Counting from
#  inside towards the edge of the device, the timestamp can be printed on
#  any line, default is line=1.
# }
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns @TRUE if the timestamp was added, otherwise @FALSE.
# }
#
# \seealso{
#   The \code{putTimestamp.Device} method is making use of 
#   the funcion @see "graphics::mtext" for writing text in margins.
#
#   @seeclass
# }
##############################################################################
if (is.null(getOption("put.timestamp")))
  options(put.timestamp=list(auto=TRUE, side=4, adj=1, cex=0.7, col="darkgray", line=0, outer=TRUE));

setMethodS3("putTimestamp", "Device", function(this, timestamp=date(), auto=NULL, side=NULL, adj=NULL, cex=NULL, col=NULL, line=NULL, outer=NULL, device=NULL, ...) {
  if (is.null(timestamp) || is.na(timestamp)) return(invisible(FALSE));
  o <- getOption("put.timestamp");
  if (is.null(auto)) auto <- o$auto;
  if (is.null(auto) || !is.logical(auto) || !auto)
    return(invisible(FALSE));
  if (is.null(adj))   adj <- o$adj;
  if (is.null(adj))   adj <- 1;
  if (is.null(cex))   cex <- o$cex;
  if (is.null(cex))   cex <- 0.7;
  if (is.null(col))   col <- o$col;
  if (is.null(col))   col <- "darkgray";
  if (is.null(side))  side <- o$side;
  if (is.null(side))  side <- 4;
  if (is.null(line))  line <- o$line;
  if (is.null(line))  line <- 0;# + 1*(side == 1 || side == 4);
  if (is.null(outer)) outer <- o$outer;
  if (is.null(outer)) outer <- TRUE;

  if (outer) line <- par("oma")[side] - line - 1;

  if (!is.null(device)) {
    if (!is.element(device, dev.list()))
      stop(paste("Can not set timestamp on device", device, 
                                    "since it does not exists."));
    dev.cur <- dev.cur();
    dev.set(device);
  }  

  mtext(timestamp, side=side, adj=adj, cex=cex, col=col, line=line, outer=outer);

  if (!is.null(device)) dev.set(dev.cur);
  return(invisible(TRUE));
}, static=TRUE)


if (is.null(getOption("put.dataset")))
  options(put.dataset=list(auto=TRUE, side=4, adj=0, cex=0.7, col="darkgray", line=0, outer=TRUE));

setMethodS3("putDataset", "Device", function(this, dataset=getOption("dataset"), auto=NULL, side=NULL, adj=NULL, cex=NULL, col=NULL, line=NULL, outer=NULL, device=NULL, ...) {
  if (is.null(dataset) || is.na(dataset)) return(invisible(FALSE));
  o <- getOption("put.dataset");
  if (is.null(auto)) auto <- o$auto;
  if (is.null(auto) || !is.logical(auto) || !auto)
    return(invisible(FALSE));
  if (is.null(adj))   adj <- o$adj;
  if (is.null(adj))   adj <- 0;
  if (is.null(cex))   cex <- o$cex;
  if (is.null(cex))   cex <- 0.7;
  if (is.null(col))   col <- o$col;
  if (is.null(col))   col <- "darkgray";
  if (is.null(side))  side <- o$side;
  if (is.null(side))  side <- 4;
  if (is.null(line))  line <- o$line;
  if (is.null(line))  line <- 0; # + 1*(side == 1 || side == 4);
  if (is.null(outer)) outer <- o$outer;
  if (is.null(outer)) outer <- TRUE;

  if (outer) line <- par("oma")[side] - line - 1;

  if (!is.null(device)) {
    if (!is.element(device, dev.list()))
      stop(paste("Can not set dataset on device", device, 
                                    "since it does not exists."));
    dev.cur <- dev.cur();
    dev.set(device);
  }  

  mtext(dataset, side=side, adj=adj, cex=cex, col=col, line=line, outer=outer);

  if (!is.null(device)) dev.set(dev.cur);
  return(invisible(TRUE));
}, static=TRUE)



if (is.null(getOption("put.author")))
  options(put.author=list(auto=TRUE, side=4, adj=0, cex=0.7, col="darkgray", line=0, outer=TRUE));


setMethodS3("putAuthor", "Device", function(this, author=getOption("author"), auto=NULL, side=NULL, adj=NULL, cex=NULL, col=NULL, line=NULL, outer=NULL, device=NULL, ...) {
  if (is.null(author) || is.na(author)) return(invisible(FALSE));

  o <- getOption("put.author");
  if (is.null(auto)) auto <- o$auto;
  if (is.null(auto) || !is.logical(auto) || !auto)
    return(invisible(FALSE));
  if (is.null(adj))   adj <- o$adj;
  if (is.null(adj))   adj <- 0;
  if (is.null(cex))   cex <- o$cex;
  if (is.null(cex))   cex <- 0.7;
  if (is.null(col))   col <- o$col;
  if (is.null(col))   col <- "darkgray";
  if (is.null(side))  side <- o$side;
  if (is.null(side))  side <- 4;
  if (is.null(line))  line <- o$line;
  if (is.null(line))  line <- 0; # + 1*(side == 1 || side == 4);
  if (is.null(outer)) outer <- o$outer;
  if (is.null(outer)) outer <- FALSE;

  if (outer) line <- par("oma")[side] - line - 1;

  if (!is.null(device)) {
    if (!is.element(device, dev.list()))
      stop(paste("Can not set author on device", device, 
                                    "since it does not exists."));
    dev.cur <- dev.cur();
    dev.set(device);
  }  

  mtext(author, side=side, adj=adj, cex=cex, col=col, line=line, outer=outer);

  if (!is.null(device)) dev.set(dev.cur);
  return(invisible(TRUE));
}, static=TRUE)




#########################################################################/**
# @RdocMethod subplots
#
# @title "Creates a grid of subplots"
#
# \description{
#   @get "title" in the current figure. If arguments
#   \code{nrow} and \code{ncol} are given a \code{nrow}-by-\code{ncol}
#   grid of subplots are created. If only argument \code{n} is given
#   then a r-by-s grid is created where |r-s| <= 1, i.e. a square or almost
#   a square of subplots is created. If \code{n} and \code{nrow} is
#   given then a grid with \code{nrow} rows and at least \code{n} subplots
#   are created. Similar if \code{n} and \code{ncol} is given.
#   The argument \code{byrow} specifies if the order of the subplots
#   should be rowwise (\code{byrow=TRUE}) or columnwise.
# }
#
# @synopsis
#
# \arguments{
#   \item{n}{If given, the minimum number of subplots.}
#   \item{nrow}{If given, the number of rows the grid of subplots should
#               contain.}
#   \item{ncol}{If given, the number of columns the grid of subplots should
#               contain.}
#   \item{...}{Not used.}
# }
#
# \value{Returns the @matrix containing the order of plots.}
#
# @author
#
# \examples{
#    Device$subplots(nrow=2, ncol=3)  # 2-by-3 grid of subplots
#    Device$subplots(n=6, nrow=2)     # 2-by-3 grid of subplots
#    Device$subplots(n=5, ncol=2)     # 3-by-2 grid of subplots
#    Device$subplots(1)               # (Reset) to a 1-by-1 grid of subplots
#    Device$subplots(2)               # 1-by-2 grid of subplots
#    Device$subplots(3)               # 2-by-2 grid of subplots
#    l <- Device$subplots(8)          # 3-by-3 grid of subplots
#    layout.show(length(l))
# }
#
# \seealso{
#   @see "graphics::layout" and \code{layout.show}().
# }
#*/#########################################################################
setMethodS3("subplots", "Device", function(this, n=1, nrow=NULL, ncol=NULL,
                                                          byrow=TRUE, ...) {
  # If a vector was passed, then use the length of the vector for 'n'
  if (!missing(n) && length(n) > 1)
    n <- length(n);
  
  if (!is.null(nrow) && !is.null(ncol) && !missing(n)) {
    if (n != nrow*ncol)
      stop("Arguments 'nrow' and 'ncol' is incompatible with argument 'n'. Do you really want to specify all three?!");
  }

  if (missing(n)) {
    layout <- matrix(seq(nrow*ncol), nrow=nrow, ncol=ncol, byrow=byrow);
  } else {
    if (n == 1) {
      nrow <- ncol <- 1
    } else if (!is.null(nrow)) {
      ncol <- ceiling(n / nrow);
    } else if (!is.null(ncol)) {
      nrow <- ceiling(n / ncol);
    } else {
      side <- sqrt(n);
      nrow <- floor(side);
        ncol <- ncol-1;
      ncol <- ceiling(n / nrow);
      if (ncol - nrow > 1) {
        nrow <- nrow+1;
        ncol <- ceiling(n / nrow);
      }
    }
    layout <- matrix(seq(nrow*ncol), nrow=nrow, ncol=ncol, byrow=byrow);
  }
  
  layout(layout, ...);
  invisible(layout);
}, static=TRUE);


setMethodS3("subplots", "ANY", function(...) {
  Device$subplots(...);
}, static=TRUE);


#########################################################################/**
# @RdocMethod setStyle
#
# @title "Sets plot style for the current device"
#
# \description{
#  @get "title" from a set of predefined plot styles. 
#  Available styles are \code{default} and \code{PowerPoint}. 
#  The style \code{default} is the default parameter settings used by [R]
#  and the style \code{PowerPoint} is a style for generating plots to be
#  used in the presentations, which requires fatter lines, larges fonts etc.
# }
#
# @synopsis
#
# \arguments{
#   \item{style}{The name of the predefined plot style to be applied.}
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
#
# @author
#
# \examples{
#   Device$setStyle("PowerPoint")
#   plot(sin(0:1000))
#   Device$setStyle()            # Reset to default [R] settings.
# }
#
# \seealso{
#   @see "graphics::par"
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("setStyle", "Device", function(this, 
                                    style=c("default", "PowerPoint"), ...) {
  # Annoyingly, when calling par() it will actually open up a new window.
  # By remembering which devices where open before calling par() we can
  # close that newly opened window immediately.
  devices.before <- dev.list();

  NORMAL     <- 1;
  BOLD       <- 2;
  ITALIC     <- 3;
  BOLDITALIC <- 4;
  if (is.null(style))
    return(invisible());
  if (style == "default") {
    par(font=NORMAL);
    par(font.axis=NORMAL);
    par(font.lab=NORMAL);
    par(font.main=BOLD);
    par(font.sub=NORMAL);
    par(cex=1);
    par(cex.axis=1);
    par(cex.lab=1);
    par(cex.main=1.2);
    par(cex.sub=1);
    par(col="black");
    par(col.axis="black");
    par(col.lab="black");
    par(col.main="black");
    par(col.sub="black");
    par(lwd=1);
    par(pch=1);
    par(ps=12);
##    par(tmag=1.2);  ## Obsolete graphical parameter
    par(mar=c(5, 4, 4, 2) + 0.1);
    par(mgp=c(3, 1, 0));
    par(tcl=-0.5);
    setOptions(this, "bitmap", width=480, height=480, pointsize=12, quality=75);
    
    # If there is nothing to reset, ps.options(reset=TRUE) gives an error.
    options(show.error.messages=FALSE);
    try(ps.options(reset=TRUE));
    options(show.error.messages=TRUE);
  } else if (style == "PowerPoint") {
    par(font=BOLD);
    par(font.axis=BOLD);
    par(font.lab=BOLD);
    par(font.main=BOLD);
    par(font.sub=NORMAL);
    par(cex=1.2);
    par(cex.axis=1.2);
    par(cex.lab=1.2);
    par(cex.main=1.4);
    par(cex.sub=1.2);
    par(col="black");
    par(col.axis="black");
    par(col.lab="black");
    par(col.main="black");
    par(col.sub="black");
    par(lwd=2);
    par(pch=1);
    par(ps=12);
##    par(tmag=1.2);  ## Obsolete graphical parameter
    par(mar=c(4, 3, 3, 1) + 0.1);
    par(mgp=c(1.5, 0.2, 0));
    par(tcl=0.3);
    setOptions(this, "bitmap", width=640, height=640, pointsize=14, quality=100);
    setOptions(this, "postscript", width=5, height=5);
  } else if (regexpr("html", style) != -1) {
    par(font=NORMAL);
    par(font.axis=NORMAL);
    par(font.lab=NORMAL);
    par(font.main=NORMAL);
    par(font.sub=NORMAL);
    par(cex=1.0);
    par(cex.axis=1.0);
    par(cex.lab=1.0);
    par(cex.main=1.2);
    par(cex.sub=1.0);
    par(col="black");
    par(col.axis="black");
    par(col.lab="black");
    par(col.main="black");
    par(col.sub="black");
    par(lwd=1);
    par(pch=1);
    par(ps=10);
##    par(tmag=1.2);  ## Obsolete graphical parameter
    par(mar=c(4, 3, 3, 1) + 0.1);
    par(mgp=c(1.8, 0.6, 0));
#   par(mar=c(5, 4, 4, 2) + 0.1);
#   par(mgp=c(3, 1, 0));
    par(tcl=-0.4);
    if (style == "html-80x80") {
      setOptions(this, "bitmap", width=80, height=80, pointsize=8, quality=100);
    } else if (style == "html-160x160") {
      setOptions(this, "bitmap", width=160, height=160, pointsize=10, quality=100); 
    } else if (style == "html-320x320") {
      setOptions(this, "bitmap", width=320, height=320, pointsize=12, quality=100);
    } else if (style == "html-480x480") {
      setOptions(this, "bitmap", width=480, height=480, pointsize=12, quality=100);
    } else if (style == "html-640x640") {
      setOptions(this, "bitmap", width=640, height=640, pointsize=12, quality=100);
    } else {
      setOptions(this, "bitmap", width=640, height=640, pointsize=12, quality=100);
    }
    setOptions(this, "postscript", width=5, height=5);
  }

  # Now close all new windows...
  new.devices <- setdiff(dev.list(), devices.before);
  if (length(new.devices) > 0)
    dev.off(new.devices);
  return(invisible());
}, static=TRUE);



setMethodS3("getMouseClick", "Device", function(static, device=dev.cur(), n=1, region=c("plot", "figure"), normalized=is.element(region, "figure"), ...) {
  region <- match.arg(region);

  Device$set(device);

  xy <- locator(n);
  if (identical(region, "plot")) {
    if (normalized) {
      # Get coordinates in [0,1]x[0,1]
      usr <- par("usr");
      w <- usr[2]-usr[1];
      h <- usr[4]-usr[3];
      xy$x <- (xy$x-usr[1])/w;
      xy$y <- (xy$y-usr[3])/h;
    }
  } else if (identical(region, "figure")) {
    # Get coordinates in plot region
    usr <- par("usr");
    ux <- usr[1];
    uy <- usr[3];
    uw <- usr[2]-ux;
    uh <- usr[4]-uy;

    plt <- par("plt");
    px <- plt[1];
    py <- plt[3];
    pw <- plt[2]-px;
    ph <- plt[4]-py;

    ph.u <- uh/ph;
    pw.u <- uw/pw;
    px.u <- ux - px*pw.u;
    py.u <- uy - py*ph.u;

    # Get coordinates in figure region, but with the same 
    # units as the plot region.
    fig <- par("fig");
    fx <- fig[1];
    fy <- fig[3];
    fw <- fig[2]-fx;
    fh <- fig[4]-fy;

    fh.u <- ph.u/fh;
    fw.u <- pw.u/fw;
    fx.u <- px.u - fx*fw.u;
    fy.u <- py.u - fy*fh.u;

    xy$x <- xy$x - fx.u;
    xy$y <- xy$y - fy.u;

    if (normalized) {
      # Get coordinates in [0,1]x[0,1]
      xy$x <- xy$x/fw.u;
      xy$y <- xy$y/fh.u;
    }
  }

  xy;
}, static=TRUE)



setMethodS3("getDimensions", "Device", function(static, device=dev.cur(), what=c("device", "figure", "plot"), unit=c("inch"), ...) {
  # Validate arguments
  unit <- match.arg(unit);
  what <- match.arg(what);

  par <- switch(what, device="din", figure="fin", plot="pin");

  # Remember active device
  odev <- dev.cur();

  # Query the given device
  dev.set(device);
  if (unit == "inch")
    res <- par(par);

  # Return focus to active device
  dev.set(odev);
  res;
})

setMethodS3("getAspectRatio", "Device", function(static, ...) {
  res <- getDimensions(static, ...);
  res[2]/res[1];
})


#########################################################################/**
# @RdocMethod plotSymbols
#
# @title "Display available plot symbols"
#
# \description{
#  Generates a plot with all available point styles (0-255) that
#  can be specified by the argument \code{pch} in almost all plot functions 
#  in [R]. \code{pch} can be a @vector of @integers specifying a symbol and 
#  @characters.}
#
# @synopsis
#
# \arguments{
#   \item{interactive}{If @TRUE the user is expected to click on symbols 
#     whose @character value, ASCII value as an @integer, an octal decimal and
#     as an hexadecimal.}
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing. Generates a plot.}
#
# @author
#
# \examples{
#   Device$plotSymbols()
# }
#
# \seealso{
#   @see "grDevices::plotmath",
#   @see "graphics::par"
#
#   @seeclass
# }
#*/#########################################################################
setMethodS3("plotSymbols", "Device", function(this, interactive=TRUE, ...) {
  interactive <- interactive && interactive();

  i <- 0:255;
  ncol <-16;
  
  top <- 3 + 2*interactive;
  opar <- par(cex.axis=0.7, mar=c(3,3,top,3)+0.1)
  on.exit(par(opar))

  plot(i%%ncol,1+i%/%ncol, pch=i, xlim=c(0,ncol-1), xlab="", ylab="", 
                                                                axes=FALSE);
  axis(1, at=0:15)
  axis(2, at=1:16, labels=0:15*16, las=2)
  axis(3, at=0:15)
  axis(4, at=1:16, labels=0:15*16+15, las=2)
  if (interactive) {
    title(main="Click on a symbol to add it to the data frame. Click in margin to quit!", cex.main=0.8, line=3.5);
    df <- list();
    usr <- par("usr");
    ready <- FALSE;
    while (!ready) {
      click <- locator(n=1);
      print(click)
      x <- click$x;
      y <- click$y - 1;
      ready <- !(x > -0.5 && x < 15.5 && y > -0.5 && y < 15.5);
      if (!ready) {
        x <- round(x);
        y <- round(y);
        z <- 16*y + x;
        ch  <- intToChar(z);
        dec <- as.character(z); 
        hex <- intToHex(z);
        oct <- intToOct(z);
        spc <- paste(rep("0", 2-nchar(hex)), collapse="");
        hex <- paste(spc, hex, sep="");
        spc <- paste(rep("0", 3-nchar(oct)), collapse="");
        oct <- paste(spc, oct, sep="");
        df$ch  <- c(df$ch , ch );
        df$dec <- c(df$dec, dec);
        df$hex <- c(df$hex, hex);
        df$oct <- c(df$oct, oct);

        if (nchar(ch) == 0) ch <- " ";
        spc <- paste(rep(" ", 3-nchar(dec)), collapse="");
        dec <- paste(spc, dec, sep="");
        cat("Selected ASCII character '", ch, "' ", dec, " 0x", hex, 
                                                 " \\", oct, "\n", sep="");
      }
    }
    return(df);
  }

  invisible();
}, static=TRUE);



#########################################################################/**
# @RdocMethod plotRegions
#
# @title "Static method plotting information about the different regions of a device"
# 
# \description{
#  Static method plotting the borders and displays information about the
#  graphical parameters that are specifying the outer margin region, the
#  figure region and the plot region.
#
#  The code executed for this is the same as the example code.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
# 
# @examples "../incl/Device.plotRegions.Rex"
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/#########################################################################
setMethodS3("plotRegions", "Device", function(static, ...) {
  example("Device.plotRegions", package="R.graphics");
}, static=TRUE)




#########################################################################/**
# @RdocMethod choose
#
# @title "GUI figure selector"
# 
# \description{
#  Opens a window with a list of open figures. By clicking on any figure
#  in the list, that figure is activated and brought to front.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{Returns nothing.}
# 
# @author
#
# \seealso{
#   @seeclass
# }
#*/#########################################################################
setMethodS3("choose", "Device", function(static, ...) {
  require(tcltk) || throw("Package tcltk is missing.");

  item <- c();
  count <- 0;

  getSelectedItem <- function() {
    as.integer(tclvalue(tkcurselection(tl))) + 1;
  }

  selectItem <- function(idx) {
    curr <- getSelectedItem();
    if (!is.na(curr))
      tkselection.set(tl, curr-1);
#    cat("Deselect ", curr, "\n");
    tkselection.set(tl, idx-1);
#    cat("Select ", idx, "\n");
#    print(as.integer(tclvalue(tkcurselection(tl))) + 1);
  }

  onCancel <- function(...) {
    tkdestroy(tt);
  }

  onFocus <- function(...) {
    idx <- getSelectedItem();
    selectedItem <- item[idx];
    win <- as.integer(gsub("Figure ", "", selectedItem));
    dev.set(win);

    # Workaround for bug in bringToTop() R v1.7.1
    missing <- setdiff(2:win,dev.list());
    if (length(missing) > 0) {
      for (k in missing) x11();
      bringToTop(win);
      for (k in missing) {
        dev.set(k);
        Sys.sleep(0.2);
        dev.off();
      }
    } else {
      bringToTop(win);
    }
    updateList();
  }

  onButtonRelease <- function(...) {
    onFocus();
  }

  onUp <- function(...) {
    idx <- getSelectedItem();
    if (idx == 1) {
      idx <- length(item);
    } else {
      idx <- idx - 1;
    }
    selectItem(idx);
    onFocus();
  }

  onDown <- function(...) {
    idx <- getSelectedItem();
    if (idx == length(item)) {
      idx <- 1;
    } else {
      idx <- idx + 1;
    }
    selectItem(idx);
    onFocus();
  }

  updateList <- function(...) {
    win <- dev.list();
    nbrOfWin <- length(win);
    item <<- paste("Figure ", win, sep="");
    tkdelete(tl, 0, "end");
    for (i in seq(item)) {
      tkinsert(tl, "end", item[i]);
    }
    selectItem(which(dev.cur() == win));
  }

  tt <- tktoplevel();

  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...) {
    tkyview(tl, ...);
  })

  tl <- tklistbox(tt, height=15, selectmode="single", 
    background="white", yscrollcommand=function(...) {
    tkset(scr, ...);
  })

  tkgrid(tklabel(tt, text="Select a figure"));
  tkgrid(tl, scr);

  tkgrid.configure(scr, rowspan=4, sticky="nsw")

#  focusButton  <- tkbutton(tt, text="Focus", command=onFocus);
  cancelButton <- tkbutton(tt, text="Cancel", command=onCancel);

#  upButton <- tkbutton(tt, text="-", command=onUp);
#  downButton <- tkbutton(tt, text="+", command=onDown);
#  tkgrid(upButton, downButton, focusButton, cancelButton);
#  tkbind(tt, "<KeyPress-Up>", onUp);
#  tkbind(tt, "<KeyPress-Down>", onDown);

  tkgrid(cancelButton);

  tkbind(tl, "<ButtonRelease-1>", onButtonRelease);

  updateList();

  tkfocus(tt);
})


# Finally, reset the options for all device types.
#Device$resetOptions("postscript");
#Device$resetOptions("bitmap");
#Device$resetOptions("pictex");



#########################################################################/**
# @RdocMethod findPngDevice
#
# @title "Searches for a working PNG device"
# 
# \description{
#  @get "title".
#
#  On Unix, the png device requires that X11 is available, which it is not
#  when running batch scripts or running \R remotely.
#
#  This method tries to create a simple png file first using the 
#  \code{png()} device, and if that is not working it tries the 
#  \code{bitmap()} device, which does not require X11 etc, because it 
#  utilizes Ghostscript. 
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns the \code{png} @function, the \code{bitmap} @function, or @NULL.
# }
# 
# @author
#
# \examples{
#   fcn <- Device$findPngDevice();
#   if (identical(fcn, png)) {
#     cat("PNG device found: png()");
#   } else if (identical(fcn, bitmap)) {
#     cat("PNG device found: bitmap()");
#   } else {
#     cat("PNG device not found.");
#   }
# }
#
# \seealso{
#   @seeclass
# }
#*/#########################################################################
setMethodS3("findPngDevice", "Device", function(static, ...) {
  file <- tempfile("hasPng");
  on.exit({
    if (file.exists(file))
      file.remove(file);
  })

  # First, try png() device, if it exists...
  if (capabilities("png")) {
    tryCatch({
      png(file);
      plot(0);
      dev.off();
      if (file.exists(file))
        return(png);
    }, error = function(error) {
    });
  }

  # Second, try bitmap() device, which utilizes ghostscript...
  tryCatch({
    bitmap(file);
    plot(0);
    dev.off();
    if (file.exists(file))
      return(bitmap);
  }, error = function(error) {
  });

  # Third, try bitmap() device again, but search for ghostview first
  gscmd <- System$findGhostscript();
  if (is.null(gscmd))
    return(NULL);

  tryCatch({
    bitmap(file);
    plot(0);
    dev.off();
    if (file.exists(file))
      return(bitmap);
  }, error = function(error) {
  });
  
  NULL;
}, static=TRUE) 


############################################################################
# HISTORY:
# 2013-02-07
# o BUG FIX: Now Device$get/setPlotParameters() uses the global 
#   environment to store the parameters.
# o BUG FIX: Device$setStyle() no longer tries to set graphical parameter
#   'tmag', because it is now obselete in R.
# 2010-11-28
# o BUG FIX: set() of Device tried to infer the default width and height
#   from the default arguments of the device function.  However, such
#   defaults has been moved to options of the grDevices package.
# o Updated callFunction() for Device such that R CMD check won't complain.
# o BUG FIX: callFunction() for Device used unknown 'this' instead of
#   'static'.
# 2007-05-09
# o Now Device$findPngDevice() is using the System class in R.utils and
#   not the one in R.lang (which is not there anymore).
# 2006-02-03
# o Rdoc bug fix: section 'argument' to 'arguments'.
# 2006-01-21
# o Added Rdoc comments to getParameters().
# o getParameters() now returns the image type (filename extension) in
#   lower case.
# 2005-12-05
# o Added static methods Device$open() to open a device similar to print().
# o Method Device$print() now calls getParameters and callFunction()
#   internally. Should work exactly the same as before.
# o Added private methods getParameters() and callFunction().
# 2005-03-07
# o Added static method findPngDevice() to Device.
# 2005-02-25
# o Added '...' arguments to all methods to please R CMD check.
# 2004-12-14
# o Added test to calculated relative width, e.g. "100%!", when height=0.
# 2004-10-21
# o Added some Rdoc comments.
# 2004-08-17
# o Added getDimensions() and getAspectRatio().
# 2004-08-02
# o BUG FIX: Device$print("xxx.eps") forgot to print with paper="special".
#   This would *not* rescale the background (for instance if forced to 
#   "white"), but only the bounding box. This would result in a figure
#   that overlapped the text in a LaTeX document.
# 2004-04-21
# o Added get-, set- and resetOptions(). This will also fix the R v1.9.0
#   problem where they've "removed" .PostScript.Options, which I previously
#   relied on.
# 2004-03-26
# o BUG FIX: The left and bottom click-outside-margin rules were incorrect
#   for plotSymbols(interactive=TRUE).
# 2003-12-23
# o Added set- and getPrintPath(), which is now used by print(). Thus, it
#   is possible to set a default output path for Device$print().
# 2003-07-18
# o Added choose().
# 2003-07-07
# o Made the Rd example for Device \dontrun{}.
# 2003-05-03
# o Now setPlotParameters() opens a new window if non is currently open.
#   Solved the bugs introduced some time ago in com.braju.sma.
# 2003-04-18
# o BUG FIX: Device$print() and Device$set() did not calculate the width
#   correclty if it was given as a percentage of the default width, e.g.
#   "200%". It became the percentage of the height instead.
# o BUG FIX: Device$print() and Device$set() did not accept numerical
#   values for width, height and pointsize. If tried, the default value
#   would be used instead.
# o Update the Rdocs to make use of the new/updated Rdoc tags.
# 2003-02-21
# o Updated set() and print() to be more clever about selecting the width
#   and the height if the arguments are missing etc.
# 2003-01-17
# o Added getMouseClick(), which return the (standardize [0,1]x[0,1] or
#   plot) coordinates of one or several mouse click similar to locator(),
#   but here the coordinates can also be relative to the window (the figure
#   region) and not only to the plot region.
# 2003-01-15
# o Added bringToTop(), which basically just call bringToTop(), but got 
#   somewhat fancier error messages.
# 2003-01-10
# o Added arguments width and height to Device$set().
# 2003-01-07
# o Made height="100%!" the default value for Device$print().
# 2003-01-03
# o BUG FIX: Using numeric values for the width and height arguments of
#   Device$print() was broken due to the changes in previous version.
# 2002-12-07
# o Now both argument 'width' and 'height' of Device$print() can be
#   specifying a relative (percentage) size at the same time. It is not a
#   percentage of the other axis (as it used to be), but the percentage of
#   the default size. To specify the relative size to the other axis one
#   has to add a "!", e.g. width="100%!", height="50%" to make the height
#   50% of the default device size and the width the same as the height.
# 2002-11-11
# o BUG FIX: The interactive mode will only take place if R is run in
#   an interactive mode, i.e. if interactive() == TRUE.
# 2002-10-31
# o plotSymbols() now also has an interactive mode where one can pick 
#   character to a list by click on them.
# o Move plotSymbols() from Plots to this class.
# o Same for setStyle() and subplots().
# 2002-10-29
# o Added static method Device$plotRegions().
# o Rename .PlotParameters to .plotParameters *according to RCC) and made
#   it an internal variable of the R.graphics environment. This required
#   updating the set-/getPlotParameters() methods.
# 2002-10-28
# o Updated some of the Rdoc comments.
# o For Device$print() if only one of the 'width' and 'height' is given,
#   the other one is assumed to be equal to "100%". If both are missing
#   the default size according to .Bitmap.Options et al are used.
# 2002-06-24
# * BUG FIX: When R CMD check is runned .PlotParameters is not correctly
#   created. Added a workaround in set-/getPlotParameter().
# 2002-05-05
# * IMPROVEMENT: If for instance on did png <- 1 and the called
#   Device$print("foo.png") dev.print() would complain that 'png' is not
#   a function.
# 2002-04-03
# * Added support for (R.io) File object filename specifications.
# 2002-03-30
# * Now library R.io is not needed, but R.base instead, which is hopefully
#   smaller and faster to load and most likely already loaded. Added
#   baseAndExtension() in R.base to deal with filename extensions.
# 2002-01-24
# * Added putTimestamp(), putAuthor() and putDataset().
# 2002-01-23
# * Update to make use of setCl assS3 and setMethodS3.
# * BUG FIX: width="50%" did not work. It was a typo and now it works.
# 2001-11-24
# * Added the static function next().
# * Now using modifiers() instead of attr().
# 2001-09-20
# * Added support for overriding the default image attribute (width, ...)
#   in the Device$print() method.
# 2001-08-06
# * Device$set() will now close all intermediate device() before returning.
# * BUG FIX: Device$set() wouldn't open a new device for index=k if there
#   already existed devices with index > k.
# * Added isOpen().
# 2001-07-28
# * Added Device$new().
# 2001-07-16
# * Added get- and setPlotParameters(). A first move away from the 
#    .lastPlot fields in MicroarrayData classes. PlotParameters should be
#    globally accessable, not just from the last plotting object.
# * try-catch the error if there is nothing to reset in ps.options(reset=T).
# 2001-06-23
# * Added Rdoc comments.
# 2001-06-08
# * Added set(), which works similar to Matlab's figure().
# * Added closeAll() for simplicity. It simply calls graphics.off().
# 2001-05-14
# * Added getInternalReferences() for improving gco() performance.
# 2001-05-04
# * Now supports formal attributes.
# * Remove constructor argument and class field 'new'.
# 2001-05-01
# * Added finalize().
# 2001-04-11
# * Created from old com.braju.graphics and made into a static class, but
#   changed it quite quickly into a regular class.
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
