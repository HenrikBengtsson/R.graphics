setConstructorS3("GifAnimator", function(filename="Ranimation.gif", path=NULL, clearOnFinalize=TRUE) {
  filename <- as.character(filename);
  recordPrefix <- gsub("[.][^.]*$", "", filename);

  extend(Object(), "GifAnimator",
    filename        = filename,
    path            = path,
    recordSuffix    = "png",
    recordPrefix    = recordPrefix,
    recordPath      = tempdir(),
    width           = NULL,
    height          = NULL,
    count           = as.integer(1),
    clearOnFinalize = as.logical(clearOnFinalize)
  )
})


setMethodS3("as.character", "GifAnimator", function(x, ...) {
  # To please R CMD check.
  this <- x;

  s <- paste(data.class(this), ": ", sep="");
  s <- paste(s, "pathname=", getPathname(this), sep="");
  s <- paste(s, ", next frame=", this$count, sep="");
  s <- paste(s, ", number of frames recorded=", length(getFrames(this)), sep="");
  s <- paste(s, ". frame format=", getRecordFormat(this), sep="");
  s;
});


setMethodS3("finalize", "GifAnimator", function(this, ...) {
  # Remove recorder path
  if (this$clearOnFinalize)
    clearRecord(this);
})

setMethodS3("getRecordPath", "GifAnimator", function(this, ...) {
  path <- as.character(this$recordPath);
  if (is.null(path))
    return("");
  path <- gsub("\\\\", "/", path);
  path <- gsub("/$", "", path);
  path <- paste(path, "/", sep="");
  path;
})

setMethodS3("setRecordPath", "GifAnimator", function(this, path=tempdir(), ...) {
  this$recordPath <- path;
})

setMethodS3("clearRecord", "GifAnimator", function(this, ...) {
  path <- getRecordPath(this);
  unlink(path, recursive=TRUE);
  dir.create(path);
})

setMethodS3("getFrames", "GifAnimator", function(this, full.names=FALSE, ...) {
  path <- getRecordPath(this);
  pattern <- paste(this$recordPrefix, "-.....[.]", this$recordSuffix, sep="");
#  pattern <- paste(this$recordPrefix, "-.....[.]", "gif", sep="");
  frames <- list.files(path=path, pattern=pattern, full.names=full.names);
  frames;
});

setMethodS3("getRecordFormat", "GifAnimator", function(this, ...) {
  paste(this$recordPrefix, "-%05i.", this$recordSuffix, sep="");
})

setMethodS3("seq", "GifAnimator", function(this, bounce=FALSE, loop=FALSE, ...) {
  index <- as.integer(seq(length=length(getFrames(this))));
  if (bounce) {
    if (loop)
      index <- c(index, rev(index[-1])[-1])
    else
      index <- c(index, rev(index)[-1]);
  }
  index;
})

setMethodS3("combineFrames", "GifAnimator", function(this, delay=10, loop=TRUE, bounce=FALSE, order=seq(this, bounce=bounce, loop=loop), ...) {
  frames <- getFrames(this);
  frames <- frames[order];
  
  imageConverter <- getOption("imageConverter");
  if (is.null(imageConverter))
    throw("Image converter is not set. Use options(imageConverter=...).");

  animGenerator <- paste("\"", imageConverter, "\"", " -adjoin", sep="");

  inputFiles <- paste(frames, collapse=" ");
  owd <- getwd();
  on.exit(setwd(owd));
  setwd(getRecordPath(this));

  options <- paste("-delay", delay);
  if (loop)
    options <- paste("-loop 0", options);

  cmd <- paste(animGenerator, options, inputFiles, getPathname(this), sep=" ");
  system(cmd);

  cmd;
})

setMethodS3("getCurrentFrame", "GifAnimator", function(this, ...) {
  as.integer(this$count);
})

setMethodS3("isFrameRecorded", "GifAnimator", function(this, frame=getCurrentFrame(this), ...) {
  pathname <- sprintf(getRecordFormat(this), as.integer(frame));
  pathname <- paste(getRecordPath(this), pathname, sep="");
  file.exists(pathname);
})

setMethodS3("getCurrentFrameName", "GifAnimator", function(this, fullname=FALSE, ...) {
  s <- sprintf(getRecordFormat(this), getCurrentFrame(this));
  if (fullname)
    s <- paste(getRecordPath(this), s, sep="");
  s;
})

setMethodS3("getPathname", "GifAnimator", function(this, ...) {
  paste(this$path, this$filename, sep="");
})

setMethodS3("reset", "GifAnimator", function(this, removeAll=FALSE, ...) {
  this$count <- as.integer(0);
  if (removeAll)
    clearRecord(this);
})

setMethodS3("skip", "GifAnimator", function(this, ...) {
  this$count <- as.integer(this$count + 1);
})

setMethodS3("recordCurrent", "GifAnimator", function(this, ...) {
  pathname <- getCurrentFrameName(this, fullname=TRUE);
  width <- if (is.null(this$width)) 640 else this$width;
  height <- if (is.null(this$height)) 480 else this$height;
  dev.print(device=png, pathname, width=width, height=height);
  skip(this);
  invisible(getCurrentFrame(this));
})

setMethodS3("recordOpen", "GifAnimator", function(this, ...) {
  pathname <- getCurrentFrameName(this, fullname=TRUE);
  width <- if (is.null(this$width)) 640 else this$width;
  height <- if (is.null(this$height)) 480 else this$height;
  png(pathname, width=width, height=height);
})

setMethodS3("recordClose", "GifAnimator", function(this, ...) {
  dev.off();
  skip(this);
  invisible(getCurrentFrame(this));
})
