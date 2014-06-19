xlim <- c(0,10);
ylim <- c(-1,1);

if (!exists("ga"))
  ga <- GifAnimator();

xs <- seq(from=xlim[1], to=xlim[2], length=21);
col <- rainbow(length(xs));
for (kk in seq(xs)) {
  x <- xs[kk];
  print(x);
  if (!isFrameRecorded(ga)) {
#    recordOpen(ga);
    par(bg=col[kk]);
    plot(x,sin(x), xlim=xlim, ylim=ylim, lwd=2, cex=2, fill="white");
#    recordClose(ga);
    recordCurrent(ga);
  } else {
    skip(ga);
  }
}

combineFrames(ga, bounce=FALSE);

