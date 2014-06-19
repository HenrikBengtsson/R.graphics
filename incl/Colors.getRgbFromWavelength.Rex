require(R.base)

low <- 380
high <- 780
lambda <- seq(from=low, to=high)
rgb <- Colors$getRgbFromWavelength(lambda)
col <- rgb2col(rgb)
opar <- par(mar=c(5,2,6,2)+0.1)
image.matrix(x=as.matrix(lambda), y=as.matrix(1), z=matrix(lambda, nrow=1), col=col, xlab=expression(paste("Top: frequency (THz), Bottom: ", lambda, "- wavelength (nm)")), ylab="", axes=FALSE)
at <- seq(from=min(lambda), to=max(lambda), by=20)
axis(side=1, at=at)
freq <- round((3e8 / (at * 1e-9)) / 10^12)
axis(side=3, at=at, labels=freq)
title(main="Colors$getRgbFromWavelength(lambda)")
par(opar)
