%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Device.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plotRegions.Device}
\alias{plotRegions.Device}
\alias{Device.plotRegions}
\alias{plotRegions.Device}
\alias{plotRegions,Device-method}


 \title{Static method plotting information about the different regions of a device}

 \description{
  Static method plotting the borders and displays information about the
  graphical parameters that are specifying the outer margin region, the
  figure region and the plot region.

  The code executed for this is the same as the example code.
 }

 \synopsis{plotRegions.Device(static, ...)}
\usage{Device$plotRegions(...)}

 \arguments{
   \item{...}{Not used.}
 }

 \value{Returns nothing.}

 \examples{
# Outer margin
par(oma=c(2.5,1,4,1))

# Figure region
par(fig=c(0.05,0.95,0.1,0.9))

# Plot region
par(mar=c(6,5,4,1)+0.1)

# Create a new empty plot canvas
plot.new()

# Set up world coordinates for graphics window
plot.window(xlim=c(0,1), ylim=c(0,2))

# Plot a sample curve
curve(1+1.1*sin(10*x), from=-1, to=2, col="red", lwd=3, lty=4, add=TRUE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 0. Reading graphical parameters to be used later
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the outer margins of the plot region limited by the device border
oma <- par("oma")  # in text lines
omi <- par("omi")  # in inches
omd <- par("omd")  # in percentage of the device region

# Get the limits for the figure region limited by the outer margin
fig <- par("fig")  # in text lines

# Get the margins of the plot region limited by the figure region
mar <- par("mar")  # in text lines
mai <- par("mai")  # in inches
plt <- par("plt")  # in percentage of the figure region
usr <- par("usr")  # in user coordinates


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Outer margin is the outmost margin
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Show the *outer* limit of the outer margin region
box(which="outer", lwd=4, lty=2, col="blue")

# Show the *inner* limit of the outer margin region
box(which="inner", lwd=3, lty=1, col="blue")

# Add label of the outer margin
mtext(side=3, expression(italic("outer margin region")), line=0.2, adj=0.5,
                                               col="blue", outer=TRUE)

line <- c(-1.2,-1,-1,-1.4)
for (side in 1:4) {
  s <- sprintf("oma[\%d]=\%.3f (lines), omi[\%d]=\%.3f (inches), omd[\%d]=\%.3f (\%\%)", side, oma[side], side, omi[side], side, omd[side])
  mtext(side=side, s, line=line[side], col="blue", outer=TRUE)
}

title(main="Main title in outer margin @ line=2", line=2, outer=TRUE)
title(sub="Subtitle in outer margin @ line=0.5", line=0.5, outer=TRUE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. The figure region is an internal region of the outer margin
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Show the figure region
box(which="figure", lwd=3, col="purple")

s <- sprintf("fig[1:2]=c(\%.3f,\%.3f) (\%\% of outer margin region)",
                                                          fig[1], fig[2])
mtext(side=1, s, line=mar[1]-1.4, col="purple")
s <- sprintf("fig[3:4]=c(\%.3f,\%.3f) (\%\% of outer margin region)",
                                                          fig[3], fig[4])
mtext(side=2, s, line=mar[2]-1, col="purple")
mtext(side=3, expression(italic("figure region")), line=mar[3]+0.2,
                                                   adj=0.5, col="purple")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. The plot region is an internal region of the outer margin
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Show the plot region
box(which="plot", lwd=3, lty=1, col="black")
mtext(side=3, expression(italic("plot region")), line=0.2, adj=0.5)

# Plot information about all the margins value
line <- c(-1.4,-2,-2,-1.4)
for (side in 1:4) {
  s <- sprintf("mar[\%d]=\%.3f (lines),\n mai[\%d]=\%.3f (inches)",
                side, mar[side], side, mai[side])
  mtext(side=side, s, line=line[side])
}

title(main="Main title @ line=3", line=2.5)
title(sub="Subtitle @ line=3", line=3)

s <- sprintf("usr[1:2]=c(\%.3f,\%.3f),\nplt[1:2]=c(\%.3f,\%.3f) (\%\% of figure region)", usr[1], usr[2], plt[1], plt[2])
mtext(side=1, s, line=1.2, col="red")
s <- sprintf("usr[3:4]=c(\%.3f,\%.3f),\nplt[3:4]=c(\%.3f,\%.3f) (\%\% of figure region)", usr[3], usr[4], plt[3], plt[4])
mtext(side=2, s, line=0.4, col="red")
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   For more information see \code{\link{Device}}.
 }
\keyword{internal}
\keyword{methods}
