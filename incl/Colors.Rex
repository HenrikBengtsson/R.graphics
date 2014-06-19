opar <- par("ask")


# Example 1
n <- 20
idx <- (0:(n-1))/n
v <- c(idx,1)
s <- c(idx,1)
h <- idx
theta <- 2*pi*idx
radius <- 0.4
x.offset <- 0.5
y.offset <- 0.5
x0 <- 0.8*radius*cos(theta)+x.offset
y0 <- 0.8*radius*sin(theta)+y.offset
p0 <- matrix(c(x0,y0), ncol=2)
x1 <- 1.2*radius*cos(theta)+x.offset
y1 <- 1.2*radius*sin(theta)+y.offset
p1 <- matrix(c(x1,y1), ncol=2)
labels <- formatC(idx,2) 
plot(x.offset,y.offset, pch=" ", xlim=c(0,1), ylim=c(0,1), ann=FALSE)

hue <- Colors$getHSV(theta, dim="h")
points(x0,y0, pch=20, col=hue, cex=10)
text(x0,y0, label=labels)
text(x.offset, y.offset, "Hue")

value <- Colors$getHSV(v, dim="v")
points(v, rep(0,n+1), pch=20, col=value, cex=5)

saturation <- Colors$getHSV(s, dim="s")
points(rep(0,n+1), s, pch=20, col=saturation, cex=5)

title(main="HSV Colorspace", xlab="V - Value", ylab="S - Saturation")



# Example 2
x <- (0:200)/200
y1 <- (1+sin(2*pi*x))/2
y2 <- x
colors <- Colors$getRGB(matrix(c(x,y1), ncol=2), dim=c("r", "g"))
plot(x,y1, col=colors, pch=20, cex=10, ann=FALSE)
colors <- Colors$getRGB(matrix(c(x,y2), ncol=2), dim=c("r", "g"))
points(x,y2, col=colors, pch=20, cex=10)
title(main="RGB color space", xlab="Amount of red", ylab="Amount of green")



# Example 3
n <- 20
x <- (0:n)/n
y <- (1+sin(2*pi*x))/2
colors <- Colors$getGray(y)
invcolors <- Colors$getGray(sign(y-0.5), dim.range=c(1,0))
plot(x,y, col=colors, pch=20, cex=10, ann=FALSE)
text(x,y, col=invcolors, label=formatC(y,digits=2,format="f"))
title(main="Grayspace", xlab="", ylab="Gray level")

par(opar)
