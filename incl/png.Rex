myImg <- function(side) {
  n <- side**2
  nrow <- side - 5
  ncol <- side %/% 3
  z <- matrix(1:n, nrow=nrow, ncol=ncol) # white
  z[1:nrow,c(1,2,ncol-1,ncol)] <- 1  # black
  z[c(1,2,nrow-1,nrow),1:ncol] <- 1  # black
#  z[1,1] <- 3               # red
#  z[1,side] <- 4            # green
#  z[side,1] <- 4            # green
#  z[side,side] <- 3         # red
  z
}

colorMap <- c("white", "black", "red", "green")
writePNG <- function(z, filename, fix=0) {
  # bg="blue" for debug purposes
  png(filename, width=ncol(z)+fix, height=nrow(z), bg="blue")
  on.exit(dev.off())
  par(mar=c(0,0,0,0))
  x <- 1:(ncol(z))
  y <- 1:(nrow(z))
  image(x, y, z, col=colorMap, axes=FALSE)
}

writeBMP <- function(z, filename, fix=0) {
  # bg="blue" for debug purposes
  bmp(filename, width=ncol(z)+fix, height=nrow(z), bg="blue")
  on.exit(dev.off())
  par(mar=c(0,0,0,0))
  y <- 1:(ncol(z))
  x <- 1:(nrow(z))
  image(x, y, z, col=colorMap, axes=FALSE)
}


s <- 21
colorMap <- c("black", rainbow(s**2))
rndCol <- rgb2col(as.integer(runif(s**2, 0,256))%%256)
colorMap <- c("black", rep(unique(rndCol), length.out=s**2))
str(length(colorMap))
z <- myImg(s)
str(length(unique(z)))
str(size(z))
y <- 1:(ncol(z)+1)
x <- 1:(nrow(z)+1)
image(x,y,z, col=colorMap)
writeBMP(z, "foo.bmp")
writeBMP(z, "bar.bmp", fix=1)
