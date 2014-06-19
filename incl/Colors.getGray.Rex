x <- 1:8; y <- rep(1,8);
plot(NA, xlim=c(1,8), ylim=c(1,2), xlab="", ylab="");

# Generate "colors" in the full grayscale range
colors <- Colors$getGray(x);
points(x,1*y, col=colors, pch=19, cex=5);

# Generate "colors" in the grayscale range [0.3, 0.8]
colors <- Colors$getGray(x, dim.range=c(0.3,0.8));
points(x,2*y, col=colors, pch=19, cex=5);

