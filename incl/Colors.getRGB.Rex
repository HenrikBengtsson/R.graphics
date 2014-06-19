r <- 1:5;
g <- 1:5;
b <- 1:5;

x <- 1:5; y <- rep(1,5);
plot(NA, xlim=c(1,5), ylim=c(1,5), xlab="", ylab="");

# Generate full-range red colors
colors <- Colors$getRGB(r);
# "#000000" "#400000" "#800000" "#BF0000" "#FF0000"
points(x,1*y, col=colors, pch=19, cex=5);

colors <- Colors$getRGB(matrix(c(r,g), ncol=2));
# "#000000" "#404000" "#808000" "#BFBF00" "#FFFF00"
points(x,2*y, col=colors, pch=19, cex=5);

# Threshold the "g" channel to the interval [2,4].
data <- matrix(c(r,b,g), ncol=3);
data.range <- matrix(c(range(r), range(b), 2,4), ncol=2);
colors <- Colors$getRGB(data, data.range, dim=c("r","b","g"));
# "#000000" "#404000" "#808080" "#BFBFFF" "#FFFFFF"
points(x,3*y, col=colors, pch=19, cex=5);

# Generate light red colors
colors <- Colors$getRGB(r, dim.range=c(0.5,1));
# "#800000" "#9F0000" "#BF0000" "#DF0000" "#FF0000"
points(x,4*y, col=colors, pch=19, cex=5);

# Generate dark red colors
colors <- Colors$getRGB(r, dim.range=c(0.2, 0.7));
# "#330000" "#530000" "#730000" "#930000" "#B30000"
points(x,5*y, col=colors, pch=19, cex=5);
