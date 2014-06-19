h <- 1:8;
s <- 1:8;
v <- 1:8;

x <- 1:8; y <- rep(1,8);
plot(NA, xlim=c(1,8), ylim=c(1,4), xlab="", ylab="");

# Generate colors in the full hue range
colors <- Colors$getHSV(h);
# [1] "#FFFFFF" "#FFFADB" "#CBFFB6" "#92FFD0" 
# [4] "#6DC1FF" "#7D49FF" "#FF24E0" "#FF0000"
points(x,1*y, col=colors, pch=19, cex=5);

# Generate colors in the full saturation range with hue=0 (default)
colors <- Colors$getHSV(s, dim="s");
# [1] "#FFFFFF" "#FFDBDB" "#FFB6B6" "#FF9292" 
# [4] "#FF6D6D" "#FF4949" "#FF2424" "#FF0000"
points(x,2*y, col=colors, pch=19, cex=5);

# Generate colors in the full value range with hue=0 (default)
colors <- Colors$getHSV(v, dim="v");
# [1] "#000000" "#240000" "#490000" "#6D0000"
# [4] "#920000" "#B60000" "#DB0000" "#FF0000"
points(x,3*y, col=colors, pch=19, cex=5);

# Generates red to green colors in the saturation range [0.3,0.8].
data <- matrix(1:8, nrow=8, ncol=2);
dim.range <- matrix(c(Colors$RED.HUE,Colors$GREEN.HUE, 0.3,0.8), nrow=2);
colors <- Colors$getHSV(data, dim.range=dim.range);
# [1] "#FFB3B3" "#FFBBA0" "#FFCF8E" "#FFEC7C"
# [4] "#EAFF6A" "#B7FF57" "#7AFF45" "#33FF33"
points(x,4*y, col=colors, pch=19, cex=5);
