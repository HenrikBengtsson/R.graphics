# Symbols to be benchmarked
pchs <- c(46,          # The dot == "."
          0:25,        # Standard plot symbols
          32:45,47,    # Some other ASCII character
          65:68,96:99, # Somer letters
          144,149,176  # Some symbol like ASCII++ characters
);


# Set random seed
set.seed(33);

# Get N random data points
N <- 10000;
x <- rnorm(N);
y <- rnorm(N);

lim <- range(x,y);

# Allow for varying cex
cex <- 1;

benchmark <- NULL;

for (pch in pchs) {
  # First create an empty plot
  plot(NA, xlim=lim, ylim=lim);

  # Activate the garbage collector
  gc();

  # Then time the plotting of the symbols
  dt <- system.time({
    points(x,y, pch=pch, cex=cex);
  });

  names(dt) <- c("user cpu", "system cpu", "elapsed", "subproc1", "subproc2");
  benchmark <- rbind(benchmark, c(pch, dt));
  colnames(benchmark) <- c("pch", names(dt));

  dt <- formatC(dt, format="f", width=7, digits=3);
  dt <- paste(dt, collapse=", ");
  cat(sprintf("pch %3d : dt=%s\n", as.integer(pch), dt));
}

# Remove zero-timers
benchmark <- benchmark[(benchmark[,"elapsed"] > 0),];

# Sort by speed
o <- order(benchmark[,"elapsed"]);
benchmark <- benchmark[o,];
K <- nrow(benchmark);

# Extract timing information
elapsed <- benchmark[,"elapsed"];
#elapsed <- log(elapsed, base=10);

rev <- K:1;
mp <- barplot(elapsed[rev], horiz=TRUE);
x <- rep(0,K);
pch <- as.integer(benchmark[rev,"pch"]);
points(rep(-0.2,length(mp)),mp, pch=pch, xpd=TRUE);
