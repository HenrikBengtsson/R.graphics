data(volcano)
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

phi=40
theta=40

opar <- par(mar=c(2,1,2,1)+0.1)
layout(matrix(1:6, nrow=2, ncol=3))
persp(x,y,z, col=Colors$getGray(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getGray()")
persp(x,y,z, col=Colors$getTopoColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getTopoColors()")
persp(x,y,z, col=Colors$getTerrainColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getTerrainColors()")
persp(x,y,z, col=Colors$getHeatColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getHeatColors()")
persp(x,y,z, col=Colors$getRainbowColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getRainbowColors()")
persp(x,y,z, col=Colors$getCyanMagentaColors(z)[-1,-1], border=NA, phi=phi, theta=theta)
title("Colors$getCyanMagentaColors()")

par(opar)