set.seed(101)
d <- data.frame(x=runif(1000),y=runif(1000),z=runif(1000),
                f=factor(sample(1:10,replace=TRUE,size=1000)))
library("lattice")
cloud(z~x*y|f,data=d, scales = "free", 
      panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.ellipse(x, y, lwd = 2, ...)
        panel.ellipse(x, y, lwd = 2, col="red", robust=TRUE, ...)
      })

wireframe(z~x*y|f,data=d, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10))

## Compare classical with robust
xyplot(z~x*y|f,data=d,scales = "free",
       par.settings = list(plot.symbol = list(cex = 1.1, pch=16)),
       layout=c(4,4),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.ellipse(x, y, lwd = 2, ...)
         panel.ellipse(x, y, lwd = 2, col="red", robust=TRUE, ...)
       })

