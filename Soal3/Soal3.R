library(BSDA)
library(mosaic)

#3B
tsum.test(mean.x = 3.63, s.x = 1.67, n.x = 19,
          mean.y = 2.79, s.y = 1.32, n.y = 27,
          alternative = "greater", var.equal = TRUE)

#3C
plotDist(dist = 't', df = 2, col = "red")

#3D
qt(p = 0.05, df = 2, lower.tail = FALSE)
