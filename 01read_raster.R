
rm(list = ls());gc()
library(raster)
raster_ABBY<-as.matrix(raster('F:/NEON/ABBY/ABBY.tif'))
raster_ABBY_list <- as.vector(raster_ABBY)
summary(raster_ABBY_list)
raster_ABBY_BILINEAR<-as.matrix(raster('F:/NEON/ABBY/ABBY_30m.tif'))
raster_ABBY_BILINEAR_list <- as.vector(raster_ABBY_BILINEAR)
summary(raster_ABBY_BILINEAR_list)
raster_ABBY_NEAREST<-as.matrix(raster('F:/NEON/ABBY/ABBY_30m_nearist.tif'))
raster_ABBY_NEAREST_list <- as.vector(raster_ABBY_NEAREST)
summary(raster_ABBY_NEAREST_list)

library(ggplot2)
plot(density(raster_ABBY_list),xlab = 'Height(m)',col=rainbow(3)[1],xlim=c(0,60),
     ylim=c(0,0.5),main = '',lwd=2)
lines(density(raster_ABBY_BILINEAR_list), col=rainbow(3)[2],lwd=2)
lines(density(raster_ABBY_NEAREST_list), col=rainbow(3)[3],lwd=2)
legend("topright", inset = 0.02, c('ABBY','ABBY_BILINEAR','ABBY_NEAREST'),
       lwd=2,col = rainbow(3), bg = "gray")
