quartz()

#Dumb tests by zheng, don't do this before running the rest of the code
imgtest <- brick("lowqualpalmyra.tif")
names(imgtest)
#projected <- projectRaster(imgtest,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")

#names(classed) <- "classification"
#rat <- levels(classed)[[1]]
#rat[["landcover"]] <- c("Cocos","Native", "Scaevola","Sand/Other")
#levels(classed) <- rat
#plot(as.factor(classed),alpha=0.7)
plot(classed,add=TRUE,alpha=0.5)

shapefiledir = "../Palmyra Crab Research/Crab tagging/crab tracks 2017/speed-filtered shapefiles/crab-01-speedtrimmed.shp"
shapefile = st_read(shapefiledir)
shapefile2 <- st_transform(shapefile, "+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
plot(shapefile2,col=shapefile2$Date,pch=15,max.plot=1,add=TRUE,cex=0.3)
dev.off()

