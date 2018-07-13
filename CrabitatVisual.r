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
plot(shapefile2, pch=15,cex=0.3,add=TRUE)

crabList = list()
for (crab in unique(crabs201X$CrabNum)) {
  crabList[[crab]] = Lines(list(Line(crabs201X[which(crabs201X$CrabNum == crab),c("Longitude","Latitude")])),ID=crab)
}

testSL = SpatialLines((crabList[101]))

testSL@lines = crabList[unique(crabs201X$CrabNum)]
proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

plot(testUTMSL,col=c("green","red","blue","yellow","purple","pink"),pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1)
dev.off()

