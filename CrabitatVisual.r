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
shapefile2 <- st_transform(shapefile, "+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(shapefile2, pch=15,cex=0.3,add=TRUE)

crabList = list()
for (crab in unique(crabs201X$CrabNum)) {
  crabList[[crab]] = Lines(list(Line(crabs201X[which(crabs201X$CrabNum == crab),c("Longitude","Latitude")])),ID=crab)
}
#Converts coordinates to a spatial lines object
testSL = SpatialLines((crabList[101]))
testSL@lines = crabList[unique(crabs201X$CrabNum)]
proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
testUTMSL@lines
plot(testUTMSL,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
dev.off()


#Plots individual crab tracks per island, by day
pdf("CrabVisualTracks.pdf")
for (crab in unique(crabs201X$CrabNum)) {
  crabList = list()
  thisCrabTrax = crabs201X[which(crabs201X$CrabNum == crab),]
  island = thisCrabTrax[1,"Island"]
  imgtest <- brick(paste0(island,".tif"))
  #proj4string(imgtest)
  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  for (date in 1:length(unique(thisCrabTrax$Date))) {
    crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
  }
  colors = rainbow(length(unique(thisCrabTrax$Date)))
  testSL = SpatialLines((crabList[1]))
  testSL@lines <- crabList
  proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1)
  plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
  legend(title=paste("Crab:",crab),x=(imgtest@extent[1]+imgtest@extent[2])/2,y=(imgtest@extent[3]+imgtest@extent[4])/2,legend=unique(thisCrabTrax$Date),col=colors,lty=1,cex=0.75)
}
dev.off()

