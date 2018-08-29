quartz()

#Dumb tests by zheng, don't do this before running the rest of the code
imgtest <- brick("lowqualpalmyra.tif")
names(imgtest)
#projected <- projectRaster(imgtest,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
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
for (crab in unique(crabs201X$CrabNum)) {
  crab= 1
  print(crab)
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
  legend(title=paste("Crab:",crab),x="topleft",legend=unique(thisCrabTrax$Date),col=colors,lty=1,cex=0.75)
}


## Plots whole island normal non-median trimmed tracks
pdf("8.1UnfiltWholeIslandCrabVisualTracks.pdf")
imgtest <- brick("lowqualpalmyra.tif")
plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
colors = rainbow(length(unique(HourlyMedianDF$CrabNum)))
iter = 0
for (crab in unique(crabs201X$CrabNum)) {
  print(crab)
  crabList = list()
  thisCrabTrax = crabs201X[which(crabs201X$CrabNum == crab),]
  for (date in 1:length(unique(thisCrabTrax$Date))) {
    crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
  }
  testSL = SpatialLines((crabList[1]))
  testSL@lines <- crabList
  proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(testUTMSL,col=colors[iter],pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
  iter = iter+1
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomright")
}
for (island in c("cooper","sand","eastern")) {
  imgtest <- brick(paste0(island,".tif"))
  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  allCrabsInIsland = subset(crabs201X,Island==island)
  colors = rainbow(length(unique(allCrabsInIsland$CrabNum)))
  iter = 0
  for (crab in unique(allCrabsInIsland$CrabNum)) {
    print(crab)
    crabList = list()
    thisCrabTrax = allCrabsInIsland[which(allCrabsInIsland$CrabNum == crab),]
    for (date in 1:length(unique(thisCrabTrax$Date))) {
      crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
    }
    testSL = SpatialLines(crabList[1])
    testSL@lines <- crabList
    proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(testUTMSL,col=colors[iter],pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
    iter = iter + 1
  }
  addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 1,
                lwd = 1, border = "black", cols = c("white", "black"),
                text.col = "black")
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomright")
  if (island=="cooper") {
    legend(title=paste("Island:",island),x="bottomleft",legend=unique(allCrabsInIsland$CrabNum),col=colors,lty=1,cex=0.75)
  } else {
    legend(title=paste("Island:",island),x="topleft",legend=unique(allCrabsInIsland$CrabNum),col=colors,lty=1,cex=0.75)
  }
}
dev.off()




##########################################
##### WHOLE PALMYRA VISUALIATION ########
#########################################

#Plots individual median-obtained crab tracks per island, by day

#imgtest <- brick("lowqualpalmyra.tif")
#plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")

pdf("8.3MDWholeIslandTracksv2.pdf")
img <- brick("palmyra-2016-17x17-ms-ALL-BANDS.tif") #change filename as needed. TIF or IMG images should work fine
names(img) <- c(paste0("T",1:8, coll=""), paste0("B",1:8, coll=""))
plotRGB(img, r=13, g=11, b=10,stretch="hist")

colors = rainbow(length(unique(HourlyMedianDF$CrabNum)))
iter = 0
for (crab in unique(HourlyMedianDF$CrabNum)) {
  print(crab)
  crabList = list()
  thisCrabTrax = HourlyMedianDF[which(HourlyMedianDF$CrabNum == crab),]
  thisCrabTrax$Date = substr(thisCrabTrax$DateTime,1,10)
  for (date in 1:length(unique(thisCrabTrax$Date))) {
    crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
  }
  testSL = SpatialLines((crabList[1]))
  testSL@lines <- crabList
  proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1)
  plot(testUTMSL,col=crabColorsList[[crab]],pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
  iter = iter+1
}
addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                        unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                        style = "bar", bar.cols = c("black", "white"), lwd = 1,
                        linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                        label.col = "black", pos = "bottomright")
dev.off()


pdf("8.2RecoloredMedWholeIslandCrabVisualTracks.pdf",height=10,width=10)
for (island in c("cooper","eastern","sand")) {
  #pdf("8.2CooperRecoloredMedWholeIslandCrabVisualTracks.pdf",height=8,width=10)
  #island = "cooper"
  imgtest <- raster(paste0(island,".tif"))
  image(imgtest, col="black",axes=FALSE)
  #imgtest <- brick(paste0(island,".tif"))
  #plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  allCrabsInIsland = subset(HourlyMedianDF,Island==island)
  #colors = rainbow(length(unique(allCrabsInIsland$CrabNum)))
  #iter = 0
  for (crab in unique(allCrabsInIsland$CrabNum)) {
    print(crab)
    crabList = list()
    thisCrabTrax = allCrabsInIsland[which(allCrabsInIsland$CrabNum == crab),]
    thisCrabTrax$Date = substr(thisCrabTrax$DateTime,1,10)
    for (date in 1:length(unique(thisCrabTrax$Date))) {
      crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
    }
    testSL = SpatialLines(crabList[1])
    testSL@lines <- crabList
    proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(testUTMSL,col=crabColorsList[[crab]],pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
    #iter = iter + 1
  }
  if (island=="cooper") {
    legend(title=paste("Island:",island),x="bottomleft",legend=unique(allCrabsInIsland$CrabNum),col=colors,lty=1,cex=0.75)
  } else {
    legend(title=paste("Island:",island),x="topleft",legend=unique(allCrabsInIsland$CrabNum),col=colors,lty=1,cex=0.75)
  }
  
  addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.5,
                lwd = 1, border = "black", cols = c("white", "black"),
                text.col = "black")
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomright")
  #dev.off()
}
dev.off()
