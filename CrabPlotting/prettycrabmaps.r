pdf("8.1RepresentativeIslandTracks.pdf")

goodCrabList = list()
goodCrabList[["cooper"]] = c(1,7,12)
goodCrabList[["sand"]] = c(1,6,7,9)
goodCrabList[["eastern"]] = c(2,3,6)
for (island in c("cooper","sand","eastern")) {
  imgtest <- brick(paste0(island,".tif"))
  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  allCrabsInIsland = subset(HourlyMedianDF,Island==island)
  colors = rainbow(length(unique(substr(allCrabsInIsland$Date,1,10))))
  usedColors = c()
  usedDates = c()
  # Prints crab tracks (median filtered)
  for (crab in unique(allCrabsInIsland$CrabNum)[goodCrabList[[island]]]) {
    #Prints tracks on top of island
    print(crab)
    crabList = list()
    thisCrabTrax = allCrabsInIsland[which(allCrabsInIsland$CrabNum == crab),]
    thisCrabTrax$Date = substr(thisCrabTrax$DateTime,1,10)
    for (date in 1:length(unique(thisCrabTrax$Date))) {
      crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
    }
    testSL = SpatialLines((crabList[1]))
    testSL@lines <- crabList
    proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(testUTMSL,col=colors[which(unique(substr(allCrabsInIsland$Date,1,10))%in%thisCrabTrax$Date)],pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
    usedColors = c(usedColors,colors[which(unique(substr(allCrabsInIsland$Date,1,10))%in%thisCrabTrax$Date)])
    usedDates = c(usedDates,unique(thisCrabTrax$Date))
  }
  usedColors = sort(usedColors[which(!duplicated(usedColors))])
  usedDates = sort(usedDates[which(!duplicated(usedDates))])
  legend(title=str_to_title(paste(island,"Island")),x="bottomright",legend=usedDates,col=usedColors,lty=1,cex=0.75)
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomleft")
  
  addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 1,
                lwd = 1, border = "black", cols = c("white", "black"),
                text.col = "black")
  # Prints crab KUDs (watertrimmed)
  imgtest <- brick(paste0(island,".tif"))
  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  for (crab in unique(allCrabsInIsland$CrabNum)[goodCrabList[[island]]]) {
    print(crab)
    ### KUD 95 ###
    crabNum = crab
    maskedver = raster::mask(imgtest,crabHRList[[crabNum+kudoffset]])
    maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
    maskedverareas = raster::area(maskedvertransformed)
    plot(maskedver,col="red",add=TRUE,legend=FALSE)
    
    ### KUD 50 ### 
    maskedver = raster::mask(imgtest,crabHRList[[crabNum+kudoffset+fiftyoffset]])
    maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
    maskedverareas = raster::area(maskedvertransformed)
    plot(maskedver,col="yellow",add=TRUE,legend=FALSE)
 }
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomright")
  
  addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 1,
                lwd = 1, border = "black", cols = c("white", "black"),
                text.col = "black")
  if (island=="cooper") {
    legend(title=paste("Island:",str_to_title(island)),x="bottomleft",legend=c("KUD95","KUD50"),fill=c("red","yellow"),lty=1,cex=0.75)
  } else {
    legend(title=paste("Island:",str_to_title(island)),x="topleft",legend=c("KUD95","KUD50"),fill=c("red","yellow"),lty=1,cex=0.75)
  }
}
dev.off()

#Select which crabs to pick
for (crab in c(1:5,25:30)) {
  print(crab)
  #Figures out which island the crab is on
  island = kudmedframe[crab,"Island"]
  imgtest <- brick(paste0(island,".tif"))
  crabNum = kudmedframe[crab,"CrabNum"]
  ### KUD 95 ###

  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
  maskedverareas = raster::area(maskedvertransformed)
  plot(maskedver,col="red",add=TRUE,legend=FALSE)
  legend("topleft",legend=c("Shapefile","Raster"),fill=c("blue","red"),title=paste("Crab No. ",kudmedframe[crab,"CrabNum"]))
}
dev.off()