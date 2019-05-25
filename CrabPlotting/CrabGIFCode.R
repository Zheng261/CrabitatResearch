### CRAB TRACKING GIF ####
#### Starts representative crab plots ####
goodCrabList = list()
goodCrabList[["cooper"]] = c(10,119,131)
goodCrabList[["sand"]] = c(117,120,115)
goodCrabList[["eastern"]] = c(15,126,128)
for (island in c("cooper","sand","eastern")) {
  if (island=="cooper") {
    pdf("8.28CooperRepresentativeCrabPlots.pdf",height=8,width=10)
  } else if (island=="sand") {
    dev.off()
    pdf("8.28SandEasternRepresentativeCrabPlots.pdf",height=10,width=10)
  }
  imgtest <- brick(paste0(island,".tif"))
  allCrabsInIsland = subset(HourlyMedianDF,CrabNum%in%goodCrabList[[island]])
  maxDaysTracked = 0
  for (crab in goodCrabList[[island]]) {
    daysTracked = length(unique(substr(subset(allCrabsInIsland,CrabNum==crab)$Date,1,10)))
    maxDaysTracked = max(maxDaysTracked,daysTracked)
  }
  colors = rainbow(maxDaysTracked)
  for (day in c(1:maxDaysTracked)) {
    image(imgtest, col="black",axes=FALSE,main="",xlab="",ylab="")
    # Prints crab tracks (median filtered)
    for (crab in goodCrabList[[island]]) {
      #Prints tracks on top of island
      print(crab)
      crabList = list()
      thisCrabTrax = allCrabsInIsland[which(allCrabsInIsland$CrabNum == crab),]
      thisCrabTrax$Date = substr(thisCrabTrax$DateTime,1,10)
      #### Iterates through each day until the max day
      for (date in 1:min(length(unique(thisCrabTrax$Date)),day)) {
        crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
      }
      testSL = SpatialLines((crabList[1]))
      testSL@lines <- crabList
      proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
      plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
    }
    legendDates = paste("Day",c(1:length(colors)))
    legend(title=str_to_title(paste(island,"Island")),x="bottomright",legend=legendDates[1:day],col=colors[1:day],lty=1,cex=1.5)
  }
}
dev.off()
