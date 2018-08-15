crabs201X <- read.csv("crabs201XAll.csv")
crabs201X[which(crabs201X$CrabNum==1),] = crabs201X[which(crabs201X$CrabNum==3),]
warnings()

crab = 133
head(crabs201X[which(crabs201X$CrabNum==crab),])
taggedMetaData17[which(taggedMetaData17$Animal..==crab-100),"Time.tagged"]

crabList = list()
thisCrabTrax = crabs201X[which(crabs201X$CrabNum == crab),]
island = thisCrabTrax[1,"Island"]
imgtest <- brick(paste0(island,".tif"))
#proj4string(imgtest)
plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
for (date in 1:length(unique(thisCrabTrax$Date))) {
  morp = thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")]
  crabList[[date]] = Lines(list(Line(morp)),ID=date)
}
colors = rainbow(length(unique(thisCrabTrax$Date)))
testSL = SpatialLines((crabList[1]))
testSL@lines <- crabList
proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1)
plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
legend(title=paste("Crab:",crab),x=(imgtest@extent[1]+imgtest@extent[2])/2,y=(imgtest@extent[3]+imgtest@extent[4])/2,legend=unique(thisCrabTrax$Date),col=colors,lty=1,cex=0.5)


