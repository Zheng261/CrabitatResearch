#########################################
###########Crab KUD trimming#############
#########################################
crabHRList <-readRDS("7.27crabHRdata.rds")
DFAreas = data.frame(matrix(ncol=4,nrow=length(kudmedframe$CrabNum)))
colnames(DFAreas) <- c("CrabNum","Island","KUD95Area","KUD50Area")
DFAreas$CrabNum <- kudmedframe$CrabNum
DFAreas$Island <- kudmedframe$Island
kudoffset = 1000
fiftyoffset = 500
crabMedRasterList <- list()
crabMedBorderRasterList <- list()
crabColorsList <- list()
k95meltRO = k95melt[order(k95melt$variable,decreasing=TRUE),]
k95meltRO = k95meltRO[!duplicated(k95meltRO$CrabNum),]
k95meltRO = k95meltRO[order(k95meltRO$value,decreasing=TRUE),]


edge_detection<- function(x,y,xcutoff=15,ycutoff=15) {
  candidates = df[which(abs(df[,'x'] - x) < xcutoff & abs(df[,'y'] - y) < ycutoff),]
  
  # Get everything to the left
  left = nrow(candidates[which(candidates[,'x'] > x & abs(candidates[,'y']-y)<0.1),])
  
  # Get everything to the right
  right = nrow(candidates[which(candidates[,'x'] < x & abs(candidates[,'y']-y)<0.1),])
  
  # Get everything above 
  above = nrow(candidates[which(candidates[,'y'] < y & abs(candidates[,'x']-x)<0.1),])
  
  # Get everything below
  below = nrow(candidates[which(candidates[,'y'] > y & abs(candidates[,'x']-x)<0.1),])
  
  return (left < 20 | right < 20 | above < 20 | below < 20)
}


### PLOTS KUDs with experimental border control technology ####
pdf("8.6CooperCrabKUDBorders.pdf",width=10,height=10)
for (island in c("cooper","eastern","sand")) {
  imgtest <- raster(paste0(island,".tif"))
  image(imgtest, col="black",axes=FALSE)
  allCrabsInIsland = subset(k95meltRO,Island==island)
  for (crab in unique(allCrabsInIsland$CrabNum)) {
    print(crab)
    rastTest <- crabMedBorderRasterList[[crab]]
    image(rastTest,col=crabColorsList[[crab]],add=TRUE)
  }
 
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomright")
  
  addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.5,
                lwd = 1, border = "black", cols = c("white", "black"),
                text.col = "black")
  legend(title=str_to_title(island),x="topleft",legend=unique(allCrabsInIsland$CrabNum),col= unlist(crabColorsList[unique(allCrabsInIsland$CrabNum)]),lty=1,cex=1)
}
dev.off()
  
  
  
  #pdf("8.3CooperCrabRasterOverlapping.pdf",width=10,height=8)
  imgtest <- raster(paste0(island,".tif"))
  image(imgtest, col="black",axes=FALSE)
  allCrabsInIsland = subset(k95meltRO,Island==island)
  #iter = 1
  #for (crabNo in unique(allCrabsInIsland$CrabNum)) {
    #crabColorsList[[crabNo]] <- colors[iter]
    #iter = iter + 1
  #}
  for (crab in unique(allCrabsInIsland$CrabNum)) {
    print(crab)
    maskedver = crabMedRasterList[[crab+kudoffset]] 
    #sum(!is.na(maskedver)[,])
    #maskedver = raster::mask(imgtest,crabHRList[[crab+kudoffset]])
    #sfile <- crabHRList[[crab+kudoffset]]
    #plot(sfile,col="red",add=TRUE)
    #crabMedRasterList[[crab+kudoffset]] <- maskedver
    #plot(maskedver,col=crabColorsList[[crab]],add=TRUE)
    df <- data.frame(rasterToPoints(maskedver))
    toKeep = mapply(edge_detection,x=df$x,y=df$y)
    rastTest <- rasterFromXYZ(df[which(toKeep),])
    proj4string(rastTest) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
    image(rastTest,col=crabColorsList[[crab]],add=TRUE)
    crabMedBorderRasterList[[crab]] <- rastTest
    #image(maskedver,col=crabColorsList[[crab]],add=TRUE)
  }
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                            style = "bar", bar.cols = c("black", "white"), lwd = 1,
                            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                            label.col = "black", pos = "bottomright")
    
  addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.5,
                  lwd = 1, border = "black", cols = c("white", "black"),
                  text.col = "black")
}
dev.off()
#saveRDS(crabMedBorderRasterList,file="8.3KUDborderLONGTIME.RDS")
#saveRDS(crabMedRasterList,file="8.3KUDALLFULLRASTERS.RDS")
#saveRDS(crabColorsList,file="8.2crabColorsByIsland.RDS")

#### Just verifying code works, nothing to see here #######
imgtest <- brick("lowqualpalmyra.tif")
plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
for (crab in c(1:5,25:30)) {
    crab = 2
    print(crab)
    #Figures out which island the crab is on
    #island = kudmedframe[crab,"Island"]
    #imgtest <- brick(paste0(island,".tif"))
    crabNum = kudmedframe[crab,"CrabNum"]
    ### KUD 95 ###
    maskedver = crabMedRasterList[[crabNum+kudoffset]] 
    
    df <- data.frame(rasterToPoints(maskedver))
    toKeep = mapply(edge_detection,x=df$x,y=df$y)
    rastTest <- rasterFromXYZ(df[which(toKeep),])
    proj4string(rastTest) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
    plot(rastTest,col="red")
    
    #plot(crabHRList[[crab+kudoffset]],border="red",lwd=4)
    maskedver = raster::mask(imgtest,crabHRList[[crab+kudoffset+fiftyoffset]])
    maskedver = raster(maskedver,layer=1)
    b <- boundaries(maskedver,type='inner',asNA=TRUE)
    crabMedRasterList[[crabNum+kudoffset+fiftyoffset]] <- maskedver
    image(b,col="red",alpha=0.6,add=TRUE,legend=FALSE)
    
    legend("topright",legend=c("95% KUD (Home range)","50% KUD (Core area)"),fill=c("blue","red"),title=("All Crab Home Range and Core Areas"))
}
prettymapr::addscalebar(imageunit = NULL, imageepsg = "32603", widthhint = 0.25,
                        unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                        style = "bar", bar.cols = c("black", "white"), lwd = 1,
                        linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                        label.col = "black", pos = "bottomleft")

addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 1,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
dev.off()
saveRDS(crabMedRasterList,file="8.2crabWaterTrimmedHRCoreArea.RDS")

pdf("7.27CrabLandTrimmedKUD.pdf")
for (crab in 1:nrow(kudmedframe)) {
  print(crab)
  #Figures out which island the crab is on
  island = kudmedframe[crab,"Island"]
  imgtest <- brick(paste0(island,".tif"))
  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  crabNum = kudmedframe[crab,"CrabNum"]
  ### KUD 95 ###
  maskedver = raster::mask(imgtest,crabHRList[[crabNum+kudoffset]])
  maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
  maskedverareas = raster::area(maskedvertransformed)
  image(maskedver,col="red",add=TRUE,legend=FALSE)
  DFAreas[crab,"KUD95Area"] =sum(maskedverareas[!is.na(maskedvertransformed[,])])
  ### KUD 50 ###
  maskedver = raster::mask(imgtest,crabHRList[[crabNum+kudoffset+fiftyoffset]])
  maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
  maskedverareas = raster::area(maskedvertransformed)
  image(maskedver,col="yellow",add=TRUE,legend=FALSE)
  DFAreas[crab,"KUD50Area"] = sum(maskedverareas[!is.na(maskedvertransformed[,])])
  legend("topleft",legend=c("95% KUD","50% KUD"),fill=c("red","yellow"),title=paste("Crab No. ",kudmedframe[crab,"CrabNum"]))
}

dev.off()
write.csv(DFAreas,"7.27KUDAreas.csv")
DFAreas <- read.csv("7.27KUDAreas.csv")


#########################################
#### Home range sufficiency analysis ####
#########################################
#Tracks for 12 days, 6 hour intervals
kernel50Area = data.frame(matrix(nrow=nrow(kudmedframe),ncol=48))
kernel50Area[,] = 0 
colnames(kernel50Area) = seq(6,6*48,6)
kernel50Area$CrabNum = kudmedframe$CrabNum
kernel50Area$Island = kudmedframe$Island
kernel95Area = kernel50Area

crabsToDo = which(rowSums(kernel95Area[1:48])==0)
for (crab in kudmedframe$CrabNum[crabsToDo]) {
  print(crab)
  thisCrabTrax = HourlyMedianDF[which(HourlyMedianDF$CrabNum == crab),]
  thisDateVec = as.POSIXct(thisCrabTrax$DateTime,format="%Y-%m-%d %H:%M:%S")
  origTime = thisDateVec[1]
  elapsedTime = thisDateVec[length(thisDateVec)] - origTime
  numHoursElapsed = floor(as.numeric(elapsedTime,units="hours"))
  numColsUsed = ceil(numHoursElapsed/6)
  
  #Finds lower and upper bound of elapsed time, gets all tracks within range
  for (hours in 1:numColsUsed) {
    UBHours = (hours)*6
    print(paste("Hours elapsed: ", UBHours))
    diff = as.numeric(thisDateVec - origTime,units="hours")
    relevantTrax = thisCrabTrax[diff <= UBHours,]
    island = kudmedframe[which(kudmedframe$CrabNum==crab),"Island"]
    imgtest <- brick(paste0(island,".tif"))
    
    #Makes sure there are enough crab entries to form a data frame - if not, then just put it as zero
    if (nrow(relevantTrax)>5) {
      thisCrabTrax.sp = SpatialPoints(coords = relevantTrax[,c("Longitude","Latitude")], proj4string = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')) # convert to SpatialPoints object
      thisCrabTrax.spt <- spTransform(thisCrabTrax.sp,CRS("+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      resolution = 1
      x <- seq(extent(thisCrabTrax.spt)[1]-300, extent(thisCrabTrax.spt)[2]+300,by=resolution) # where resolution is the pixel size you desire 
      y <- seq(extent(thisCrabTrax.spt)[3]-300, extent(thisCrabTrax.spt)[4]+300,by=resolution)
      xy <- expand.grid(x=x,y=y)
      coordinates(xy) <- ~x+y
      gridded(xy) <- TRUE
      
      #Constructs kernel utilization density of crab tracks
      kud <- kernelUD(thisCrabTrax.spt, grid=xy, h="href") # NOTE: look into changing grid value. Currently I think it's too big because the estimated homerange (imageted below) looks too big. Either grid or h needs tweaking.
      #KUD 95, works because this makes sense
      ver95 <- getverticeshr(kud, 95)
      maskedver = raster::mask(imgtest,ver95)
      maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
      maskedverareas = raster::area(maskedvertransformed)
      kernel95Area[which(kernel95Area$CrabNum == crab),hours] = sum(maskedverareas[!is.na(maskedvertransformed[,])])
    }
  }
}

#write.csv(kernel50Area,"7.25kernel50area")
#write.csv(kernel95Area,"7.29kernel95area")

#kernel50Area <- read.csv("7.25kernel50area")
#kernel95Area <- read.csv("7.29kernel95area")

kernel50Area = kernel50Area[,which(colnames(kernel50Area)!="X")]
kernel95Area = kernel95Area[,which(colnames(kernel95Area)!="X")]
colnames(kernel50Area)[1:48] = substr(colnames(kernel50Area)[1:48],2,1000)
colnames(kernel95Area)[1:48] = substr(colnames(kernel95Area)[1:48],2,1000)
k50melt <- melt(kernel50Area,id=c("Island","CrabNum"))
k95melt <- melt(kernel95Area,id=c("Island","CrabNum"))
k50melt$variable = as.numeric(k50melt$variable)*6
k95melt$variable = as.numeric(k95melt$variable)*6
k50melt = subset(k50melt,value>0)
k95melt = subset(k95melt,value>0)

ggimage(k50melt,aes(x=variable,y=value,color=as.factor(CrabNum))) + geom_line() + coord_cartesian(xlim=c(0,200)) + ggtitle("Area of 50% KUD over time, by crab") + xlab("Time Elapsed") + ylab("Area (km?)")
ggimage(k95melt,aes(x=variable,y=value,color=as.factor(CrabNum))) + geom_line() + coord_cartesian(xlim=c(0,200)) + ggtitle("Area of 95% KUD over time, by crab") + xlab("Time Elapsed") + ylab("Area (km?)")
ggimage(k50melt,aes(x=variable,y=value,color=as.factor(CrabNum))) + geom_line() + coord_cartesian(xlim=c(0,200),ylim=c(0,1))
ggimage(k95melt,aes(x=variable,y=value,color=as.factor(CrabNum))) + geom_line() + coord_cartesian(xlim=c(0,200),ylim=c(0,1))




#### KUD 50, currently not doing
ver50 <- getverticeshr(kud, 50)
maskedver = raster::mask(imgtest,ver50)
maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
maskedverareas = raster::area(maskedvertransformed)
kernel50Area[which(kernel50Area$CrabNum == crab),hours] = sum(maskedverareas[!is.na(maskedvertransformed[,])])


#0.0296921 0.0296921 0.02209978
#2.9741  2.9741  2.2117

b <- boundaries(maskedver,type='inner')
crabMedRasterList[[crabNum+kudoffset]] <- maskedver
image(b,col="yellow",add=TRUE,legend=FALSE)
maskedver = raster::mask(imgtest,crabHRList[[crabNum+kudoffset+fiftyoffset]])
maskedver = raster(maskedver,layer=1)
