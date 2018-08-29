#########################################
#### Home range sufficiency analysis ####
#########################################
#Tracks for 12 days, 6 hour intervals

#KUDmedframe is a data frame with all the crab numbers and islands, which is all we use it for
kernel50Area = data.frame(matrix(nrow=nrow(kudmedframe),ncol=48))
kernel50Area[,] = 0 
colnames(kernel50Area) = seq(6,6*48,6)
kernel50Area$CrabNum = kudmedframe$CrabNum
kernel50Area$Island = kudmedframe$Island
kernel95Area = kernel50Area

### Determine which crabs still need to be analyzed (this code takes awhile to run, so I ran it at separate times over the
### course of a few days and constantly saved in fear of a crash)
crabsToDo = which(rowSums(kernel95Area[1:48])==0)

#Iterate through remaining crabs
for (crab in kudmedframe$CrabNum[crabsToDo]) {
  print(crab)
  ### HourlyMedianDF is a data frame with all of the median filtered tracks, labeled by crab number, island, and datetime
  thisCrabTrax = HourlyMedianDF[which(HourlyMedianDF$CrabNum == crab),]
  thisDateVec = as.POSIXct(thisCrabTrax$DateTime,format="%Y-%m-%d %H:%M:%S")
  origTime = thisDateVec[1]
  elapsedTime = thisDateVec[length(thisDateVec)] - origTime
  # Calculate the elapsed time between the last track and the first track, in hours (rounded down).   
  numHoursElapsed = floor(as.numeric(elapsedTime,units="hours"))
  # Calculate the minimum number of columns we need. 
  numColsUsed = ceil(numHoursElapsed/6)
  
  #Finds lower and upper bound of elapsed time, gets all tracks within range
  for (hours in 1:numColsUsed) {
    ### Every 6 hours, calculate KUD ###
    UBHours = (hours)*6
    print(paste("Hours elapsed: ", UBHours))
    diff = as.numeric(thisDateVec - origTime,units="hours")
    relevantTrax = thisCrabTrax[diff <= UBHours,]
    island = kudmedframe[which(kudmedframe$CrabNum==crab),"Island"]
    imgtest <- brick(paste0(island,".tif"))
    
    ### Makes sure there are enough crab entries - if not, then just leave it as zero
    if (nrow(relevantTrax)>5) {
      thisCrabTrax.sp = SpatialPoints(coords = relevantTrax[,c("Longitude","Latitude")], proj4string = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')) # convert to SpatialPoints object
      thisCrabTrax.spt <- spTransform(thisCrabTrax.sp,CRS("+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      resolution = 1
      ## Construct custom grid using extent and resolution of crab tracks
      x <- seq(extent(thisCrabTrax.spt)[1]-300, extent(thisCrabTrax.spt)[2]+300,by=resolution) # where resolution is the pixel size you desire 
      y <- seq(extent(thisCrabTrax.spt)[3]-300, extent(thisCrabTrax.spt)[4]+300,by=resolution)
      xy <- expand.grid(x=x,y=y)
      coordinates(xy) <- ~x+y
      gridded(xy) <- TRUE
      
      #Constructs kernel utilization density of crab tracks
      kud <- kernelUD(thisCrabTrax.spt, grid=xy, h="href") # NOTE: look into changing grid value. Currently I think it's too big because the estimated homerange (imageted below) looks too big. Either grid or h needs tweaking.
      #KUD 95
      ver95 <- getverticeshr(kud, 95)
      maskedver = raster::mask(imgtest,ver95)
      maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
      maskedverareas = raster::area(maskedvertransformed)
      # Grabs the sum of areas for all raster cells of the mask that are not NA
      kernel95Area[which(kernel95Area$CrabNum == crab),hours] = sum(maskedverareas[!is.na(maskedvertransformed[,])])
    }
  }
}
#write.csv(kernel95Area,"7.29kernel95area")