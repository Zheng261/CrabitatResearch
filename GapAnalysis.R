#for (crab in unique(crabs201X$CrabNum)) {
#thisCrab = crabs201X[which(crabs201X$CrabNum == crab),]
#timeVec = thisCrab$Date[2]
#dateVec = as.POSIXct(paste(thisCrab$Date,thisCrab$Time),format="%m/%d/%y %H:%M:%S")
#dateVec2 = c(dateVec[1],dateVec[-length(dateVec)])
#plot(dateVec,thisCrab$Latitude) 
#plot(dateVec,thisCrab$Longitude)
#plot(dateVec,thisCrab$Distance)
#}

gaps = crabs201X[c(which(crabs201X$Elapsed > 5000),which(crabs201X$Elapsed > 5000)-1),]
dateVecGaps = as.POSIXct(paste(gaps$Date,gaps$Time),format="%m/%d/%y %H:%M:%S")

#plot(dateVecGaps,gaps$Latitude) 
#plot(dateVec,crabs201X$Latitude)
#plot(dateVecGaps,gaps$Longitude) 
#plot(dateVec,crabs201X$Longitude)

#Sets up crab location data frame
gapDF = data.frame(matrix(0,ncol=11,nrow=length(unique(gaps$CrabNum))))
colnames(gapDF) <- c("CrabNum","Island","NumEntries","TotalCocos","TotalNatives","TotalScaevola","TotalSand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
gapDF$CrabNum <- unique(gaps$CrabNum)


findClosestPoint <- function(x,y,df) {
  dfclose = df[which(abs(df$lat-x) < 0.00001 & abs(df$long-y) < 0.00001),]
  minDist = 100;
  minRow = -1;
  if (nrow(dfclose)==0) {
    return(dfclose)
  }
  for(row in 1:nrow(dfclose)) {
    dist = sqrt((df[row,"lat"] - x)^2 + (df[row,"long"] - y)^2)
    if (dist < minDist) {
      minRow = row
      minDist = dist
    }
  }
  return(dfclose[minRow,])
}

#for (crab in 1:nrow(gapDF)) {
thisCrabTracks = gaps[gaps$CrabNum==gapDF[crab,"CrabNum"],]
gapDF[crab,"Island"]= thisCrabTracks[1,"Island"]
gapDF[crab,"NumEntries"] = nrow(thisCrabTracks)
gapDF[crab,"AvailCocos"] = allLocations[gapDF[crab,"Island"],"Cocos"]
gapDF[crab,"AvailNatives"] = allLocations[gapDF[crab,"Island"],"Natives"]
gapDF[crab,"AvailScaevola"] = allLocations[gapDF[crab,"Island"],"Scaevola"]
gapDF[crab,"AvailSand"] = allLocations[gapDF[crab,"Island"],"Sand"]
thisCrabIsland = islandCoordsList[[gapDF[crab,"Island"]]]
thisCrabTracks$isInWater = FALSE;
for (i in 1:nrow(thisCrabTracks)) {
  #temp = thisCrabIsland[(thisCrabIsland[,"long"]==thisCrabTracks$Longitude[i]&thisCrabIsland[,"lat"]==thisCrabTracks$Latitude[i]),]
  #temp = r.coordpts@data[(r.coordpts@data[,"long"]==thisCrabTracks$Longitude[i]&r.coordpts@data[,"lat"]==thisCrabTracks$Latitude[i]),]
  temp = findClosestPoint(thisCrabTracks$Latitude[i], thisCrabTracks$Longitude[i], thisCrabIsland)
  if (nrow(temp) != 0) {
    for (j in 1:nrow(temp)) {
      if (temp[j,1] == 0) {
        gapDF[crab,"TotalCocos"] = gapDF[crab,"TotalCocos"] + 1
      } else if (temp[j,1] == 1) {
        gapDF[crab,"TotalNatives"] = gapDF[crab,"TotalNatives"] + 1
      } else if (temp[j,1] == 2) {
        gapDF[crab,"TotalScaevola"] = gapDF[crab,"TotalScaevola"] + 1
      } else if (temp[j,1] == 5) {
        gapDF[crab,"TotalSand"] = gapDF[crab,"TotalSand"] + 1
      }
    }
  } 
}

gapDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")] = (gapDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")]+0.01)/rowSums(gapDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")])

coordinates(gaps) <- ~Longitude+Latitude
proj4string(gaps)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
gaps@data = gaps@data[,1:9]
shapefile(gaps, "gapsVisualTenHours.shp")
widesIII(gapDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")],gapDF[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])

