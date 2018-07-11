library(reshape)
library(adehabitatHS)


# Import smoothed classification image:
classed <- raster("6.26MASKEDPalmyra-RF-classification-v3-5x5-modal.tif") # Took 5x5 MODAL average

# Initialized crab tracking import
newWD = "../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks"
crabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks")
importantValues = c("Date","Time","Latitude","Longitude","Altitude","Speed","Course","Distance","CrabNum","Island")

#Classifies crabs by island
islands = c("sand","cooper","eastern","paradise")
islandList = c()
for (island in islands) {
  islandList[as.integer(substr(crabList[grep(island,crabList)],6,7))] = island
}


#Starts off crab tracking files
crabs2016 = read.csv(paste0(newWD,"/",crabList[1]))[1,]
crabs2016$CrabNum <- NA
crabs2016$Island <- NA
crabs2016 = crabs2016[-1,]

#Imports all crab tracking files and compiles into one big data frame
for (name in crabList) {
  print(name)
  temp = read.csv(paste0(newWD,"/",name));
  #firstDate = temp[1,"Date"]
  #temp = temp[-which(temp$Date == firstDate),]
  #if(nrow(temp)!=0) {
  #print(paste("Crab: ", as.integer(substring(name,6,7))))
  #print(paste("Dates: ", unique(temp$Date)))
  temp$CrabNum = as.integer(substring(name,6,7))
  temp$Island = islandList[as.integer(substring(name,6,7))]
  crabs2016 = rbind(crabs2016,temp[,importantValues]);
  #tempsf <- temp[which(!is.na(temp$Latitude)),]
  #coordinates(tempsf) <- ~Longitude+Latitude
  #proj4string(tempsf)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #shapefile(tempsf, paste0(newWD,"/../rawshapefiles/",substr(name,1,7),"-untrimmed.shp"))
   #}
}

crabs2016 = crabs2016[which(!is.na(crabs2016$Latitude)),]



#Speed filter
crabs2016$m.s.before = NA
crabs2016$secbefore = NA

sfcrabs2016 = crabs2016[0,]
for (crab in unique(crabs2016$CrabNum)) {
  thisCrab = crabs2016[which(crabs2016$CrabNum == crab),]
  timeVec = thisCrab$Date[2]
  class(thisCrab$Date[2])
  dateVec = as.POSIXct(paste(thisCrab$Date,thisCrab$Time),format="%m/%d/%y %H:%M:%S")
  dateVec2 = as.POSIXct(paste(thisCrab$Date,thisCrab$Time),format="%m/%d/%y %H:%M:%S")[-length(dateVec)]
  dateVec2 = c(dateVec2[1],dateVec2)
  thisCrab$secbefore = dateVec - dateVec2
  thisCrab$m.s.before = thisCrab$Distance/as.numeric(thisCrab$secbefore)
  thisCrab = thisCrab[-which(thisCrab$m.s.before > 0.15),]
  sfcrabs2016 = rbind(sfcrabs2016,thisCrab)
}

View(head(sfcrabs2016))


#Begins analyzing island habitat distribution
allLocations = data.frame(matrix(ncol=4,nrow=4))
colnames(allLocations) = c("Cocos","Natives","Scaevola","Sand")
rownames(allLocations) = islands

#Loops through all islands, imports their QGIS-cropped images, and calculates their habitat ratios
islandList <- list()
for (isle in islands) {
  classedIsle = raster(paste0(isle,".tif"))
  #Converts classified habitat spatial image into global coordinates
  r.pts <- rasterToPoints(classedIsle, spatial=TRUE)
  proj4string(r.pts)
  islecoordpts <- spTransform(r.pts, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  islecoordpts@data <- data.frame(islecoordpts@data, long=coordinates(islecoordpts)[,1],
                                  lat=coordinates(islecoordpts)[,2])
  #Removes all duplicates from classified satellite imagery
  islecoordpts@data = islecoordpts@data[!duplicated(islecoordpts@data[,1:3]),]
  islandList[[isle]] <- islecoordpts@data
  colnames(islecoordpts@data) <- c("class","long","lat")
  #View(islecoordpts@data)
  totalavailhab = table(islecoordpts@data[,"class"])
  allLocations[isle,"Cocos"] = totalavailhab[1]
  allLocations[isle,"Natives"] = totalavailhab[2]
  allLocations[isle,"Scaevola"] = totalavailhab[3]
  allLocations[isle,"Sand"] = totalavailhab[4]
}

crabitat16DF = data.frame(matrix(0,ncol=11,nrow=length(unique(sfcrabs2016$CrabNum))))
colnames(crabitat16DF) <- c("CrabNum","Island","NumEntries","TotalCocos","TotalNatives","TotalScaevola","TotalSand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
crabitat16DF$CrabNum <- unique(sfcrabs2016$CrabNum)

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

#:nrow(crabitat16DF)
#For each crab, figure out which island it's on, and compute its habitat preferences/availability
for (crab in 1:nrow(crabitat16DF)) {
  thisCrabTracks = sfcrabs2016[sfcrabs2016$CrabNum==crabitat16DF[crab,"CrabNum"],]
  crabitat16DF[crab,"Island"]= thisCrabTracks[1,"Island"]
  crabitat16DF[crab,"NumEntries"] = nrow(thisCrabTracks)
  crabitat16DF[crab,"AvailCocos"] = allLocations[crabitat16DF[crab,"Island"],"Cocos"]
  crabitat16DF[crab,"AvailNatives"] = allLocations[crabitat16DF[crab,"Island"],"Natives"]
  crabitat16DF[crab,"AvailScaevola"] = allLocations[crabitat16DF[crab,"Island"],"Scaevola"]
  crabitat16DF[crab,"AvailSand"] = allLocations[crabitat16DF[crab,"Island"],"Sand"]
  thisCrabIsland = islandList[[crabitat16DF[crab,"Island"]]]
  thisCrabTracks$isInWater = FALSE;
  for (i in 1:nrow(thisCrabTracks)) {
    #temp = thisCrabIsland[(thisCrabIsland[,"long"]==thisCrabTracks$Longitude[i]&thisCrabIsland[,"lat"]==thisCrabTracks$Latitude[i]),]
    #temp = r.coordpts@data[(r.coordpts@data[,"long"]==thisCrabTracks$Longitude[i]&r.coordpts@data[,"lat"]==thisCrabTracks$Latitude[i]),]
    temp = findClosestPoint(thisCrabTracks$Latitude[i], thisCrabTracks$Longitude[i], thisCrabIsland)
    if (nrow(temp) != 0) {
      for (j in 1:nrow(temp)) {
        if (temp[j,1] == 0) {
          crabitat16DF[crab,"TotalCocos"] = crabitat16DF[crab,"TotalCocos"] + 1
        } else if (temp[j,1] == 1) {
          crabitat16DF[crab,"TotalNatives"] = crabitat16DF[crab,"TotalNatives"] + 1
        } else if (temp[j,1] == 2) {
          crabitat16DF[crab,"TotalScaevola"] = crabitat16DF[crab,"TotalScaevola"] + 1
        } else if (temp[j,1] == 5) {
          crabitat16DF[crab,"TotalSand"] = crabitat16DF[crab,"TotalSand"] + 1
        }
      }
    } 
    else {
      thisCrabTracks$isInWater[i] = TRUE;
    }
  }
  # Code to generate dataset of crab tracks sorted by only those near classified land pixels
  #thisCrabTracks = thisCrabTracks[which(thisCrabTracks$isInWater == FALSE),]
  #write.csv(thisCrabTracks,paste0(newWD,"/waterspeedtrimmed/crab-",crabitat16DF[crab,"CrabNum"],"waterspeed-trimmed.csv"))
}

#Calculates approximate % time spent in each of these habitats in crabs
#crabLocation
#crabLocation[1,"Cocos"]/sum(crabLocation)
#crabLocation[1,"Natives"]/sum(crabLocation)

#Calculates habitat selection 
widesIII(crabitat16DF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")],crabitat16DF[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
crabitat16DF$CocosWI= 0
crabitat16DF$NativesWI= 0
crabitat16DF$ScaevolaWI= 0
crabitat16DF$SandWI= 0
for (crab in 1:nrow(crabitat16DF)) {
  #ADEHabitat really doesn't like it when we have more than one zero in a column
  if (crabitat16DF[crab,"TotalScaevola"]==0) {
    crabitat16DF[crab,"TotalScaevola"]=1
  }
  if(crabitat16DF[crab,"TotalSand"]==0) {
    crabitat16DF[crab,"TotalSand"]=1
  }
  if (crabitat16DF[crab,"TotalScaevola"]==0) {
    crabitat16DF[crab,"TotalScaevola"]=1
  }
  if(crabitat16DF[crab,"TotalSand"]==0) {
    crabitat16DF[crab,"TotalSand"]=1
  }
  ratios = widesIII(crabitat16DF[crab,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")],crabitat16DF[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  crabitat16DF[crab,"CocosWI"] = ratios$wi[1]
  crabitat16DF[crab,"NativesWI"] = ratios$wi[2]
  crabitat16DF[crab,"ScaevolaWI"] = ratios$wi[3]
  crabitat16DF[crab,"SandWI"] = ratios$wi[4]
}
mean(crabitat16DF$CocosWI)
mean(crabitat16DF$NativesWI)
mean(crabitat16DF$ScaevolaWI)
mean(crabitat16DF$SandWI)

var(crabitat16DF$CocosWI)
var(crabitat16DF$NativesWI)
var(crabitat16DF$ScaevolaWI)
var(crabitat16DF$SandWI)

#write.csv(crabitat16DF,"crabanalysis2016v1.csv")