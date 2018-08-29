install.packages("adehabitatHS")
library(adehabitatHS)

# Import smoothed classification image:
classed <- raster("6.26MASKEDPalmyra-RF-classification-v3-5x5-modal.tif") # Took 5x5 MODAL average

# Initialized crab tracking import
newWD = "../Palmyra Crab Research/Crab tagging/crab tracks 2017/trimmed crab tracks"
crabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2017/trimmed crab tracks")
importantValues = c("Date","Time","Latitude","Longitude","Altitude","Speed","Course","Distance","CrabNum","Island","m.s.before")
crabList = crabList[which(crabList!="waterspeedtrimmed")]

#Classifies crabs by island
islands = c("sand","cooper","eastern","paradise")
islandList = c()
otherCrabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2017/2017 crab tracks CSV")
for (island in islands) {
  islandList[as.integer(substr(otherCrabList[grep(island,otherCrabList)],22,23))] = island
}



#Starts off crab tracking files
crabs2017 = read.csv(paste0(newWD,"/crab-01-trimmed.csv"))[1,]
crabs2017$CrabNum <- NA
crabs2017$Island <- NA
crabs2017 = crabs2017[-1,]

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
  crabs2017 = rbind(crabs2017,temp[,importantValues]);
  #}
}

crabs2017 = crabs2017[which(!is.na(crabs2017$Latitude)),]

#Speed filter
crabs2017 = crabs2017[which(crabs2017$m.s.before <= 0.15),]
View(head(crabs2017))

#Converts classified habitate data into coordinate points in data frame form
#r.pts <- rasterToPoints(classed, spatial=TRUE)
#proj4string(r.pts)
#r.coordpts <- spTransform(r.pts, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#r.coordpts@data <- data.frame(r.coordpts@data, long=coordinates(r.coordpts)[,1],lat=coordinates(r.coordpts)[,2])

#colnames(r.coordpts@data) <- c("class","long","lat")

#TODO: Find a less hacky way of doing this. Plot data afterwards
#Rounds to 5 digits in both crab tracks and satellite imagery to ensure that crab and satellite imagery data can be matched together

#Removes all duplicates from classified satellite imagery
#dupes = duplicated(r.coordpts@data[,1:3])
#r.coordpts@data = r.coordpts@data[!dupes,]

#Begins analyzing island habitat distribution
allLocations = data.frame(matrix(ncol=4,nrow=4))
colnames(allLocations) = c("Cocos","Natives","Scaevola","Sand")
rownames(allLocations) = islands

#Loops through all islands, imports their QGIS-cropped images, and calculates their habitat ratios
islandCoordsList <- list()
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
  islandCoordsList[[isle]] <- islecoordpts@data
  colnames(islecoordpts@data) <- c("class","long","lat")
  #View(islecoordpts@data)
  totalavailhab = table(islecoordpts@data[,"class"])
  allLocations[isle,"Cocos"] = totalavailhab[1]
  allLocations[isle,"Natives"] = totalavailhab[2]
  allLocations[isle,"Scaevola"] = totalavailhab[3]
  allLocations[isle,"Sand"] = totalavailhab[4]
}


#Calculates total coco and native tree percents
#totalavailhab = table(r.coordpts@data[,1])
#cocopercent =totalavailhab[1]/sum(totalavailhab)
#nativepercent = totalavailhab[2]/sum(totalavailhab)

#Sets up crab location data frame
crabitatDF = data.frame(matrix(0,ncol=11,nrow=length(unique(crabs2017$CrabNum))))
colnames(crabitatDF) <- c("CrabNum","Island","NumEntries","TotalCocos","TotalNatives","TotalScaevola","TotalSand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
crabitatDF$CrabNum <- unique(crabs2017$CrabNum)

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

#:nrow(crabitatDF)
#For each crab, figure out which island it's on, and compute its habitat preferences/availability
for (crab in 1:nrow(crabitatDF)) {
  thisCrabTracks = crabs2017[crabs2017$CrabNum==crabitatDF[crab,"CrabNum"],]
  crabitatDF[crab,"Island"]= thisCrabTracks[1,"Island"]
  crabitatDF[crab,"NumEntries"] = nrow(thisCrabTracks)
  crabitatDF[crab,"AvailCocos"] = allLocations[crabitatDF[crab,"Island"],"Cocos"]
  crabitatDF[crab,"AvailNatives"] = allLocations[crabitatDF[crab,"Island"],"Natives"]
  crabitatDF[crab,"AvailScaevola"] = allLocations[crabitatDF[crab,"Island"],"Scaevola"]
  crabitatDF[crab,"AvailSand"] = allLocations[crabitatDF[crab,"Island"],"Sand"]
  thisCrabIsland = islandCoordsList[[crabitatDF[crab,"Island"]]]
  thisCrabTracks$isInWater = FALSE;
  for (i in 1:nrow(thisCrabTracks)) {
    #temp = thisCrabIsland[(thisCrabIsland[,"long"]==thisCrabTracks$Longitude[i]&thisCrabIsland[,"lat"]==thisCrabTracks$Latitude[i]),]
    #temp = r.coordpts@data[(r.coordpts@data[,"long"]==thisCrabTracks$Longitude[i]&r.coordpts@data[,"lat"]==thisCrabTracks$Latitude[i]),]
    temp = findClosestPoint(thisCrabTracks$Latitude[i], thisCrabTracks$Longitude[i], thisCrabIsland)
    if (nrow(temp) != 0) {
      for (j in 1:nrow(temp)) {
        if (temp[j,1] == 0) {
          crabitatDF[crab,"TotalCocos"] = crabitatDF[crab,"TotalCocos"] + 1
        } else if (temp[j,1] == 1) {
          crabitatDF[crab,"TotalNatives"] = crabitatDF[crab,"TotalNatives"] + 1
        } else if (temp[j,1] == 2) {
          crabitatDF[crab,"TotalScaevola"] = crabitatDF[crab,"TotalScaevola"] + 1
        } else if (temp[j,1] == 5) {
          crabitatDF[crab,"TotalSand"] = crabitatDF[crab,"TotalSand"] + 1
        }
      }
    } 
    #else {
    #  thisCrabTracks$isInWater[i] = TRUE;
    #}
  }
  # Code to generate dataset of crab tracks sorted by only those near classified land pixels
  #thisCrabTracks = thisCrabTracks[which(thisCrabTracks$isInWater == FALSE),]
  #write.csv(thisCrabTracks,paste0(newWD,"/waterspeedtrimmed/crab-",crabitatDF[crab,"CrabNum"],"waterspeed-trimmed.csv"))
}


crabitatDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")] = crabitatDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")]/rowSums(crabitatDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")])

#Calculates habitat selection 
widesIII(crabitatDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")],crabitatDF[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])

crabitatDF$CocosWI= 0
crabitatDF$NativesWI= 0
crabitatDF$ScaevolaWI= 0
crabitatDF$SandWI= 0
for (crab in 1:nrow(crabitatDF)) {
  ratios = widesIII(crabitatDF[crab,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")],crabitatDF[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  crabitatDF[crab,"CocosWI"] = ratios$wi[1]
  crabitatDF[crab,"NativesWI"] = ratios$wi[2]
  crabitatDF[crab,"ScaevolaWI"] = ratios$wi[3]
  crabitatDF[crab,"SandWI"] = ratios$wi[4]
}

#write.csv(crabitatDF,"crabanalysisv3.csv")
crabitatDF = read.csv("crabanalysisv3.csv")

#Calculates the selection ratio of these crabs
selectionRatioCoco = crabLocation[1,"Cocos"]/(sum(crabLocation)*cocopercent)
selectionRatioNative = crabLocation[1,"Natives"]/(sum(crabLocation)*nativepercent)
selectionRatioBush = crabLocation[1,"Scaevola"]/(sum(crabLocation)*allLocations[3]/sum(allLocations))
selectionRatioSand = crabLocation[1,"Sand"]/(sum(crabLocation)*allLocations[4]/sum(allLocations))

#crabitatDF = crabitatDF[-11,]
mean(crabitatDF$CocosWI)
mean(crabitatDF$NativesWI)
range(crabitatDF$CocosWI)
range(crabitatDF$NativesWI)
var(crabitatDF$CocosWI)
var(crabitatDF$NativesWI)
mean(crabitatDF$ScaevolaWI)
mean(crabitatDF$SandWI)
range(crabitatDF$ScaevolaWI)
range(crabitatDF$SandWI)
var(crabitatDF$ScaevolaWI)
var(crabitatDF$SandWI)
for (isle in islands) {
  print(paste0(isle," Cocos: ",mean(crabitatDF[which(crabitatDF$Island==isle),"CocosWI"])))
  print(paste0(isle," Natives: ",mean(crabitatDF[which(crabitatDF$Island==isle),"NativesWI"])))
  print(paste0(isle," Scaevola: ",mean(crabitatDF[which(crabitatDF$Island==isle),"ScaevolaWI"])))
  print(paste0(isle," Sand: ",mean(crabitatDF[which(crabitatDF$Island==isle),"SandWI"])))
}

#totalPixels = sum(crabitatDF$TotalCocos) +sum(crabitatDF$TotalNatives)+sum(crabitatDF$TotalSand)+sum(crabitatDF$TotalScaevola)
#sum(crabitatDF$TotalCocos)/totalPixels
#sum(crabitatDF$TotalNatives)/totalPixels

#Visualization (WARNING: BAD)
colnames(r.coordpts@data) <- c("Class","long","lat")
classedGraph <- ggplot(data=r.coordpts@data,aes(x=long,y=lat,color=as.factor(Class))) + geom_point()
crabGraph <- ggplot(data=crabs2017[(crabs2017$Longitude < -162.06) & (crabs2017$Longitude > -162.10) & (crabs2017$Latitude > 5.871) & (crabs2017$Latitude < 5.894),],aes(x=Longitude,y=Latitude)) + geom_point()
pdf("crabGraphOverlay.pdf")
classedGraph
crabGraph
dev.off()

#Code for speed filtering
for (name in files) {
  temp = read.csv(paste0(newWD,"/",name));
  temp$CrabNum = as.integer(substring(name,6,7))
  temp$Island = islandList[as.integer(substring(name,6,7))]
  temp[1,"m.s.before"] = 0
  temp = temp[-which(temp$m.s.before > 0.15),]
  tempsf <- temp[which(!is.na(temp$Latitude)),]
  coordinates(tempsf) <- ~Longitude+Latitude
  proj4string(tempsf)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  shapefile(tempsf, paste0(newWD,"/../speed-filtered shapefiles/",substr(name,1,7),"-speedtrimmed.shp"))
  #crabs2017 = rbind(crabs2017,temp[,importantValues]);
}

#Compares my speed filtered files with Mike's
shapefiledir = "../Palmyra Crab Research/Crab tagging/crab tracks 2017/mikespeedfilteredshapefiles/2017-crab07-speed.shp"
shapefile = st_read(shapefiledir)
sfdf = fortify(shapefile)
sfdf$Latitude = round(sfdf$Latitude, digits = 5)
sfdf$Latitude
crabs2017[which(crabs2017$CrabNum==7),]$Latitude

thisCrabTracks <- thisCrabTracks[,c("Longitude","Latitude","Classified")]
coordinates(thisCrabTracks) <- ~Longitude+Latitude
proj4string(thisCrabTracks)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
shapefile(thisCrabTracks, "Crab1TracksClassified.shp")




