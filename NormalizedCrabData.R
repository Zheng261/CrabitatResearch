crabs201X <- read.csv("crabs201XAll.csv")


##### NORMALIZED POINTS #####
HourlyTrackDF = data.frame(matrix(nrow=0,ncol=9))
colnames(HourlyTrackDF) <- c("DateTime","CrabNum","NumEntries","Cocos","Natives","Scaevola","Sand","Island","Year")

#Function to find the closest point between pixels on a raster and crab tracks. Also serves as water masking 
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


for (crab in unique(crabs201X$CrabNum)) {
  #Grabs everything for each crab
  thisCrabTrax = subset(crabs201X,CrabNum==crab) 
  #Calculates date range of crab and figures out how many hourly rows we'll need
  thisDateVec = as.POSIXct(paste(thisCrabTrax$Date,thisCrabTrax$Time),format="%m/%d/%y %H:%M:%S")
  entryDF = data.frame(matrix(nrow=ceiling(as.numeric(thisDateVec[length(thisDateVec)] - thisDateVec[1],units="hours")),ncol=length(colnames(HourlyTrackDF))))
  entryDF[,] = 0
  colnames(entryDF) <- c("DateTime","CrabNum","NumEntries","Cocos","Natives","Scaevola","Sand","Island","Year")
  entryDF$CrabNum = thisCrabTrax$CrabNum[1]
  entryDF$Island = thisCrabTrax$Island[1]
  entryDF$Year = thisCrabTrax$Year[1]
  origTime = thisDateVec[1]
  entryDF$DateTime = seq(origTime,thisDateVec[length(thisDateVec)],by="hour")
  #For every track, figure out where the track is and update the habitat proportions in its time range accordingly
  for (track in 1:length(thisDateVec)) {
    elapsedTime = thisDateVec[track] - origTime
    numHoursElapsed = floor(as.numeric(elapsedTime,units="hours"))
    #Stores index in array which represents the amount of time passed for this entry since the first entry
    hoursIndex = numHoursElapsed + 1
    thisCrabIsland = islandCoordsList[[thisCrabTrax[1,"Island"]]]
    temp = findClosestPoint(thisCrabTrax$Latitude[track], thisCrabTrax$Longitude[track], thisCrabIsland)
    if (nrow(temp) != 0) {
      #Just seeing how often this dupe thing occurs really
      if (nrow(temp) > 1) { print(nrow(temp))}
      for (j in 1:nrow(temp)) {
        entryDF[hoursIndex,"NumEntries"] = entryDF[hoursIndex,"NumEntries"]+1
        if (temp[j,1] == 0) {
          entryDF[hoursIndex,"Cocos"] = entryDF[hoursIndex,"Cocos"] + 1
        } else if (temp[j,1] == 1) {
          entryDF[hoursIndex,"Natives"] = entryDF[hoursIndex,"Natives"] + 1
        } else if (temp[j,1] == 2) {
          entryDF[hoursIndex,"Scaevola"] = entryDF[hoursIndex,"Scaevola"] + 1
        } else if (temp[j,1] == 5) {
          entryDF[hoursIndex,"Sand"] = entryDF[hoursIndex,"Sand"] + 1
        }
      }
    }
  }
  #Normalizes every hour bucket
  for (hour in 1:nrow(entryDF)) {
    if (entryDF[hour,"NumEntries"] > 0) {
      entryDF[hour,c("Cocos","Natives","Scaevola","Sand")] = entryDF[hour,c("Cocos","Natives","Scaevola","Sand")]/entryDF[hour,"NumEntries"]
    }
  }
  HourlyTrackDF = rbind(HourlyTrackDF,entryDF)
}

## imports if we don't want to run the above code again
HourlyTrackDF <- read.csv("HourlyTrackNormalizedCrabsDF.csv")
HourlyTrackDF = HourlyTrackDF[,which(colnames(HourlyTrackDF)!="X")]
HourlyTrackDF$Island = as.character(HourlyTrackDF$Island)

#Removes all zero rows, of which there are quite many
HourlyTrackDF = HourlyTrackDF[which(HourlyTrackDF$NumEntries!=0),]
HourlyTrackDF = HourlyTrackDF[-which(HourlyTrackDF$Island=="paradise"),]
HourlyTrackDataDF = data.frame(matrix(nrow=length(unique(HourlyTrackDF$CrabNum)),ncol=11))
HourlyTrackDataDF[,] = 0
colnames(HourlyTrackDataDF) <- c("CrabNum","Island","NumEntries","TotalCocos","TotalNatives","TotalScaevola","TotalSand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
HourlyTrackDataDF$CrabNum = unique(HourlyTrackDF$CrabNum)

for (crab in 1:nrow(HourlyTrackDataDF)) {
  thisCrabTracks = HourlyTrackDF[HourlyTrackDF$CrabNum==HourlyTrackDataDF[crab,"CrabNum"],]
  HourlyTrackDataDF[crab,"Island"]= thisCrabTracks[1,"Island"]
  HourlyTrackDataDF[crab,"NumEntries"] = nrow(thisCrabTracks)
  HourlyTrackDataDF[crab,"AvailCocos"] = allLocations[HourlyTrackDataDF[crab,"Island"],"Cocos"]
  HourlyTrackDataDF[crab,"AvailNatives"] = allLocations[HourlyTrackDataDF[crab,"Island"],"Natives"]
  HourlyTrackDataDF[crab,"AvailScaevola"] = allLocations[HourlyTrackDataDF[crab,"Island"],"Scaevola"]
  HourlyTrackDataDF[crab,"AvailSand"] = allLocations[HourlyTrackDataDF[crab,"Island"],"Sand"]
  HourlyTrackDataDF[crab,"TotalCocos"] = sum(thisCrabTracks$Cocos)
  HourlyTrackDataDF[crab,"TotalNatives"] = sum(thisCrabTracks$Natives)
  HourlyTrackDataDF[crab,"TotalScaevola"] = sum(thisCrabTracks$Scaevola)
  HourlyTrackDataDF[crab,"TotalSand"] = sum(thisCrabTracks$Sand)
}
HourlyTrackDataDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")] = HourlyTrackDataDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")]/rowSums(HourlyTrackDataDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")])

#Adds 0.000001 to every entry so wides III isn't angry at us
HourlyTrackDataDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")] = HourlyTrackDataDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")] + 0.000001

widesIII(HourlyTrackDataDF[,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")],HourlyTrackDataDF[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])

HourlyTrackDataDF$CocosWI= 0
HourlyTrackDataDF$NativesWI= 0
HourlyTrackDataDF$ScaevolaWI= 0
HourlyTrackDataDF$SandWI= 0
for (crab in 1:nrow(HourlyTrackDataDF)) {
  ratios = widesIII(HourlyTrackDataDF[crab,c("TotalCocos","TotalNatives","TotalScaevola","TotalSand")],HourlyTrackDataDF[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  HourlyTrackDataDF[crab,"CocosWI"] = ratios$wi[1]
  HourlyTrackDataDF[crab,"NativesWI"] = ratios$wi[2]
  HourlyTrackDataDF[crab,"ScaevolaWI"] = ratios$wi[3]
  HourlyTrackDataDF[crab,"SandWI"] = ratios$wi[4]
}


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

  
meltHourlyTrackDataDF <- melt(HourlyTrackDataDF[,c("Island","CocosWI","NativesWI","ScaevolaWI","SandWI")],id=c("Island"))
dfHourlyNorm <- data_summary(meltHourlyTrackDataDF,varname="value",groupnames=c("Island","variable"))



pdf("7.16NormalizedCrabTracksv2.pdf")
#All crabs ever, plotting selection ratios of habitats by island
p <- ggplot(data=dfHourlyNorm,aes(x=Island,y=value,fill=variable)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17, \nNormalized Entries, Tracks Only")
print(p)

#Analysis of crab selection by island, comparing actual track% presence vs. available presence
meltNormFrameAll = melt(HourlyTrackDataDF[,-c(1,3,12:15)],id=c("Island"))
relativem <- data_summary(meltNormFrameAll,varname="value",groupnames= c("Island","variable"))
relativem$Island = paste(relativem$Island,substr(relativem$variable,1,5))
relativem$variable = substr(relativem$variable,6,200)
p <- ggplot(data=relativem,aes(x=Island,y=value,fill=variable)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) + theme(text = element_text(size=10),
                                                     axis.text.x = element_text(angle=60, hjust=1)) + ggtitle("% Crab Tracks in Each Habitat Type by Island, \nNormalized, Compared to Real Habitat Ratio") 
print(p)

p <- ggplot(data=meltHourlyTrackDataDF,aes(x=variable,y=value,fill=variable)) + geom_violin(position=position_dodge()) +
  stat_summary(fun.y="mean",geom="point",position=position_dodge(0.9)) + ggtitle("Crab Selection Ratio in Each Habitat Type By Island") + facet_wrap(~Island)
p

dev.off()

##### MEDIAN APPROACH ######
HourlyMedianDF = data.frame(matrix(nrow=0,ncol=6))
colnames(HourlyMedianDF) <-  c("DateTime","Latitude","Longitude","CrabNum","Island","Year")

#For each crab, get all the crabs at times within each 30 minute interval, 
for (crab in unique(crabs201X$CrabNum)) {
  #Grabs everything for each crab
  thisCrabTrax = subset(crabs201X,CrabNum==crab) 
  #Calculates date range of crab and figures out how many hourly rows we'll need
  thisDateVec = as.POSIXct(paste(thisCrabTrax$Date,thisCrabTrax$Time),format="%m/%d/%y %H:%M:%S")
  entryDF = data.frame(matrix(nrow= ceiling(as.numeric(thisDateVec[length(thisDateVec)] - thisDateVec[1],units="hours")),ncol=length(colnames(HourlyMedianDF))))
  entryDF[,] = 0
  colnames(entryDF) <- c("DateTime","Latitude","Longitude","CrabNum","Island","Year")
  entryDF$CrabNum = thisCrabTrax$CrabNum[1]
  entryDF$Island = thisCrabTrax$Island[1]
  entryDF$Year = thisCrabTrax$Year[1]
  origTime = thisDateVec[1]
  entryDF$DateTime = seq(origTime,thisDateVec[length(thisDateVec)],by="hour")
  for (time in 1:nrow(entryDF)){
    #Either gets all the crab entries between two time intervals or gets everything after the last timestamp to avoid 
    #an off-by-one error
    if (time < nrow(entryDF)) {
      tempdf = thisCrabTrax[which(thisDateVec>=entryDF$DateTime[time] & thisDateVec<=entryDF$DateTime[time+1]),]
    } else {
      tempdf = thisCrabTrax[which(thisDateVec>=entryDF$DateTime[nrow(entryDF)]),]
    }
    if (nrow(tempdf) >0 ) {
      #Apparently you can't take the geometric median of a matrix with one row. wow.
      if (nrow(tempdf == 1)) {
        entryDF[time,c("Latitude","Longitude")] = tempdf[1,c("Latitude","Longitude")]
      } else {
        entryDF[time,c("Latitude","Longitude")] = geo_median(as.matrix(tempdf[,c("Latitude","Longitude")]))$p
      }
    }
  }
  HourlyMedianDF = rbind(HourlyMedianDF,entryDF)
}
HourlyMedianDF = HourlyMedianDF[which(HourlyMedianDF$Latitude != 0),]


#Plots individual median-obtained crab tracks per island, by day
#pdf("7.16TimeMedianCrabVisualTracks.pdf")
for (crab in unique(HourlyMedianDF$CrabNum)) {
  crabList = list()
  thisCrabTrax = HourlyMedianDF[which(HourlyMedianDF$CrabNum == crab),]
  thisCrabTrax$Date = substr(thisCrabTrax$DateTime,1,10)
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
  legend(title=paste("Crab:",crab),x=(0.5*imgtest@extent[1]+1.5*imgtest@extent[2])/2,y=(1.25*imgtest@extent[3]+0.75*imgtest@extent[4])/2,legend=unique(thisCrabTrax$Date),col=colors,lty=1,cex=0.75)
}
#dev.off()

fiftyoffset = 50
kudoffset = 100
#pdf("7.16just134.pdf")
#Generates a list to add different HR estimates to
crabHRList = list()

#Distribution of island crabs
#table(HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),"Island"])
for (crab in unique(HourlyMedianDF$CrabNum)) {
  print(crab)
  #Converts to spatial points
  thisCrabTrax = HourlyMedianDF[which(HourlyMedianDF$CrabNum == crab),];
  thisCrabTrax.sp = SpatialPoints(coords = thisCrabTrax[,c("Longitude","Latitude")], proj4string = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')) # convert to SpatialPoints object
  thisCrabTrax.spt <- spTransform(thisCrabTrax.sp,CRS("+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  resolution = 1
  x <- seq(extent(thisCrabTrax.spt)[1]-300, extent(thisCrabTrax.spt)[2]+300,by=resolution) # where resolution is the pixel size you desire 
  y <- seq(extent(thisCrabTrax.spt)[3]-300, extent(thisCrabTrax.spt)[4]+300,by=resolution)
  xy <- expand.grid(x=x,y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  
  #Constructs kernel utilization density of crab tracks
  kud <- kernelUD(thisCrabTrax.spt, grid=xy, h="href") # NOTE: look into changing grid value. Currently I think it's too big because the estimated homerange (plotted below) looks too big. Either grid or h needs tweaking.
  kernel.area(kud)
  
  #Plots island
  island = thisCrabTrax[1,"Island"]
  imgtest <- brick(paste0(island,".tif"))
  
  #Plots kud over island and crab tracks
  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  ver <- getverticeshr(kud, 95)
  plot(ver, add=TRUE, col=rainbow(4)[1])
  crabHRList[crab+kudoffset] = ver
  ver2 <- getverticeshr(kud, 50)
  plot(ver2, add=TRUE, col=rainbow(4)[2])
  crabHRList[crab+fiftyoffset+kudoffset] = ver2
  legend(legend=c("KUD95", "KUD50"),title=paste("Crab:",crab),x="bottomright",
         col=c(rainbow(4)[1], rainbow(4)[2]), lty=1, cex=1)
  
  #Constructs mcp for crab 1 tracks
  polygon <- mcp(thisCrabTrax.spt, percent=95)
  crabHRList[crab] = polygon
  polygon2 <- mcp(thisCrabTrax.spt, percent=50)
  crabHRList[crab+fiftyoffset] = polygon2
  #Plots mcp16 for crab 1 with respect to the percentage of crab tracks included
  #hrs <- mcp16.area(thisCrabTrax.sp, percent=seq(50, 100, by = 5)) 
  #plot(hrs) 
 
  
  #Plots mcp (95/50) over island and crab tracks
  plotRGB(imgtest, r=1, g=2, b=3,stretch="lin")
  plot(polygon, add=TRUE, col = rainbow(4)[1])
  plot(polygon2, add=TRUE, col = rainbow(4)[2])
  legend(legend=c("MCP95", "MCP50"),title=paste("Crab:",crab),x="bottomright",
         col=c(rainbow(4)[1], rainbow(4)[2]), lty=1, cex=1)
}
#dev.off()
HourlyMedianDF = subset(HourlyMedianDF,CrabNum!=133)
#saveRDS(crabHRList, file = "crabHRdata.rds")






#Starts homerange analysis
islandRasterList <- list()
for (isle in islands) {
  classedIsle = raster(paste0(isle,".tif"))
  islandRasterList[isle] <- classedIsle
}

#Creates data frames to hold crab habitat association data using home ranges
mcpmedframe = data.frame(matrix(nrow=length(unique(HourlyMedianDF$CrabNum)),ncol=14))
kudmedframe = data.frame(matrix(nrow=length(unique(HourlyMedianDF$CrabNum)),ncol=14))
colnames(mcpmedframe) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
colnames(kudmedframe) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
mcpmedframe$CrabNum = unique(HourlyMedianDF$CrabNum)
kudmedframe$CrabNum = unique(HourlyMedianDF$CrabNum)
View(head(HourlyMedianDF))
#Calculates amount of available habitat in crab home range (95%/50% mcp16)
for (crab in 1:nrow(mcpmedframe)) {
  mcpmedframe[crab,"Island"] = substr(HourlyMedianDF[HourlyMedianDF$CrabNum==mcpmedframe[crab,"CrabNum"],][1,"Island"],0,100)
  mcpmedframe[crab,"AvailCocos"] = allLocations[mcpmedframe[crab,"Island"],"Cocos"]
  mcpmedframe[crab,"AvailNatives"] = allLocations[mcpmedframe[crab,"Island"],"Natives"]
  mcpmedframe[crab,"AvailScaevola"] = allLocations[mcpmedframe[crab,"Island"],"Scaevola"]
  mcpmedframe[crab,"AvailSand"] = allLocations[mcpmedframe[crab,"Island"],"Sand"]
  thisCrabIsland = islandRasterList[[mcpmedframe[crab,"Island"]]]
  #95% mcp16
  maskedIsland95 = mask(thisCrabIsland,crabHRList[[mcpmedframe[crab,"CrabNum"]]])
  
  masked95pts <- rasterToPoints(maskedIsland95, spatial=TRUE)
  masked95tb <- table(masked95pts@data)  
  masked95tb[c("0","1","2","5")[is.na(masked95tb[c("0","1","2","5")])]] = 0
  masked95tb = masked95tb[c("0","1","2","5")]
  mcpmedframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")] <- masked95tb
  #50% mcp16
  maskedIsland50 = mask(thisCrabIsland,crabHRList[[mcpmedframe[crab,"CrabNum"]+fiftyoffset]])
  masked50pts <- rasterToPoints(maskedIsland50, spatial=TRUE)
  masked50tb <- table(masked50pts@data)
  masked50tb[c("0","1","2","5")[is.na(masked50tb[c("0","1","2","5")])]] = 0
  masked50tb = masked50tb[c("0","1","2","5")]
  mcpmedframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")] <- masked50tb
}


#Calculates amount of available habitat in crab home range (95%/50% kernel)
for (crab in 1:nrow(kudmedframe)) {
  kudmedframe[crab,"Island"] = substr(HourlyMedianDF[HourlyMedianDF$CrabNum==kudmedframe[crab,"CrabNum"],][1,"Island"],0,100)
  kudmedframe[crab,"AvailCocos"] = allLocations[kudmedframe[crab,"Island"],"Cocos"]
  kudmedframe[crab,"AvailNatives"] = allLocations[kudmedframe[crab,"Island"],"Natives"]
  kudmedframe[crab,"AvailScaevola"] = allLocations[kudmedframe[crab,"Island"],"Scaevola"]
  kudmedframe[crab,"AvailSand"] = allLocations[kudmedframe[crab,"Island"],"Sand"]
  thisCrabIsland = islandRasterList[[kudmedframe[crab,"Island"]]]

  #95% kernel
  maskedIsland95 = mask(thisCrabIsland,crabHRList[[kudmedframe[crab,"CrabNum"]+kudoffset]])
  masked95pts <- rasterToPoints(maskedIsland95, spatial=TRUE)
  masked95tb <- table(masked95pts@data)  
  masked95tb[c("0","1","2","5")[is.na(masked95tb[c("0","1","2","5")])]] = 0
  masked95tb = masked95tb[c("0","1","2","5")]
  kudmedframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")] <- masked95tb
  #50% kernel
  maskedIsland50 = mask(thisCrabIsland,crabHRList[[kudmedframe[crab,"CrabNum"]+kudoffset+fiftyoffset]])
  masked50pts <- rasterToPoints(maskedIsland50, spatial=TRUE)
  masked50tb <- table(masked50pts@data)
  masked50tb[c("0","1","2","5")[is.na(masked50tb[c("0","1","2","5")])]] = 0
  masked50tb = masked50tb[c("0","1","2","5")]
  kudmedframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")] <- masked50tb
}
#write.csv(mcpmedframe,"7.17normMCPFrame.csv")
#write.csv(kudmedframe,"7.17normKUDFrame.csv")

#Adds 0.0001 so widesIII isn't too angry at us
mcpmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")] = mcpmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]/rowSums(mcpmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]) + 0.0001
mcpmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")] = mcpmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]/rowSums(mcpmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]) + 0.0001
kudmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")] = kudmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]/rowSums(kudmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]) + 0.0001
kudmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")] = kudmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]/rowSums(kudmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]) + 0.0001

#Creates new data frames to hold individual selection ratios
mcpmedwiframe = data.frame(matrix(nrow=length(unique(HourlyMedianDF$CrabNum)),ncol=8))
kudmedwiframe = data.frame(matrix(nrow=length(unique(HourlyMedianDF$CrabNum)),ncol=8))

colnames(mcpmedwiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")
colnames(kudmedwiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")


#Calculates individual selection ratios for each crab 
for (crab in 1:nrow(mcpmedwiframe)) {
  mcp1695ratios = widesIII(mcpmedframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],mcpmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1695ratios = widesIII(kudmedframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],kudmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  
  mcp1650ratios = widesIII(mcpmedframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],mcpmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1650ratios = widesIII(kudmedframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],kudmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  
  mcpmedwiframe[crab,"95CocosWI"] = mcp1695ratios$wi[1]
  mcpmedwiframe[crab,"95NativesWI"] = mcp1695ratios$wi[2]
  mcpmedwiframe[crab,"95ScaevolaWI"] = mcp1695ratios$wi[3]
  mcpmedwiframe[crab,"95SandWI"] = mcp1695ratios$wi[4]
  
  mcpmedwiframe[crab,"50CocosWI"] = mcp1650ratios$wi[1]
  mcpmedwiframe[crab,"50NativesWI"] = mcp1650ratios$wi[2]
  mcpmedwiframe[crab,"50ScaevolaWI"] = mcp1650ratios$wi[3]
  mcpmedwiframe[crab,"50SandWI"] = mcp1650ratios$wi[4]
  
  kudmedwiframe[crab,"95CocosWI"] = kud1695ratios$wi[1]
  kudmedwiframe[crab,"95NativesWI"] = kud1695ratios$wi[2]
  kudmedwiframe[crab,"95ScaevolaWI"] = kud1695ratios$wi[3]
  kudmedwiframe[crab,"95SandWI"] = kud1695ratios$wi[4]
  
  kudmedwiframe[crab,"50CocosWI"] = kud1650ratios$wi[1]
  kudmedwiframe[crab,"50NativesWI"] = kud1650ratios$wi[2]
  kudmedwiframe[crab,"50ScaevolaWI"] = kud1650ratios$wi[3]
  kudmedwiframe[crab,"50SandWI"] = kud1650ratios$wi[4]
}

range(mcpmedwiframe$`95NativesWI`)
range(mcpmedwiframe$`50NativesWI`)
range(kudmedwiframe$`95NativesWI`)
range(kudmedwiframe$`50NativesWI`)

mean(mcpmedwiframe$`95NativesWI`)
mean(mcpmedwiframe$`50NativesWI`)
mean(kudmedwiframe$`95NativesWI`)
mean(kudmedwiframe$`50NativesWI`)

mean(mcpmedwiframe$`95CocosWI`)
mean(mcpmedwiframe$`50CocosWI`)
mean(kudmedwiframe$`95CocosWI`)
mean(kudmedwiframe$`50CocosWI`)

mean(mcpmedwiframe$`95ScaevolaWI`)
mean(mcpmedwiframe$`50ScaevolaWI`)
mean(kudmedwiframe$`95ScaevolaWI`)
mean(kudmedwiframe$`50ScaevolaWI`)

mcpmedwiframe$CrabNum = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$CrabNum
mcpmedwiframe$Island = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Island
mcpmedwiframe$Year = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Year
  
kudmedwiframe$CrabNum = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$CrabNum
kudmedwiframe$Island = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Island
kudmedwiframe$Year = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Year
  

# Begin visualizing 2016-2017 crabs data (normalized with median)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

mcpmedframe = subset(mcpmedframe,Island!="paradise")
kudmedframe = subset(kudmedframe,Island!="paradise")
mcpmedwiframe = subset(mcpmedwiframe,Island!="paradise")
kudmedwiframe = subset(kudmedwiframe,Island!="paradise")

pdf("7.16Crabs201617NormCompiledAnalysis.pdf")
meltmcpallwiframe <- melt(mcpmedwiframe,id=c("CrabNum","Island","Year"))
meltmcpallwiframe$type = revalue(substr(meltmcpallwiframe$variable,1,2),c("95" = "mcp95","50" = "mcp50"))
meltmcpallwiframe$variable = substr(meltmcpallwiframe$variable,3,100)
dfmcpall <- data_summary(meltmcpallwiframe,varname="value",groupnames=c("type","variable"))
meltkudallwiframe <- melt(kudmedwiframe,id=c("CrabNum","Island","Year"))
meltkudallwiframe$type = revalue(substr(meltkudallwiframe$variable,1,2),c("95" = "kud95","50" = "kud50"))
meltkudallwiframe$variable = substr(meltkudallwiframe$variable,3,100)
dfkudall <- data_summary(meltkudallwiframe,varname="value",groupnames=c("type","variable"))
dfall = rbind(dfmcpall,dfkudall)

meltall = rbind(meltmcpallwiframe,meltkudallwiframe)

p <- ggplot(data=meltall,aes(x=variable,y=value,fill=type)) + geom_violin(position=position_dodge()) + 
  stat_summary(fun.y="mean",geom="point",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17")  
p 

#All crbas ever, plotting in general
p <- ggplot(data=dfall,aes(x=variable,y=value,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17")
p

#All crabs ever, plotting by year
meltmcpallwiframe$varYear = paste(meltmcpallwiframe$variable,meltmcpallwiframe$Year)
meltkudallwiframe$varYear = paste(meltkudallwiframe$variable,meltkudallwiframe$Year)
dfmcpallvy <- data_summary(meltmcpallwiframe,varname="value",groupnames=c("type","varYear"))
dfkudallvy <- data_summary(meltkudallwiframe,varname="value",groupnames=c("type","varYear"))
dfallvy = rbind(dfmcpallvy,dfkudallvy)
p <- ggplot(data=dfallvy,aes(x=varYear,y=value,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +  ggtitle("Average Habitat Selection Ratio for Coconut Crabs By Year")
p

#All crabs ever, plotting by island 
meltmcpallwiframe$varIsle = paste(meltmcpallwiframe$variable,meltmcpallwiframe$Island)
meltkudallwiframe$varIsle = paste(meltkudallwiframe$variable,meltkudallwiframe$Island)
dfmcpallvi <- data_summary(meltmcpallwiframe,varname="value",groupnames=c("type","varIsle"))
dfkudallvi <- data_summary(meltkudallwiframe,varname="value",groupnames=c("type","varIsle"))
dfallvi = rbind(dfmcpallvi,dfkudallvi)
p <- ggplot(data=dfallvi,aes(x=varIsle,y=value,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) + theme(text = element_text(size=10),
                                                     axis.text.x = element_text(angle=60, hjust=1)) +  ggtitle("Average Habitat Selection Ratio for Coconut Crabs By Island")
p

#Analysis of crab selection by island
meltmcpframeall = melt(mcpmedframe[,2:10],id=c("Island"))
meltmcpframeall$type = revalue(substr(meltmcpframeall$variable,1,2),c("95" = "mcp95","50" = "mcp50"))
meltmcpframeall$variable = substr(meltmcpframeall$variable,3,100)
relativemcp <- data_summary(meltmcpframeall,varname="value",groupnames= c("type","Island","variable"))
meltkudframeall = melt(kudmedframe[,2:10],id=c("Island"))
meltkudframeall$type = revalue(substr(meltkudframeall$variable,1,2),c("95" = "kud95","50" = "kud50"))
meltkudframeall$variable = substr(meltkudframeall$variable,3,100)
relativekud <- data_summary(meltkudframeall,varname="value",groupnames= c("type","Island","variable"))
relativeall = rbind(relativemcp,relativekud)
for (isle in 1:nrow(allLocations)) {
  if (rownames(allLocations)[isle] != "paradise") {
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Cocos",allLocations[isle,"Cocos"],0)
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Natives",allLocations[isle,"Natives"],0)
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Scaevola",allLocations[isle,"Scaevola"],0)
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Sand",allLocations[isle,"Sand"],0)
  }
}
relativeall$varIsle = paste(relativeall$variable,relativeall$Island)
relativeall$sd[is.na(relativeall$sd)] = 0
relativeall$value = as.numeric(relativeall$value)
relativeall$sd = as.numeric(relativeall$sd)
p <- ggplot(data=relativeall,aes(x=varIsle,y=value,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) + theme(text = element_text(size=10),
                                                    axis.text.x = element_text(angle=60, hjust=1)) + ggtitle("% Crab Home Range in Each Habitat Type by Island, Compared to Real Habitat Ratio")
#+ theme_minimal()
p

meltcraball = rbind(meltmcpframeall,meltkudframeall)
for (isle in 1:nrow(allLocations)) {
  if (rownames(allLocations)[isle] != "paradise") {
  meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Cocos",allLocations[isle,"Cocos"],"actual")
  meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Natives",allLocations[isle,"Natives"],"actual")
  meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Scaevola",allLocations[isle,"Scaevola"],"actual")
  meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Sand",allLocations[isle,"Sand"],"actual")
  }
}
meltcraball$varIsle = paste(meltcraball$variable,meltcraball$Island)
meltcraball = subset(meltcraball,type=="kud95" | type == "actual")
meltcraball$value = as.numeric(meltcraball$value)
p <- ggplot(data=meltcraball,aes(x=variable,y=value,fill=type)) + geom_violin(position=position_dodge(),fill="green") +
  stat_summary(fun.y="mean",geom="point",aes(color=type)) + ggtitle("% Crab Home Range in Each Habitat Type By Island, Compared to Real Habitat Ratio") + facet_wrap(~Island)
p
#dev.off()
dev.off()
