
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

#Removes all zero rows, of which there are quite many
HourlyTrackDF = HourlyTrackDF[which(HourlyTrackDF$NumEntries!=0),]

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

#pdf("7.16NormalizedCrabTracks.pdf")

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

#dev.off()

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
pdf("TimeMedianCrabVisualTracks.pdf")
for (crab in unique(HourlyMedianDF$CrabNum)) {
  crabList = list()
  thisCrabTrax = HourlyMedianDF[which(HourlyMedianDF$CrabNum == crab),]
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
  legend(title=paste("Crab:",crab),x=(imgtest@extent[1]+imgtest@extent[2])/2,y=(0.5*imgtest@extent[3]+1.5*imgtest@extent[4])/2,legend=unique(thisCrabTrax$Date),col=colors,lty=1,cex=0.75)
}
dev.off()



