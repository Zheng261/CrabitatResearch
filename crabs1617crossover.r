# Import smoothed classification image:
#classed <- raster("6.26MASKEDPalmyra-RF-classification-v3-5x5-modal.tif") # Took 5x5 MODAL average
#classed <- raster("Palmyra-RF-classification-v3-5x5-modal.tif") # Took 5x5 MODAL average

####### Crabs 2017 #######
# Initialized crab tracking import. All files are filtered for speed and water
newWD = "../Palmyra Crab Research/Crab tagging/crab tracks 2017/trimmed crab tracks/waterspeedtrimmed"
crabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2017/trimmed crab tracks/waterspeedtrimmed")
importantValues = c("Date","Time","Latitude","Longitude","Altitude","Speed","Course","Distance","CrabNum","Island")

#Classifies crabs by island
islands = c("sand","cooper","eastern","paradise")
islandList = c()
otherCrabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2017/2017 crab tracks CSV")
for (island in islands) {
  islandList[as.integer(substr(otherCrabList[grep(island,otherCrabList)],22,23))] = island
}

#Starts off crab tracking files
crabs2017 <- read.csv(paste0(newWD,"/",crabList[1]))[1,]
crabs2017$CrabNum <- NA
crabs2017$Island <- NA
crabs2017 = crabs2017[-1,]

#Imports all crab tracking files and compiles into one big data frame
for (name in crabList) {
  print(name)
  temp = read.csv(paste0(newWD,"/",name));
  #Optional date filtering
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



####### Crabs 2016 #######
# Initialized crab tracking import. All files are filtered for speed and water
newWD = "../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks/waterspeedtrimmed"
crabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks/waterspeedtrimmed")
otherCrabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks")

#Classifies crabs by island
islands = c("sand","cooper","eastern","paradise")
islandList = c()
for (island in islands) {
  islandList[as.integer(substr(otherCrabList[grep(island,otherCrabList)],6,7))] = island
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
  temp$CrabNum = as.integer(substring(name,6,7))
  temp$Island = islandList[as.integer(substring(name,6,7))]
  crabs2016 = rbind(crabs2016,temp[,importantValues]);
}

crabs2016$Year = "2016"
crabs2017$Year = "2017"
crabs2017$CrabNum = crabs2017$CrabNum + 100
TempDate = format(as.POSIXct(paste(crabs2017$Date,crabs2017$Time),format="%m/%d/%Y %H:%M:%S"),format="%m/%d/%y")
TempDateOthers = format(as.POSIXct(paste(crabs2017$Date,crabs2017$Time),format="%Y/%m/%d %H:%M:%S"),format="%m/%d/%y")
TempDate[is.na(TempDate)] = TempDateOthers[!is.na(TempDateOthers)]

crabs2017$Date = TempDate
unique(crabs2017$Date)
crabs201X = rbind(crabs2016,crabs2017)


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
  allLocations[isle,"Cocos"] = totalavailhab[1]/sum(totalavailhab)
  allLocations[isle,"Natives"] = totalavailhab[2]/sum(totalavailhab)
  allLocations[isle,"Scaevola"] = totalavailhab[3]/sum(totalavailhab)
  allLocations[isle,"Sand"] = totalavailhab[4]/sum(totalavailhab)
}

dateVec = as.POSIXct(paste(crabs201X$Date,crabs201X$Time),format="%m/%d/%y %H:%M:%S")
crabs201X$Elapsed = dateVec - c(dateVec[1],dateVec[-length(dateVec)])
for (entry in 2:nrow(crabs201X)) {
   if (crabs201X[entry,"CrabNum"] != crabs201X[entry-1,"CrabNum"]) {
     crabs201X[entry,"Elapsed"] = 0
   }
}


abacus<-function(state_rec, times,states=NULL,labels=NULL,add=FALSE,xlim=NULL,tunit="month", format="%m/%y",col="black",
                 ylab="Station",xlab="date",yline=4,xline=3,xcex=1.5,ycex=1.5,cex.yaxis=.75,cex.xaxis=.75,pch=15,main="Crab"){
  length.out<-length(state_rec)
  if(is.null(states)){
    states<-unique(state_rec)
  }
  nstates<-length(states)
  order<-rep(1,length.out)
  for(i in 2:nstates){
    order[which(state_rec==states[i])]<-i
  }
  if(add==FALSE){
    if(is.null(xlim)){
      xlim<-c(min(times,na.rm=TRUE),max(times,na.rm=TRUE))
    } 
    if(is.null(labels)){
      labels<-states
    }
    ylim<-c(.5,(nstates+.5))
    
    plot(main=main,0,type="n",ylim=ylim,xlim=xlim,yaxt="n",xaxt="n",xlab=" ",ylab=" ")
    axis(2,at=c(1:nstates),labels=labels,las=2,cex.axis=cex.yaxis)
    axis.POSIXct(1, at=seq(xlim[1], xlim[2], by=tunit)
                 ,format=format,las=2,cex.axis=cex.xaxis)
    mtext(text = xlab,side = 1,line = xline, cex = xcex)
    mtext(text = ylab,side = 2,line = yline, cex = ycex)
  }
  points(order ~ times,pch=pch,col=col)
}

#StateTime<-seq(as.POSIXct("2016-05-20"),as.POSIXct("2016-05-24"),by="days")
#States<-sample(c("State1","State2","State3","State4"),size=5, replace=TRUE)
#ind<-sample(c("Fish1","Fish2","Fish3","Fish4","Fish5","Fish6","Fish7","Fish9","Fish10"),size=5,replace=TRUE)
#Plotting by station
#abacus(state_rec=States,times=StateTime,states=c("State1","State2","State3","State4","State5"),col=as.factor(ind))
#abacus(state_rec=ind,times=StateTime,col=as.factor(States))

length(crabs201X[,1])
unique(crabs201X$Date)
#pdf("GapAnalysis.pdf")
length(unique(crabs201X[,"CrabNum"]))
for (day in unique(crabs201X$Date)) {
  thisCrabDay = subset(crabs201X,Date==day)
  thisDateVec = as.POSIXct(paste(thisCrabDay$Date,thisCrabDay$Time),format="%m/%d/%y %H:%M:%S")
  abacus(state_rec=thisCrabDay$CrabNum,times=thisDateVec,states=unique(thisCrabDay$CrabNum),format="%m/%d - %H",tunit="hour",xcex=1,xlab="Date",ylab="Crab",col=revalue(thisCrabDay$Island,c("cooper"="red","sand"="yellow","paradise"="green","eastern"="blue")),main=day)
}
#dev.off()
#pdf("SingleCrabGapAnalysis.pdf")
for (crab in unique(crabs201X$CrabNum)) {
  thisCrabTrax = subset(crabs201X,CrabNum==crab)
  thisDateVec = as.POSIXct(paste(thisCrabTrax$Date,thisCrabTrax$Time),format="%m/%d/%y %H:%M:%S")
  abacus(state_rec=thisCrabTrax$CrabNum,times=thisDateVec,states=unique(thisCrabTrax$CrabNum),format="%m/%d - %H",tunit="hour",xcex=1,xlab="Date",ylab="Crab",col=revalue(thisCrabTrax$Island,c("cooper"="red","sand"="yellow","paradise"="green","eastern"="blue")),main=crab)
}
#dev.off()


#Plotting how many crab hits per half an hour 
#crabNumVec = crabs201X$CrabNum
#thisDateVec = as.POSIXct(paste(crabs201X$Date, crabs201X$Time),format="%m/%d/%y %H:%M:%S")
#pdf("7.12CrabHitsPerHour.pdf")
for (crab in unique(crabs201X$CrabNum)) {
  thisCrabTrax = subset(crabs201X,CrabNum==crab) 
  thisDateVec = as.POSIXct(paste(thisCrabTrax$Date,thisCrabTrax$Time),format="%m/%d/%y %H:%M:%S")
  entryDF = data.frame(matrix(nrow=round(as.numeric(thisDateVec[length(thisDateVec)] - thisDateVec[1],units="hours"),0)+1,ncol=2))
  colnames(entryDF) = c("Time","NumEntries")
  entryDF$Time = seq(0,nrow(entryDF)-1,by=1)
  entryDF$NumEntries = 0
  origTime = thisDateVec[1]
  for (track in 1:length(thisDateVec)) {
     elapsedTime = thisDateVec[track] - origTime
     numHoursElapsed = as.numeric(elapsedTime,units="hours")
     entryDF[round(numHoursElapsed,0),"NumEntries"] = entryDF[round(numHoursElapsed,0),"NumEntries"]+1
  }
  bucketplot = ggplot(data=entryDF,aes(x=Time,y=NumEntries)) + geom_bar(stat="identity") + theme_minimal() + xlab("Hours elapsed") + ylab("Number of entries") + ggtitle(paste("Crab#",crab))
  print(bucketplot)
}
#dev.off()


# Begins compiling 2017 and 2016 crab data
mcpframe <- read.csv("MCP17-95-and-50-crabs-palmyra.csv")
kudframe <- read.csv("KUD17-95-and-50-crabs-palmyra.csv")
mcp16frame <- read.csv("propmcp16-95-and-50-crabs-palmyra.csv")
kud16frame <- read.csv("propkud16-95-and-50-crabs-palmyra.csv")
mcpframe$Year = "2017"
kudframe$Year = "2017"
mcpframe$CrabNum = mcpframe$CrabNum + 100
kudframe$CrabNum = kudframe$CrabNum + 100
mcp16frame$Year = "2016"
kud16frame$Year = "2016"


mcpframeAll = rbind(mcp16frame,mcpframe)
kudframeAll = rbind(kud16frame,kudframe)
colnames(mcpframeAll) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand","Year")
colnames(kudframeAll) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand","Year")

mcpallwiframe = data.frame(matrix(nrow=length(unique(crabs201X$CrabNum)),ncol=11))
kudallwiframe = data.frame(matrix(nrow=length(unique(crabs201X$CrabNum)),ncol=11))
colnames(mcpallwiframe) <- c("CrabNum","Island","Year",paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI"))
colnames(kudallwiframe) <- c("CrabNum","Island","Year",paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI"))
mcpallwiframe[,c("CrabNum","Island","Year")] = mcpframeAll[,c("CrabNum","Island","Year")]
kudallwiframe[,c("CrabNum","Island","Year")] = kudframeAll[,c("CrabNum","Island","Year")]

for (crab in 1:nrow(mcpallwiframe)) {
  #ADEHabitat really doesn't like it when we have more than one zero in a column
  mcp1695ratios = widesIII(mcpframeAll[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],mcpframeAll[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1695ratios = widesIII(kudframeAll[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],kudframeAll[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  mcp1650ratios = widesIII(mcpframeAll[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],mcpframeAll[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1650ratios = widesIII(kudframeAll[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],kudframeAll[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  
  mcpallwiframe[crab,"95CocosWI"] = mcp1695ratios$wi[1]
  mcpallwiframe[crab,"95NativesWI"] = mcp1695ratios$wi[2]
  mcpallwiframe[crab,"95ScaevolaWI"] = mcp1695ratios$wi[3]
  mcpallwiframe[crab,"95SandWI"] = mcp1695ratios$wi[4]
  
  mcpallwiframe[crab,"50CocosWI"] = mcp1650ratios$wi[1]
  mcpallwiframe[crab,"50NativesWI"] = mcp1650ratios$wi[2]
  mcpallwiframe[crab,"50ScaevolaWI"] = mcp1650ratios$wi[3]
  mcpallwiframe[crab,"50SandWI"] = mcp1650ratios$wi[4]
  
  kudallwiframe[crab,"95CocosWI"] = kud1695ratios$wi[1]
  kudallwiframe[crab,"95NativesWI"] = kud1695ratios$wi[2]
  kudallwiframe[crab,"95ScaevolaWI"] = kud1695ratios$wi[3]
  kudallwiframe[crab,"95SandWI"] = kud1695ratios$wi[4]
  
  kudallwiframe[crab,"50CocosWI"] = kud1650ratios$wi[1]
  kudallwiframe[crab,"50NativesWI"] = kud1650ratios$wi[2]
  kudallwiframe[crab,"50ScaevolaWI"] = kud1650ratios$wi[3]
  kudallwiframe[crab,"50SandWI"] = kud1650ratios$wi[4]
}
kudallwiframe

mean(kudallwiframe$`95CocosWI`)
mean(kudallwiframe$`50CocosWI`)
mean(kudallwiframe$`95NativesWI`)
mean(kudallwiframe$`50NativesWI`)

mean(mcpallwiframe$`95CocosWI`)
mean(mcpallwiframe$`50CocosWI`)
mean(mcpallwiframe$`95NativesWI`)
mean(mcpallwiframe$`50NativesWI`)

mean(kudallwiframe$`95ScaevolaWI`)
mean(kudallwiframe$`50ScaevolaWI`)

mean(mcpallwiframe$`95SandWI`)
mean(mcpallwiframe$`50SandWI`)


# Begin visualizing 2016-2017 crabs data
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

#pdf("Crabs201617CompiledAnalysis.pdf")

meltmcpallwiframe <- melt(mcpallwiframe,id=c("CrabNum","Island","Year"))
meltmcpallwiframe$type = revalue(substr(meltmcpallwiframe$variable,1,2),c("95" = "mcp95","50" = "mcp50"))
meltmcpallwiframe$variable = substr(meltmcpallwiframe$variable,3,100)
dfmcpall <- data_summary(meltmcpallwiframe,varname="value",groupnames=c("type","variable"))
meltkudallwiframe <- melt(kudallwiframe,id=c("CrabNum","Island","Year"))
meltkudallwiframe$type = revalue(substr(meltkudallwiframe$variable,1,2),c("95" = "kud95","50" = "kud50"))
meltkudallwiframe$variable = substr(meltkudallwiframe$variable,3,100)
dfkudall <- data_summary(meltkudallwiframe,varname="value",groupnames=c("type","variable"))
dfall = rbind(dfmcpall,dfkudall)
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
meltmcpframeall = melt(mcpframeAll[,2:10],id=c("Island"))
meltmcpframeall$type = revalue(substr(meltmcpframeall$variable,1,2),c("95" = "mcp95","50" = "mcp50"))
meltmcpframeall$variable = substr(meltmcpframeall$variable,3,100)
relativemcp <- data_summary(meltmcpframeall,varname="value",groupnames= c("type","Island","variable"))
meltkudframeall = melt(kudframeAll[,2:10],id=c("Island"))
meltkudframeall$type = revalue(substr(meltkudframeall$variable,1,2),c("95" = "kud95","50" = "kud50"))
meltkudframeall$variable = substr(meltkudframeall$variable,3,100)
relativekud <- data_summary(meltkudframeall,varname="value",groupnames= c("type","Island","variable"))
relativeall = rbind(relativemcp,relativekud)
for (isle in 1:nrow(allLocations)) {
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Cocos",allLocations[isle,"Cocos"],0)
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Natives",allLocations[isle,"Natives"],0)
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Scaevola",allLocations[isle,"Scaevola"],0)
  relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Sand",allLocations[isle,"Sand"],0)
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

#dev.off()



