library(geosphere) 
library(adehabitatHS)
# Import smoothed classification image:
classed <- raster("6.26MASKEDPalmyra-RF-classification-v3-5x5-modal.tif") # Took 5x5 MODAL average

# Initialized crab tracking import
newWD = "../Palmyra Crab Research/Crab tagging/crab tracks 2017/trimmed crab tracks/waterspeedtrimmed"
crabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2017/trimmed crab tracks/waterspeedtrimmed")
importantValues = c("Date","Time","Latitude","Longitude","Altitude","Speed","Course","Distance","CrabNum","Island","m.s.before")


#Classifies crabs by island
islands = c("sand","cooper","eastern","paradise")
islandList = c()
#This crab list contains all mappings of crabs to islands, so we use it to figure out which crabs belong where
otherCrabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2017/2017 crab tracks CSV")
for (island in islands) {
  islandList[as.integer(substr(otherCrabList[grep(island,otherCrabList)],22,23))] = island
}

#Starts off crab tracking files, literally just reads in column names and adds two new ones
crabs2017 = read.csv(paste0(newWD,"/crab-01waterspeed-trimmed.csv"))[1,]
crabs2017$CrabNum <- NA
crabs2017$Island <- NA
crabs2017 = crabs2017[-1,]

#Imports all crab tracking files and compiles into one big data frame
for (name in crabList) {
  print(name)
  temp = read.csv(paste0(newWD,"/",name));
  temp$CrabNum = as.integer(substring(name,6,7))
  temp$Island = islandList[as.integer(substring(name,6,7))]
  crabs2017 = rbind(crabs2017,temp[,importantValues]);
}
crabs2017 = crabs2017[which(!is.na(crabs2017$Latitude)),]

KUDoffset = 100
fiftyoffset = 50

#Begins calculating and plotting home range v. crab tracks 
#pdf("CrabitatNoTraxHomeRangeIndividual.pdf")
#Generates a list to add different HR estimates to
crabHRList = list()
for (crab in unique(crabs2017$CrabNum)) {
  #Converts to spatial points
  crab1 = crabs2017[which(crabs2017$CrabNum == crab),];
  crab1.sp = SpatialPoints(coords = crab1[,c("Longitude","Latitude")], proj4string = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')) # convert to SpatialPoints object
  crab1.spt <- spTransform(crab1.sp,CRS("+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  #Constructs MCP for crab 1 tracks
  polygon <- mcp(crab1.spt, percent=95)
  crabHRList[crab] = polygon
  polygon2 <- mcp(crab1.spt, percent=50)
  crabHRList[crab+fiftyoffset] = polygon2
  
  #Constructs kernel utilization density of crab tracks
  kud <- kernelUD(crab1.spt, grid=100, h="href") # NOTE: look into changing grid value. Currently I think it's too big because the estimated homerange (plotted below) looks too big. Either grid or h needs tweaking.
  
  #Finds area of polygon
  #areaPolygon(polygon)
  
  #Plots MCP for crab 1 with respect to the percentage of crab tracks included
  #hrs <- mcp.area(crab1.sp, percent=seq(50, 100, by = 5)) 
  #plot(hrs) 
  
  #Plots MCP (95/50) over island and crab tracks
  #plotRGB(imgtest, r=1, g=2, b=3,stretch="lin", main="MCP 95/50")
  #plot(polygon, add=TRUE, col = 2)
  #plot(polygon2, add=TRUE, col = 4)
  
  #Plots KUD over island and crab tracks
  #plotRGB(imgtest, r=1, g=2, b=3,stretch="lin", main="Kud 95/50")
  ver <- getverticeshr(kud, 95)
  #plot(ver, add=TRUE, col=rainbow(4)[1])
  crabHRList[crab+KUDoffset] = ver
  ver2 <- getverticeshr(kud, 50)
  #plot(ver2, add=TRUE, col=rainbow(4)[2])
  crabHRList[crab+fiftyoffset+KUDoffset] = ver2
}
#dev.off()


#Calculates KUD for crabs
image(kud)
class(kud)
str(kud)
#legend(699000, 3165000, legend = names(ud), fill = rainbow(4))


#Stores list of islands to be used for home range masking 
islandRasterList <- list()
for (isle in islands) {
  classedIsle = raster(paste0(isle,".tif"))
  islandRasterList[isle] <- classedIsle
}

#Creates data frames to hold crab habitat association data using home ranges
mcpframe = data.frame(matrix(nrow=length(unique(crabs2017$CrabNum)),ncol=14))
kudframe = data.frame(matrix(nrow=length(unique(crabs2017$CrabNum)),ncol=14))
colnames(mcpframe) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
colnames(kudframe) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
mcpframe$CrabNum = unique(crabs2017$CrabNum)
kudframe$CrabNum = unique(crabs2017$CrabNum)

#Calculates amount of available habitat in crab home range (95%/50% MCP)
for (crab in 1:nrow(mcpframe)) {
  mcpframe[crab,"Island"] = crabs2017[crabs2017$CrabNum==mcpframe[crab,"CrabNum"],][1,"Island"]
  mcpframe[crab,"AvailCocos"] = allLocations[mcpframe[crab,"Island"],"Cocos"]
  mcpframe[crab,"AvailNatives"] = allLocations[mcpframe[crab,"Island"],"Natives"]
  mcpframe[crab,"AvailScaevola"] = allLocations[mcpframe[crab,"Island"],"Scaevola"]
  mcpframe[crab,"AvailSand"] = allLocations[mcpframe[crab,"Island"],"Sand"]
  thisCrabIsland = islandRasterList[[mcpframe[crab,"Island"]]]
  #95% MCP
  maskedIsland95 = mask(thisCrabIsland,crabHRList[[mcpframe[crab,"CrabNum"]]])

  masked95pts <- rasterToPoints(maskedIsland95, spatial=TRUE)
  masked95tb <- table(masked95pts@data)  
  masked95tb[c("0","1","2","5")[is.na(masked95tb[c("0","1","2","5")])]] = 0
  masked95tb = masked95tb[c("0","1","2","5")]
  mcpframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")] <- masked95tb
  #50% MCP
  maskedIsland50 = mask(thisCrabIsland,crabHRList[[mcpframe[crab,"CrabNum"]+fiftyoffset]])
  masked50pts <- rasterToPoints(maskedIsland50, spatial=TRUE)
  masked50tb <- table(masked50pts@data)
  masked50tb[c("0","1","2","5")[is.na(masked50tb[c("0","1","2","5")])]] = 0
  masked50tb = masked50tb[c("0","1","2","5")]
  mcpframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")] <- masked50tb
}


#Calculates amount of available habitat in crab home range (95%/50% kernel)
for (crab in 1:nrow(kudframe)) {
  kudframe[crab,"Island"] = crabs2017[crabs2017$CrabNum==kudframe[crab,"CrabNum"],][1,"Island"]
  kudframe[crab,"AvailCocos"] = allLocations[kudframe[crab,"Island"],"Cocos"]
  kudframe[crab,"AvailNatives"] = allLocations[kudframe[crab,"Island"],"Natives"]
  kudframe[crab,"AvailScaevola"] = allLocations[kudframe[crab,"Island"],"Scaevola"]
  kudframe[crab,"AvailSand"] = allLocations[kudframe[crab,"Island"],"Sand"]
  thisCrabIsland = islandRasterList[[kudframe[crab,"Island"]]]
  #95% kernel
  maskedIsland95 = mask(thisCrabIsland,crabHRList[[kudframe[crab,"CrabNum"]+KUDoffset]])
  masked95pts <- rasterToPoints(maskedIsland95, spatial=TRUE)
  masked95tb <- table(masked95pts@data)  
  masked95tb[c("0","1","2","5")[is.na(masked95tb[c("0","1","2","5")])]] = 0
  masked95tb = masked95tb[c("0","1","2","5")]
  kudframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")] <- masked95tb
  #50% kernel
  maskedIsland50 = mask(thisCrabIsland,crabHRList[[kudframe[crab,"CrabNum"]+KUDoffset+fiftyoffset]])
  masked50pts <- rasterToPoints(maskedIsland50, spatial=TRUE)
  masked50tb <- table(masked50pts@data)
  masked50tb[c("0","1","2","5")[is.na(masked50tb[c("0","1","2","5")])]] = 0
  masked50tb = masked50tb[c("0","1","2","5")]
  kudframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")] <- masked50tb
}

widesIII(mcpframe[,c("95Cocos","95Natives","95Scaevola","95Sand")],mcpframe[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
widesIII(mcpframe[,c("50Cocos","50Natives","50Scaevola","50Sand")],mcpframe[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
widesIII(kudframe[,c("95Cocos","95Natives","95Scaevola","95Sand")],kudframe[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
widesIII(kudframe[,c("50Cocos","50Natives","50Scaevola","50Sand")],kudframe[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])

mcpwiframe = data.frame(matrix(nrow=length(unique(crabs2017$CrabNum)),ncol=8))
kudwiframe = data.frame(matrix(nrow=length(unique(crabs2017$CrabNum)),ncol=8))

colnames(mcpwiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")
colnames(kudwiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")


for (crab in 1:nrow(mcpwiframe)) {
  #ADEHabitat really doesn't like it when we have more than one zero in a column
  if (mcpframe[crab,"95Scaevola"]==0) {
    mcpframe[crab,"95Scaevola"]=1
  }
  if(mcpframe[crab,"95Sand"]==0) {
    mcpframe[crab,"95Sand"]=1
  }
  if (mcpframe[crab,"50Scaevola"]==0) {
    mcpframe[crab,"50Scaevola"]=1
  }
  if(mcpframe[crab,"50Sand"]==0) {
    mcpframe[crab,"50Sand"]=1
  }
  if (kudframe[crab,"95Scaevola"]==0) {
    kudframe[crab,"95Scaevola"]=1
  }
  if(kudframe[crab,"95Sand"]==0) {
    kudframe[crab,"95Sand"]=1
  }
  if (kudframe[crab,"50Scaevola"]==0) {
    kudframe[crab,"50Scaevola"]=1
  }
  if(kudframe[crab,"50Sand"]==0) {
    kudframe[crab,"50Sand"]=1
  }
  mcp95ratios = widesIII(mcpframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],mcpframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud95ratios = widesIII(kudframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],kudframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
 
  mcp50ratios = widesIII(mcpframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],mcpframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud50ratios = widesIII(kudframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],kudframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  
  mcpwiframe[crab,"95CocosWI"] = mcp95ratios$wi[1]
  mcpwiframe[crab,"95NativesWI"] = mcp95ratios$wi[2]
  mcpwiframe[crab,"95ScaevolaWI"] = mcp95ratios$wi[3]
  mcpwiframe[crab,"95SandWI"] = mcp95ratios$wi[4]
  
  mcpwiframe[crab,"50CocosWI"] = mcp50ratios$wi[1]
  mcpwiframe[crab,"50NativesWI"] = mcp50ratios$wi[2]
  mcpwiframe[crab,"50ScaevolaWI"] = mcp50ratios$wi[3]
  mcpwiframe[crab,"50SandWI"] = mcp50ratios$wi[4]
  
  kudwiframe[crab,"95CocosWI"] = kud95ratios$wi[1]
  kudwiframe[crab,"95NativesWI"] = kud95ratios$wi[2]
  kudwiframe[crab,"95ScaevolaWI"] = kud95ratios$wi[3]
  kudwiframe[crab,"95SandWI"] = kud95ratios$wi[4]
  
  kudwiframe[crab,"50CocosWI"] = kud50ratios$wi[1]
  kudwiframe[crab,"50NativesWI"] = kud50ratios$wi[2]
  kudwiframe[crab,"50ScaevolaWI"] = kud50ratios$wi[3]
  kudwiframe[crab,"50SandWI"] = kud50ratios$wi[4]
}



range(mcpwiframe$`95NativesWI`)
range(mcpwiframe$`50NativesWI`)
range(kudwiframe$`95NativesWI`)
range(kudwiframe$`50NativesWI`)

mean(mcpwiframe$`95NativesWI`)
mean(mcpwiframe$`50NativesWI`)
mean(kudwiframe$`95NativesWI`)
mean(kudwiframe$`50NativesWI`)

mean(mcpwiframe$`95CocosWI`)
mean(mcpwiframe$`50CocosWI`)
mean(kudwiframe$`95CocosWI`)
mean(kudwiframe$`50CocosWI`)

mcpframe<- read.csv("MCP-95-and-50-crabs-palmyra.csv")
kudframe<- read.csv("KUD-95-and-50-crabs-palmyra.csv")
mcpwiframe<- read.csv("MCPWI-95-and-50-crabs-palmyra.csv")
kudwiframe<- read.csv("KUDWI-95-and-50-crabs-palmyra.csv")










#PLOTS
library(plyr)
library(dplyr)
library(reshape)
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

meltmcpwiframe <- melt(mcpwiframe,id="X")
meltmcpwiframe$type = revalue(substr(meltmcpwiframe$variable,1,3),c("X95" = "MCP95","X50" = "MCP50"))
meltmcpwiframe$variable = substr(meltmcpwiframe$variable,4,100)
dfmcp <- data_summary(meltmcpwiframe,varname="value",groupnames=c("type","variable"))

meltkudwiframe <- melt(kudwiframe,id="X")
meltkudwiframe$type = revalue(substr(meltkudwiframe$variable,1,3),c("X95" = "KUD95","X50" = "KUD50"))
meltkudwiframe$variable = substr(meltkudwiframe$variable,4,100)
dfkud <- data_summary(meltkudwiframe,varname="value",groupnames=c("type","variable"))

dfall = rbind(dfmcp,dfkud)

p <- ggplot(data=dfall,aes(x=variable,y=value,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9))
p

mcpwiframe

#write.csv(mcpframe,"MCP-95-and-50-crabs-palmyra.csv")
#write.csv(kudframe,"KUD-95-and-50-crabs-palmyra.csv")
#write.csv(mcpwiframe,"MCPWI-95-and-50-crabs-palmyra.csv")
#write.csv(kudwiframe,"KUDWI-95-and-50-crabs-palmyra.csv")