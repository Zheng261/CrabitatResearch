library(geosphere) 
library(adehabitatHS)

# Import smoothed classification image:
classed <- raster("6.26MASKEDPalmyra-RF-classification-v3-5x5-modal.tif") # Took 5x5 MODAL average

# Initialized crab tracking import
newWD = "../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks/waterspeedtrimmed"
crabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks/waterspeedtrimmed")
importantValues = c("Date","Time","Latitude","Longitude","Altitude","Speed","Course","Distance","CrabNum","Island")
otherCrabList = list.files("../Palmyra Crab Research/Crab tagging/crab tracks 2016/crab tracks")

#Classifies crabs by island
islands = c("sand","cooper","eastern","paradise")
islandList = c()
for (island in islands) {
  islandList[as.integer(substr(otherCrabList[grep(island,otherCrabList)],6,7))] = island
}

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
  allLocations[isle,"Cocos"] = totalavailhab[1]/sum(totalavailhab)
  allLocations[isle,"Natives"] = totalavailhab[2]/sum(totalavailhab)
  allLocations[isle,"Scaevola"] = totalavailhab[3]/sum(totalavailhab)
  allLocations[isle,"Sand"] = totalavailhab[4]/sum(totalavailhab)
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

kudoffset = 100
fiftyoffset = 50

#Begins calculating and plotting home range v. crab tracks 
#pdf("CrabitatNoTraxHomeRangeIndividual.pdf")
#Generates a list to add different HR estimates to
crabHRList = list()
for (crab in unique(crabs2016$CrabNum)) {
  #Converts to spatial points
  crab1 = crabs2016[which(crabs2016$CrabNum == crab),];
  crab1.sp = SpatialPoints(coords = crab1[,c("Longitude","Latitude")], proj4string = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')) # convert to SpatialPoints object
  crab1.spt <- spTransform(crab1.sp,CRS("+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  #Constructs mcp16 for crab 1 tracks
  polygon <- mcp(crab1.spt, percent=95)
  crabHRList[crab] = polygon
  polygon2 <- mcp(crab1.spt, percent=50)
  crabHRList[crab+fiftyoffset] = polygon2
  
  #Constructs kernel utilization density of crab tracks
  kud16 <- kernelUD(crab1.spt, grid=100, h="href") # NOTE: look into changing grid value. Currently I think it's too big because the estimated homerange (plotted below) looks too big. Either grid or h needs tweaking.
  
  #Finds area of polygon
  #areaPolygon(polygon)
  
  #Plots mcp16 for crab 1 with respect to the percentage of crab tracks included
  #hrs <- mcp16.area(crab1.sp, percent=seq(50, 100, by = 5)) 
  #plot(hrs) 
  
  #Plots mcp (95/50) over island and crab tracks
  #plotRGB(imgtest, r=1, g=2, b=3,stretch="lin", main="mcp16 95/50")
  #plot(polygon, add=TRUE, col = 2)
  #plot(polygon2, add=TRUE, col = 4)
  
  #Plots kud over island and crab tracks
  #plotRGB(imgtest, r=1, g=2, b=3,stretch="lin", main="kud16 95/50")
  ver <- getverticeshr(kud16, 95)
  #plot(ver, add=TRUE, col=rainbow(4)[1])
  crabHRList[crab+kudoffset] = ver
  ver2 <- getverticeshr(kud16, 50)
  #plot(ver2, add=TRUE, col=rainbow(4)[2])
  crabHRList[crab+fiftyoffset+kudoffset] = ver2
}
#dev.off()


#Calculates kud for crabs
#image(kud16)
#class(kud16)
#str(kud16)
#legend(699000, 3165000, legend = names(ud), fill = rainbow(4))


#Stores list of islands to be used for home range masking 
islandRasterList <- list()
for (isle in islands) {
  classedIsle = raster(paste0(isle,".tif"))
  islandRasterList[isle] <- classedIsle
}

#Creates data frames to hold crab habitat association data using home ranges
mcp16frame = data.frame(matrix(nrow=length(unique(crabs2016$CrabNum)),ncol=14))
kud16frame = data.frame(matrix(nrow=length(unique(crabs2016$CrabNum)),ncol=14))
colnames(mcp16frame) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
colnames(kud16frame) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
mcp16frame$CrabNum = unique(crabs2016$CrabNum)
kud16frame$CrabNum = unique(crabs2016$CrabNum)
View(head(crabs2016))
#Calculates amount of available habitat in crab home range (95%/50% mcp16)
for (crab in 1:nrow(mcp16frame)) {
  mcp16frame[crab,"Island"] = crabs2016[crabs2016$CrabNum==mcp16frame[crab,"CrabNum"],][1,"Island"]
  mcp16frame[crab,"AvailCocos"] = allLocations[mcp16frame[crab,"Island"],"Cocos"]
  mcp16frame[crab,"AvailNatives"] = allLocations[mcp16frame[crab,"Island"],"Natives"]
  mcp16frame[crab,"AvailScaevola"] = allLocations[mcp16frame[crab,"Island"],"Scaevola"]
  mcp16frame[crab,"AvailSand"] = allLocations[mcp16frame[crab,"Island"],"Sand"]
  thisCrabIsland = islandRasterList[[mcp16frame[crab,"Island"]]]
  #95% mcp16
  maskedIsland95 = mask(thisCrabIsland,crabHRList[[mcp16frame[crab,"CrabNum"]]])
  
  masked95pts <- rasterToPoints(maskedIsland95, spatial=TRUE)
  masked95tb <- table(masked95pts@data)  
  masked95tb[c("0","1","2","5")[is.na(masked95tb[c("0","1","2","5")])]] = 0
  masked95tb = masked95tb[c("0","1","2","5")]
  mcp16frame[crab,c("95Cocos","95Natives","95Scaevola","95Sand")] <- masked95tb
  #50% mcp16
  maskedIsland50 = mask(thisCrabIsland,crabHRList[[mcp16frame[crab,"CrabNum"]+fiftyoffset]])
  masked50pts <- rasterToPoints(maskedIsland50, spatial=TRUE)
  masked50tb <- table(masked50pts@data)
  masked50tb[c("0","1","2","5")[is.na(masked50tb[c("0","1","2","5")])]] = 0
  masked50tb = masked50tb[c("0","1","2","5")]
  mcp16frame[crab,c("50Cocos","50Natives","50Scaevola","50Sand")] <- masked50tb
}


#Calculates amount of available habitat in crab home range (95%/50% kernel)
for (crab in 1:nrow(kud16frame)) {
  kud16frame[crab,"Island"] = crabs2016[crabs2016$CrabNum==kud16frame[crab,"CrabNum"],][1,"Island"]
  kud16frame[crab,"AvailCocos"] = allLocations[kud16frame[crab,"Island"],"Cocos"]
  kud16frame[crab,"AvailNatives"] = allLocations[kud16frame[crab,"Island"],"Natives"]
  kud16frame[crab,"AvailScaevola"] = allLocations[kud16frame[crab,"Island"],"Scaevola"]
  kud16frame[crab,"AvailSand"] = allLocations[kud16frame[crab,"Island"],"Sand"]
  thisCrabIsland = islandRasterList[[kud16frame[crab,"Island"]]]
  #95% kernel
  maskedIsland95 = mask(thisCrabIsland,crabHRList[[kud16frame[crab,"CrabNum"]+kudoffset]])
  masked95pts <- rasterToPoints(maskedIsland95, spatial=TRUE)
  masked95tb <- table(masked95pts@data)  
  masked95tb[c("0","1","2","5")[is.na(masked95tb[c("0","1","2","5")])]] = 0
  masked95tb = masked95tb[c("0","1","2","5")]
  kud16frame[crab,c("95Cocos","95Natives","95Scaevola","95Sand")] <- masked95tb
  #50% kernel
  maskedIsland50 = mask(thisCrabIsland,crabHRList[[kud16frame[crab,"CrabNum"]+kudoffset+fiftyoffset]])
  masked50pts <- rasterToPoints(maskedIsland50, spatial=TRUE)
  masked50tb <- table(masked50pts@data)
  masked50tb[c("0","1","2","5")[is.na(masked50tb[c("0","1","2","5")])]] = 0
  masked50tb = masked50tb[c("0","1","2","5")]
  kud16frame[crab,c("50Cocos","50Natives","50Scaevola","50Sand")] <- masked50tb
}

#Creates new data frames to hold individual selection ratios
mcp16wiframe = data.frame(matrix(nrow=length(unique(crabs2016$CrabNum)),ncol=8))
kud16wiframe = data.frame(matrix(nrow=length(unique(crabs2016$CrabNum)),ncol=8))

colnames(mcp16wiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")
colnames(kud16wiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")

#Calculates individual selection ratios for each crab 
for (crab in 1:nrow(mcp16wiframe)) {
  #ADEHabitat really doesn't like it when we have more than one zero in a column
  if (FALSE) {
  if (mcp16frame[crab,"95Scaevola"]==0) {
    mcp16frame[crab,"95Scaevola"]=1
  }
  if(mcp16frame[crab,"95Sand"]==0) {
    mcp16frame[crab,"95Sand"]=1
  }
  if (mcp16frame[crab,"50Scaevola"]==0) {
    mcp16frame[crab,"50Scaevola"]=1
  }
  if(mcp16frame[crab,"50Sand"]==0) {
    mcp16frame[crab,"50Sand"]=1
  }
  if (kud16frame[crab,"95Scaevola"]==0) {
    kud16frame[crab,"95Scaevola"]=1
  }
  if(kud16frame[crab,"95Sand"]==0) {
    kud16frame[crab,"95Sand"]=1
  }
  if (kud16frame[crab,"50Scaevola"]==0) {
    kud16frame[crab,"50Scaevola"]=1
  }
  if(kud16frame[crab,"50Sand"]==0) {
    kud16frame[crab,"50Sand"]=1
  }
  }
  mcp1695ratios = widesIII(mcp16frame[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],mcp16frame[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1695ratios = widesIII(kud16frame[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],kud16frame[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  
  mcp1650ratios = widesIII(mcp16frame[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],mcp16frame[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1650ratios = widesIII(kud16frame[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],kud16frame[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  
  mcp16wiframe[crab,"95CocosWI"] = mcp1695ratios$wi[1]
  mcp16wiframe[crab,"95NativesWI"] = mcp1695ratios$wi[2]
  mcp16wiframe[crab,"95ScaevolaWI"] = mcp1695ratios$wi[3]
  mcp16wiframe[crab,"95SandWI"] = mcp1695ratios$wi[4]
  
  mcp16wiframe[crab,"50CocosWI"] = mcp1650ratios$wi[1]
  mcp16wiframe[crab,"50NativesWI"] = mcp1650ratios$wi[2]
  mcp16wiframe[crab,"50ScaevolaWI"] = mcp1650ratios$wi[3]
  mcp16wiframe[crab,"50SandWI"] = mcp1650ratios$wi[4]
  
  kud16wiframe[crab,"95CocosWI"] = kud1695ratios$wi[1]
  kud16wiframe[crab,"95NativesWI"] = kud1695ratios$wi[2]
  kud16wiframe[crab,"95ScaevolaWI"] = kud1695ratios$wi[3]
  kud16wiframe[crab,"95SandWI"] = kud1695ratios$wi[4]
  
  kud16wiframe[crab,"50CocosWI"] = kud1650ratios$wi[1]
  kud16wiframe[crab,"50NativesWI"] = kud1650ratios$wi[2]
  kud16wiframe[crab,"50ScaevolaWI"] = kud1650ratios$wi[3]
  kud16wiframe[crab,"50SandWI"] = kud1650ratios$wi[4]
}

range(mcp16wiframe$`95NativesWI`)
range(mcp16wiframe$`50NativesWI`)
range(kud16wiframe$`95NativesWI`)
range(kud16wiframe$`50NativesWI`)

mean(mcp16wiframe$`95NativesWI`)
mean(mcp16wiframe$`50NativesWI`)
mean(kud16wiframe$`95NativesWI`)
mean(kud16wiframe$`50NativesWI`)

mean(mcp16wiframe$`95CocosWI`)
mean(mcp16wiframe$`50CocosWI`)
mean(kud16wiframe$`95CocosWI`)
mean(kud16wiframe$`50CocosWI`)


mcp16frame<- read.csv("propmcp16-95-and-50-crabs-palmyra.csv")
kud16frame<- read.csv("propkud16-95-and-50-crabs-palmyra.csv")
mcp16wiframe<- read.csv("mcp16WI-95-and-50-crabs-palmyra.csv")
kud16wiframe<- read.csv("kud16WI-95-and-50-crabs-palmyra.csv")
colnames(kud16frame) <- c("CrabNum","Island" ,"95Cocos", "95Natives"  ,   "95Scaevola" ,   "95Sand",       
                          "50Cocos"   ,    "50Natives"  ,   "50Scaevola"   , "50Sand"   ,     "AvailCocos" ,   "AvailNatives" ,
                          "AvailScaevola" ,"AvailSand")
colnames(mcp16frame) <- c("CrabNum","Island" ,"95Cocos", "95Natives"  ,   "95Scaevola" ,   "95Sand",       
                          "50Cocos"   ,    "50Natives"  ,   "50Scaevola"   , "50Sand"   ,     "AvailCocos" ,   "AvailNatives" ,
                          "AvailScaevola" ,"AvailSand")

#mcp16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")] = mcp16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")]/rowSums(mcp16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")])
#mcp16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")] = mcp16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")]/rowSums(mcp16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")])
#kud16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")] = kud16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")]/rowSums(kud16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")])
#kud16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")] = kud16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")]/rowSums(kud16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")])

#Calculates widesIII selection ratios for each individual home range assessment
widesIII(mcp16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")],mcp16frame[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
widesIII(mcp16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")],mcp16frame[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
widesIII(kud16frame[,c("95Cocos","95Natives","95Scaevola","95Sand")],kud16frame[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
widesIII(kud16frame[,c("50Cocos","50Natives","50Scaevola","50Sand")],kud16frame[,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])

sum(mcp16frame$'95Cocos')

#Constructs bar plots for coconut crabs 
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

meltmcp16wiframe <- melt(mcp16wiframe,id="X")
meltmcp16wiframe$type = revalue(substr(meltmcp16wiframe$variable,1,3),c("X95" = "mcp1695","X50" = "mcp1650"))
meltmcp16wiframe$variable = substr(meltmcp16wiframe$variable,4,100)
dfmcp16 <- data_summary(meltmcp16wiframe,varname="value",groupnames=c("type","variable"))

meltkud16wiframe <- melt(kud16wiframe,id="X")
meltkud16wiframe$type = revalue(substr(meltkud16wiframe$variable,1,3),c("X95" = "kud1695","X50" = "kud1650"))
meltkud16wiframe$variable = substr(meltkud16wiframe$variable,4,100)
dfkud16 <- data_summary(meltkud16wiframe,varname="value",groupnames=c("type","variable"))

dfall = rbind(dfmcp16,dfkud16)

p <- ggplot(data=dfall,aes(x=variable,y=value,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9))
p


#write.csv(mcp16frame,"propmcp16-95-and-50-crabs-palmyra.csv")
#write.csv(kud16frame,"propkud16-95-and-50-crabs-palmyra.csv")
#write.csv(mcp16wiframe,"mcp16WI-95-and-50-crabs-palmyra.csv")
#write.csv(kud16wiframe,"kud16WI-95-and-50-crabs-palmyra.csv")