####Multi-island water model####
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)
glcmnames <- readRDS("GLCMNames.rdat")

#Generates all-island water mask
#colorNormFrame = data.frame(matrix(nrow=6,ncol=4))
#colnames(colorNormFrame) <- c("Red","Green","Blue","IR")
#rownames(colorNormFrame) <- c("MeanPalmyra","STDevPalmyra","MeanTeraina","STDevTeraina","MeanFanning","STDevFanning")

palmyra <-brick("8-24-11x11wateryPalmyra.tif")
teraina <-brick("8-24-11x11wateryTeraina.tif")
fanning <-brick("8-24-11x11wateryFanning.tif")

names(fanning) <- glcmnames[c(1:4,26:32)]
names(teraina) <- glcmnames[c(1:4,26:32)]
names(palmyra) <- glcmnames[c(1:4,26:32)]


## FILTERS OUT HIGH VALUES, we temporarily do not do this ##
#for(color in colnames(colorNormFrame)) {
  #print(color)
  #mean = cellStats(palmyra[[color]],"mean")
  #sd = cellStats(palmyra[[color]],"sd")
  #All points 2 SD above mean are marked as true, or 1
  #highFilter = (palmyra[[color]]-mean) > 3*sd
  #setValues(palmyra,value=NA,index= which(highFilter[,] == 1))
  
  #mean = cellStats(teraina[[color]],"mean")
  #sd = cellStats(teraina[[color]],"sd")
  #All points 2 SD above mean are marked as true, or 1
  #highFilter = (teraina[[color]]-mean) > 3*sd
  #setValues(teraina,value=NA,index= which(highFilter[,] == 1))
  
  #mean = cellStats(fanning[[color]],"mean")
  #sd = cellStats(fanning[[color]],"sd")
  #All points 2 SD above mean are marked as true, or 1
  #highFilter = (fanning[[color]]-mean) > 3*sd
  #setValues(fanning,value=NA,index= which(highFilter[,] == 1))
#}

#for(color in colnames(colorNormFrame)) {
#  print(color)
#  print("Palmyra")
#  colorNormFrame["MeanPalmyra",color] <- cellStats(subset(palmyra,color),stat="mean")
#  colorNormFrame["STDevPalmyra",color] <- cellStats(subset(palmyra,color),stat="sd")
#  print("Teraina")
#  colorNormFrame["MeanTeraina",color] <- cellStats(subset(teraina,color),stat="mean")
#  colorNormFrame["STDevTeraina",color] <- cellStats(subset(teraina,color),stat="sd")
#  print("Fanning")
#  colorNormFrame["MeanFanning",color] <- cellStats(subset(fanning,color),stat="mean")
#  colorNormFrame["STDevFanning",color] <- cellStats(subset(fanning,color),stat="sd")
#}
#write.csv(colorNormFrame,"8-28-18PalmTerainaFanningColorsStatistics.csv")

if (FALSE) {
plotRGB(fanning,r=1,g=2,b=3)
for(color in colnames(colorNormFrame)) {
  print(color)
  
  #Multiplies every value by the ratio in standard deviation
  palmyra[[color]] = palmyra[[color]] * (colorNormFrame["STDevPalmyra",color]/mean(colorNormFrame[c("STDevPalmyra","STDevTeraina","STDevFanning"),color]))
  #Shifts down every value by the average mean of the islands
  palmyra[[color]] = palmyra[[color]] - (colorNormFrame["MeanPalmyra",color] - mean(colorNormFrame[c("MeanPalmyra","MeanTeraina","MeanFanning"),color]))
 
   #does the same for the others too
  #Multiplies every value by the difference in standard deviation
  teraina[[color]] = teraina[[color]] * (colorNormFrame["STDevTeraina",color]/mean(colorNormFrame[c("STDevPalmyra","STDevTeraina","STDevFanning"),color]))
  #Shifts down every value by the average mean of the islands
  teraina[[color]] = teraina[[color]] - (colorNormFrame["MeanTeraina",color] - mean(colorNormFrame[c("MeanPalmyra","MeanTeraina","MeanFanning"),color]))
  
  #Multiplies every value by the ratio in standard deviation
  fanning[[color]] = fanning[[color]] * (colorNormFrame["STDevFanning",color]/mean(colorNormFrame[c("STDevPalmyra","STDevTeraina","STDevFanning"),color]))
  #Shifts down every value by the average mean of the islands
  fanning[[color]] = fanning[[color]] - (colorNormFrame["MeanFanning",color] - mean(colorNormFrame[c("MeanPalmyra","MeanTeraina","MeanFanning"),color]))
}

writeRaster(palmyra,"8-29-11x11NORMwateryPalmyra.tif")
writeRaster(teraina,"8-29-11x11NORMwateryTeraina.tif")
writeRaster(fanning,"8-29-11x11NORMwateryFanning.tif")
}
#fanning <- brick("8-29-11x11NORMwateryFanning.tif")
#teraina <- brick("8-29-11x11NORMwateryFanning.tif")
#palmyra <- brick("8-29-11x11NORMwateryFanning.tif")

### FANNING ####
fanningTrainingDataOrig <- readOGR(dsn = "FanningTraining.shp", layer = "FanningTraining")
proj4string(fanningTrainingDataOrig) <- CRS("+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
fanningTrainingData = fanningTrainingDataOrig
#You get a warning here for some NA points. I'm not sure why this happens but I just remove them and then it works fine. Perhaps one of the points
#was accidentally classified as water and removed.
dataSet <- as.data.frame(raster::extract(fanning, fanningTrainingData))
fanningTrainingData@data = cbind(fanningTrainingData@data,fanningTrainingData@data[,c("id")]==4)
colnames(fanningTrainingData@data) <- c("Class","isWater")
fanningTrainingData@data = data.frame(fanningTrainingData@data, dataSet[match(rownames(fanningTrainingData@data), rownames(dataSet)),])

### TERAINA ####
terainaTrainingDataOrig <- readOGR(dsn = "TerainaTrainingScav.shp", layer = "TerainaTrainingScav")
#This shapefile for some reason was saved with the wrong CRS refernece - QGIS didn't convert any points before setting the CRS. 
proj4string(terainaTrainingDataOrig) <- CRS("+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#You get a warning here for some NA points. I'm not sure why this happens but I just remove them and then it works fine. Perhaps one of the points
#was accidentally classified as water and removed.
terainaTrainingData = terainaTrainingDataOrig
dataSet <- as.data.frame(extract(teraina, terainaTrainingData))
terainaTrainingData@data = cbind(terainaTrainingData@data,terainaTrainingData@data[,c("id")]==4)
colnames(terainaTrainingData@data) <- c("Class","isWater")
terainaTrainingData@data = data.frame(terainaTrainingData@data, dataSet[match(rownames(terainaTrainingData@data), rownames(dataSet)),])

### PALMYRA ####
palmyraTrainingDataOrig  <- readOGR(dsn = "/volumes/Seagate 4tb/Palmyra Remote Sensing/palmyra-2016-truthing-points-v2.shp", layer = "palmyra-2016-truthing-points-v2")
palmyraTrainingDataOrig = spTransform(palmyraTrainingDataOrig,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
palmyraTrainingData <- palmyraTrainingDataOrig[,-1] #removing some extra coordinate columns...
palmyraTrainingData <- palmyraTrainingData[,-1]

#You get a warning here for some NA points. I'm not sure why this happens but I just remove them and then it works fine. Perhaps one of the points
#was accidentally classified as water and removed.
dataSet <- as.data.frame(extract(palmyra, palmyraTrainingData))
palmyraTrainingData@data = cbind(palmyraTrainingData@data,palmyraTrainingData@data[,c("landcover")]==4 | palmyraTrainingData@data[,c("landcover")]==3)
colnames(palmyraTrainingData@data) <- c("Class","isWater")
palmyraTrainingData@data = data.frame(palmyraTrainingData@data, dataSet[match(rownames(palmyraTrainingData@data), rownames(dataSet)),])

## Removes NAs
palmyraTrainingData@data = palmyraTrainingData@data[complete.cases(palmyraTrainingData@data),]
terainaTrainingData@data = terainaTrainingData@data[complete.cases(terainaTrainingData@data),]
fanningTrainingData@data = fanningTrainingData@data[complete.cases(fanningTrainingData@data),]
allTrainingData = rbind(palmyraTrainingData@data,terainaTrainingData@data,fanningTrainingData@data)

#### LEARNS GENERALIZABLE WATER MASK ####
### Classify based on bands: RGB, IR, IR GLCM
rf.mdl.mask <- randomForest(x=allTrainingData[,c(6:13)], y=as.factor(allTrainingData[,"isWater"]), ntree=500, importance=TRUE, progress="window")
### Not normalized ####
#saveRDS(rf.mdl.mask,"10.13WATERNNrandomForestSMPalmTerrFann.RDS")

palmyramask = predict(palmyra, rf.mdl.mask, filename="8.29-SMPalmyraWaterMask.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(palmyramask)

