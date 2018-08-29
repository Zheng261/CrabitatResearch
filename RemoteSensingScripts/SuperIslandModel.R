####Multi-island forest model ####
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)

glcmnames <- readRDS("GLCMNames.rdat")
fanning <- brick("8.22-11x11WaterTrimmedFanning.tif")
teraina <- brick("8.22-11x11WaterTrimmedTeraina.tif")
palmyra <- brick("8.22-11x11TrimmedPalmyra.tif")
names(fanning) <- glcmnames
names(teraina) <- glcmnames
names(palmyra) <- glcmnames

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
#rf.mdl.mask <- randomForest(x=allTrainingData[,c(3:6,28:34)], y=as.factor(allTrainingData[,"isWater"]), ntree=500, importance=TRUE, progress="window")
# Classify the image with the above RF model that targets only LAND vs WATER
#palmyramask = predict(palmyra, rf.mdl.mask, filename="8.24-SMPalmyraWaterMask.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
#plot(palmyramask)
#varImpPlot(rf.mdl.mask, sort=TRUE, type=2, scale=TRUE)
#View(importance(rf.mdl))
#crappyLandvWater = raster("8.21-FanningWaterMask.tif")
#This kind of takes forever and idk why
#crappyLandOnly = raster::mask(testimg,crappyLandvWater,filename="8.22-WaterTrimmedFanning.tif",maskvalue=2,updatevalue=NA,overwrite=TRUE)
#names(crappyLandOnly) <- names(testimg)
#plotRGB(crappyLandOnly,r=1,b=2,g=3,stretch="hist")
#subset(crappyLandOnly,1)
#dev.off()

## REMOVES ALL WATER FROM TRAINING SET ###
allTrainingData= subset(allTrainingData,Class!=4 & Class!=3)

#Reads in info containing how important each band is in determining accuracy 
bandOrderInfo <- read.csv("8.22OrderOfImportanceALLISLANDBands.csv")
#as.character(bandOrderInfo[c(1:24),1])
rf.mdl <-randomForest(x=allTrainingData[,as.character(bandOrderInfo[c(1:24),1])],y=as.factor(droplevels(allTrainingData[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")

# Check error convergence. These "Out of bag" errors are a built in feature of random forest that tells you roughly how well your algorithm is doing
#plot(rf.mdl, main="Out-of-bag errors for 16-feature RF model")#, xlab="Number of trees grown", ylab="OOB error")

PalmyraPred <- predict(palmyra, rf.mdl, type="response", filename="8.24-11x11SMPalmyra.tif",index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
TerainaPred <- predict(teraina, rf.mdl, type="response", filename="8.24-11x11SMTeraina.tif",index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
FanningPred <- predict(fanning, rf.mdl, type="response", filename="8.24-11x11SMFanning.tif",index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

varImpPlot(rf.mdl, sort=TRUE, type=2, scale=TRUE)
#View(importance(rf.mdl))

# Here I like to average together the MDA and MDG accuracy scores and use that ranking as my new basis for feature selection
var.score <- data.frame(importance(rf.mdl)[,5],importance(rf.mdl)[,6]) # make new dataframe to combine mda and mdg scores
var.score$mdarank   <- rank(var.score$importance.rf.mdl....5.)
var.score$mdgrank   <- rank(var.score$importance.rf.mdl....6.)
var.score$avgrank   <- ( var.score$mdarank + var.score$mdgrank ) / 2
var.score = var.score[order(var.score$avgrank,decreasing=TRUE),]
View(var.score) # Higher ranking is better
#Checks how the important bands change as we expand GLCM radius
#rownames(var.score)[1:20]%in%as.character(bandOrderInfo[,1])[1:20]
#write.csv(var.score,"8.22OrderOfImportanceALLISLANDBands.csv")

##### Calculates confusion matrix - OOB #######
# OOB calculation
nvariables = 4
conf <- rf.mdl$confusion
conf <- data.frame(conf)
conf$Accuracy = 0
conf$Precision = 0
colnames(conf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure", "Error", "Accuracy", "Precision")
rownames(conf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure")
for (i in 1:nrow(conf)) {
  numSamples = 0
  for (j in 1:nvariables) {
    numSamples = numSamples + conf[i,j]
  }
  conf$Accuracy[i] = conf[i,i]/numSamples
  conf$Precision[i] = conf[i,i]/sum(conf[,i])
}

View(conf)
mean(conf$Accuracy)
mean(conf$Precision)

#### PALMYRA TESTING VERIFICATION ####
valData <- readOGR(dsn = "/volumes/Seagate 4tb/Palmyra Remote Sensing/palmyra-2016-validation-points-v2.shp", layer = "palmyra-2016-validation-points-v2")
#ogrInfo(dsn = "palmyra-2016-validation-points-v2.shp",layer="palmyra-2016-validation-points-v2")
#valData <- shapefile("palmyra-2016-validation-points-v2.shp")
valData <- subset(valData, select = landcover)
valData <- spTransform(valData,CRS("+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
View(valData)
# Assign classification values to corresponding validation pixels
valData$classified <- as.data.frame(extract(PalmyraPred, valData))
pred <- list("0")
truth<- list("0")

for (i in 1:nrow(valData)){
  if (!is.na(valData@data[i,2])) {
    pred[i] <- valData@data[i,2]
    truth[i]<- as.numeric(levels(valData@data[i,1])[valData@data[i,1]])
  } else {
    pred[i] <- -1
    truth[i] <- -1
  }
}

### Grabs all of the predicted and actual values, and puts them side by side
testData = data.frame(matrix(unlist(pred), nrow=611, byrow=T)) %>% cbind(data.frame(matrix(unlist(truth), nrow=611, byrow=T)))
testData = subset(testData,Truth != -1)
colnames(testData) = c("Predicted","Truth")

#### Testing set accuracy calculation
testconf <- data.frame(matrix(ncol=nvariables+3,nrow=nvariables))
colnames(testconf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure", "Error", "Accuracy","Precision")
rownames(testconf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure")

for (i in 1:nrow(testconf)) {
  Total = testData[which(testData$Truth == sort(unique(testData$Truth))[i]),]
  numSamples = 0
  for (j in 1:nvariables) {
    testconf[i,j] = nrow(Total[which(Total$Predicted == sort(unique(testData$Truth))[j]),])
    numSamples = numSamples + testconf[i,j]
  }
  testconf$Accuracy[i] = testconf[i,i]/numSamples
}
for (i in 1:nrow(testconf)) {
  testconf$Error[i] = 1-testconf$Accuracy[i]
  testconf$Precision[i] = testconf[i,i]/sum(testconf[,i])
}
View(testconf)
mean(testconf$Accuracy)
mean(testconf$Precision)

saveRDS(rf.mdl,"8.24randomForestSMPalmTerrFann.RDS")

## PlanetScope Data ##
islands = c("Sand","Cooper","Paradise","Eastern")
allLocations = data.frame(matrix(ncol=4,nrow=4))
colnames(allLocations) = c("Cocos","Natives","Scaevola","Unveg")
rownames(allLocations) = islands

allLocationsPS = data.frame(matrix(ncol=4,nrow=3))
colnames(allLocationsPS) = c("Cocos","Natives","Scaevola","Unveg")
rownames(allLocationsPS) = c("Palmyra","Terraina","Fanning")

#Gets points from all the rasters
palmyrapts = rasterToPoints(PalmyraPred)
terainapts = rasterToPoints(TerainaPred)
fanningpts = rasterToPoints(FanningPred)
totalavailhabPalmyra = table(palmyrapts[,3])
totalavailhabTeraina = table(terainapts[,3])
totalavailhabFanning = table(fanningpts[,3])

allLocationsPS[1,"Cocos"] = totalavailhabPalmyra[1]/sum(totalavailhabPalmyra)
allLocationsPS[1,"Natives"] = totalavailhabPalmyra[2]/sum(totalavailhabPalmyra)
allLocationsPS[1,"Scaevola"] = totalavailhabPalmyra[3]/sum(totalavailhabPalmyra)
allLocationsPS[1,"Unveg"] = totalavailhabPalmyra[4]/sum(totalavailhabPalmyra)

allLocationsPS[2,"Cocos"] = totalavailhabTeraina[1]/sum(totalavailhabTeraina)
allLocationsPS[2,"Natives"] = totalavailhabTeraina[2]/sum(totalavailhabTeraina)
allLocationsPS[2,"Scaevola"] = totalavailhabTeraina[3]/sum(totalavailhabTeraina)
allLocationsPS[2,"Unveg"] = totalavailhabTeraina[4]/sum(totalavailhabTeraina)

allLocationsPS[3,"Cocos"] = totalavailhabFanning[1]/sum(totalavailhabFanning)
allLocationsPS[3,"Natives"] = totalavailhabFanning[2]/sum(totalavailhabFanning)
allLocationsPS[3,"Scaevola"] = totalavailhabFanning[3]/sum(totalavailhabFanning)
allLocationsPS[3,"Unveg"] = totalavailhabFanning[4]/sum(totalavailhabFanning)

#Grabs all the palmyra data, split by island
for (isle in islands) {
  classedIsle = raster(paste0(isle,".tif"))
  islecoordpts <- rasterToPoints(classedIsle, spatial=TRUE)
  #plot(crappyForestPred)
  #plot(islecoordpts,add=TRUE)
  islecoordpts@data <- data.frame(islecoordpts@data, long=coordinates(islecoordpts)[,1],
                                  lat=coordinates(islecoordpts)[,2], NA)
  colnames(islecoordpts@data) <- c("digitalglobe","long","lat","planetscope")
  islecoordpts@data[,"planetscope"] = raster::extract(PalmyraPred, data.frame(islecoordpts@data[,c("long","lat")]),method="simple")
  totalavailhab = table(islecoordpts@data[,"planetscope"])
  allLocations[isle,"Cocos"] = totalavailhab[1]/sum(totalavailhab)
  allLocations[isle,"Natives"] = totalavailhab[2]/sum(totalavailhab)
  allLocations[isle,"Scaevola"] = totalavailhab[3]/sum(totalavailhab)
  allLocations[isle,"Unveg"] = totalavailhab[4]/sum(totalavailhab)
}

allLocationsPS
allLocations


### I wrote some code to calculate the accuracy per island, but that don't work


## Ignore everything below ##
#### OOB Confusion Matrices ####
xpts = palmyraTrainingDataOrig
xpts@data[,"landcover"] = as.factor(c(0,1,2,5))
palmClass <- as.data.frame(raster::extract(PalmyraPred,palmyraTrainingDataOrig),drop=FALSE)
terainaClass <- as.data.frame(raster::extract(TerainaPred, terainaTrainingDataOrig),drop=FALSE)
fanningClass <- as.data.frame(raster::extract(FanningPred, fanningTrainingDataOrig),drop=FALSE)

palmyraIndClass = data.frame(palmyraTrainingDataOrig@data, palmClass[match(rownames(palmyraTrainingDataOrig@data), rownames(palmClass)),])

#Maybe fix these column names if we publish this code
palmyraIndvSuperClass <- cbind(data.frame(palmyraTrainingDataOrig@data[,"landcover"]),palmClass)
terainaIndvSuperClass <- cbind(data.frame(terainaTrainingDataOrig@data[,"id"]),terainaClass)
fanningIndvSuperClass <- cbind(data.frame(fanningTrainingDataOrig@data[,"id"]),fanningClass)
colnames(palmyraIndvSuperClass) <- c("Individual","Super")
colnames(terainaIndvSuperClass) <- c("Individual","Super")
colnames(fanningIndvSuperClass) <- c("Individual","Super")
palmyraIndvSuperClass <- palmyraIndvSuperClass[complete.cases(palmyraIndvSuperClass),]
terainaIndvSuperClass <- terainaIndvSuperClass[complete.cases(terainaIndvSuperClass),]
fanningIndvSuperClass <- fanningIndvSuperClass[complete.cases(fanningIndvSuperClass),]

palmyraIndvSuperClass <- subset(palmyraIndvSuperClass,Individual != 3 & Individual != 4)
terainaIndvSuperClass <- subset(terainaIndvSuperClass,Individual != 3 & Individual != 4)
fanningIndvSuperClass <- subset(fanningIndvSuperClass,Individual != 3 & Individual != 4)

confusionMatrix(as.factor(droplevels(palmyraIndvSuperClass[,1])),as.factor(palmyraIndvSuperClass[,2]))
confusionMatrix(as.factor(droplevels(terainaIndvSuperClass[,1])),as.factor(terainaIndvSuperClass[,2]))
confusionMatrix(as.factor(droplevels(fanningIndvSuperClass[,1])),as.factor(fanningIndvSuperClass[,2]))
