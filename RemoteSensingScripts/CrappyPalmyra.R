setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)

testimg <- brick("CrappyPalmyraClipped.tif")
names(testimg) <- c("Blue","Green","Red","IR")
testimg <- subset(testimg, order(c(3,2,1,4)))
plotRGB(testimg,stretch="lin")

####### GLCM ON GRAYSCALE ########
#testimgbw <- (testimg$Red + testimg$Green + testimg$Blue ) / 3
#glcmtestimg <- glcm(testimgbw ,window=c(5,5))
#names(glcmtestimg) <- paste("GrayScale.",names(glcmtestimg))
#glcmtestimg <- dropLayer(glcmtestimg,8)
#testimg <- addLayer(testimg,glcmtestimg)

####### GLCM ON RED ########
glcmtestimg <- glcm(testimg$Red ,window=c(11,11))
names(glcmtestimg) <- paste("Red.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

####### GLCM ON GREEN ########
glcmtestimg <- glcm(testimg$Green ,window=c(11,11))
names(glcmtestimg) <- paste("Green.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

####### GLCM ON BLUE########
glcmtestimg <- glcm(testimg$Blue,window=c(11,11))
names(glcmtestimg) <- paste("Blue.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

####### GLCM ON INFRARED ########
glcmtestimg <- glcm(testimg$IR,window=c(11,11))
names(glcmtestimg) <- paste("IR.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

#writeRaster(testimg,"8-24-11x11wateryPalmyra.tif",overwrite=TRUE)

############# WATER MASKING ###############
crappyTrainingData <- readOGR(dsn = "PalmyraWater.shp", layer = "PalmyraWater")
crappyTrainingData = spTransform(crappyTrainingData,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(crappyTrainingData,add=TRUE,col="red")

#dataSet <- as.data.frame(extract(testimg, crappyTrainingData))
#crappyTrainingData@data = data.frame(crappyTrainingData@data, dataSet[match(rownames(crappyTrainingData@data), rownames(dataSet)),])
#colnames(crappyTrainingData@data)[which(colnames(crappyTrainingData@data)=="id")] = "isWater"
#crappyTrainingData@data = crappyTrainingData@data[which(!is.na(crappyTrainingData@data$isWater)),]
#View(crappyTrainingData)

#rf.mdl.mask <- randomForest(x=crappyTrainingData@data[,c(2:5,27:31)], y=as.factor(crappyTrainingData@data[,"isWater"]), ntree=200, importance=TRUE, progress="window")

# Classify the image with the above RF model that targets only LAND vs WATER
#crappyLandvWater = predict(testimg, rf.mdl.mask, type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
#filename="8.16-MaskForCrappyPalmyra.tif"
crappyLandvWater = raster("8.16-MaskForCrappyPalmyra.tif")
#plot(crappyLandvWater)
#varImpPlot(rf.mdl.mask, sort=TRUE, type=2, scale=TRUE)
#View(importance(rf.mdl))

#filename="8.17-WaterMaskedCrappyPalmyra.tif"
#This kind of takes forever and idk why
crappyLandOnly = raster::mask(testimg,crappyLandvWater,maskvalue=0,filename="8.22-11x11TrimmedPalmyra.tif",updatevalue=NA,overwrite=TRUE)
names(crappyLandOnly) <- names(testimg)
plotRGB(crappyLandOnly,r=1,b=2,g=3,stretch="hist")


#dev.off()
############# START TRAINING FOREST TYPES ###############
trainingData <- readOGR(dsn = "/volumes/Seagate 4tb/Palmyra Remote Sensing/palmyra-2016-truthing-points-v2.shp", layer = "palmyra-2016-truthing-points-v2")
trainingData <- trainingData[,-1] #removing some extra coordinate columns...
trainingData <- trainingData[,-1]
trainingData = spTransform(trainingData,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

trainingData$land <- 0
for (i in 1:nrow(trainingData)){
  if (trainingData$landcover[i] != 3 & trainingData$landcover[i] != 4){
    trainingData$land[i] = 1
  }
}
trainingData.2 <- subset(trainingData, land!= 0)

# Assign raster values to training data points
dataSet <- as.data.frame(extract(testimg, trainingData.2))

trainingData.2@data = cbind(trainingData.2@data,dataSet)

plot(trainingData.2,add=TRUE,cex=0.3)

#subset(trainingData.2@data,landcover==5)
#Reads in info containing how important each band is in determining accuracy 
bandOrderInfo <- read.csv("8.14OrderOfImportancePalmyraBands.csv")
#Try out different mtry values?
rf.mdl <-randomForest(x=trainingData.2@data[,as.character(bandOrderInfo[c(1:24),1])], y=as.factor(droplevels(trainingData.2@data[,"landcover"])),ntree=1200,na.action=na.omit, importance=TRUE, progress="window")

# Check error convergence. These "Out of bag" errors are a built in feature of random forest that tells you roughly how well your algorithm is doing
plot(rf.mdl, main="Out-of-bag errors for 16-feature RF model")#, xlab="Number of trees grown", ylab="OOB error")

crappyForestPred <- predict(crappyLandOnly , rf.mdl, filename="8.22PalmyraPrediction11x11.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(crappyForestPred)

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
#write.csv(var.score,"8.14OrderOfImportancePalmyraBands.csv")

############# TESTING ACCURACY ###############

#Read in validation points shapefile; get rid of extra coordinate columns as with above
valData <- readOGR(dsn = "/volumes/Seagate 4tb/Palmyra Remote Sensing/palmyra-2016-validation-points-v2.shp", layer = "palmyra-2016-validation-points-v2")
#ogrInfo(dsn = "palmyra-2016-validation-points-v2.shp",layer="palmyra-2016-validation-points-v2")
#valData <- shapefile("palmyra-2016-validation-points-v2.shp")
valData <- subset(valData, select = landcover)
valData <- spTransform(valData,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#View(valData)
# Assign classification values to corresponding validation pixels
valData$classified <- as.data.frame(extract(crappyForestPred, valData))


# Now, loop through classification values and reassign to their landcover equivalents based on 6 classes. This shouldn't be necessary if the above steps were done right lol
# These numbers are specific to my case and you can probably figure out how to get around doing this hacked-together thing that doesn't really work
# 0  -> 0 - Cocos
# 51 -> 1 - Native Trees
# 102-> 2 - Scaevola
# 154-> 3 - Shallows/reefs
# 205-> 4 - Open Water
# 255-> 5 - Sand/infrastructure
for (i in 1:nrow(valData)){
  if (!is.na(valData@data[i,2])) {
    if (valData@data[i,2] == 51){
      valData@data[i,2] <- 1
    }
    if (valData@data[i,2] == 102){
      valData@data[i,2] <- 2
    }
    if (valData@data[i,2] == 154){
      valData@data[i,2] <- 3
    }
    if (valData@data[i,2] == 205){
      valData@data[i,2] <- 4
    }
    if (valData@data[i,2] == 255){
      valData@data[i,2] <- 5
    }
  }
}

valData@data
testData = valData@data
colnames(testData) <- c("Truth","Predicted")
testData = testData[which(testData$Truth!=3),]
testData$Predicted[which(is.na(testData$Predicted)),] = -1

##### Calculates confusion matrix - OOB #######
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


######### Test Accuracy Calculation #########################
testconf <- data.frame(matrix(ncol=nvariables+3,nrow=nvariables))
colnames(testconf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure", "Error", "Accuracy","Precision")
rownames(testconf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure")
for (i in 1:nrow(testconf)) {
  Total = testData[which(testData$Truth == sort(unique(testData$Truth))[i]),]
  numSamples = 0
  for (j in 1:nvariables) {
    testconf[i,j] = nrow(Total[which(Total$Predicted == as.numeric(levels(testData$Truth)[as.numeric(sort(unique(testData$Truth))[j])])),])
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




### Comparison of overall island stats ###
#Begins analyzing island habitat distribution
## DigitalGlobe Data ##
#Loops through all islands, imports their QGIS-cropped images, and calculates their habitat ratios
islands = c("sand","cooper","eastern","paradise")

allLocations = data.frame(matrix(ncol=4,nrow=4))
colnames(allLocations) = c("Cocos","Natives","Scaevola","Sand")
rownames(allLocations) = islands

## PlanetScope Data ##
allLocationsPS = allLocations

for (isle in islands) {
  classedIsle = raster(paste0(isle,".tif"))
  islecoordpts <- rasterToPoints(classedIsle, spatial=TRUE)
  #plot(crappyForestPred)
  #plot(islecoordpts,add=TRUE)
  islecoordpts@data <- data.frame(islecoordpts@data, long=coordinates(islecoordpts)[,1],
                                  lat=coordinates(islecoordpts)[,2], NA)
  colnames(islecoordpts@data) <- c("digitalglobe","long","lat","planetscope")
  islecoordpts@data[,"planetscope"] = raster::extract(crappyForestPred, data.frame(islecoordpts@data[,c("long","lat")]),method="simple")
  totalavailhab = table(islecoordpts@data[,"digitalglobe"])
  allLocations[isle,"Cocos"] = totalavailhab[1]/sum(totalavailhab)
  allLocations[isle,"Natives"] = totalavailhab[2]/sum(totalavailhab)
  allLocations[isle,"Scaevola"] = totalavailhab[3]/sum(totalavailhab)
  allLocations[isle,"Sand"] = totalavailhab[4]/sum(totalavailhab)
  
  totalavailhab = table(islecoordpts@data[,"planetscope"])
  allLocationsPS[isle,"Cocos"] = totalavailhab[1]/sum(totalavailhab)
  allLocationsPS[isle,"Natives"] = totalavailhab[2]/sum(totalavailhab)
  allLocationsPS[isle,"Scaevola"] = totalavailhab[3]/sum(totalavailhab)
  allLocationsPS[isle,"Sand"] = totalavailhab[4]/sum(totalavailhab)
}
allLocationsPS/allLocations

wholePalmyraTif = raster("../Palmyra Remote Sensing/6.26MASKEDPalmyra-RF-classification-v3-5x5-modal.tif")
mikePalmyraDistribution <- table(wholePalmyraTif[,])
crappyPalmyraDistribution <- table(crappyForestPred[,])
mikePalmyraRatios <- mikePalmyraDistribution/sum(mikePalmyraDistribution)
crappyPalmyraRatios <- crappyPalmyraDistribution/sum(crappyPalmyraDistribution)

