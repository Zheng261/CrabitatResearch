#### Fanning island classification ####

setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)

testimg <- brick("FanningClipped.tif")
names(testimg) <- c("Blue","Green","Red","IR")
testimg <- subset(testimg, order(c(3,2,1,4)))
plotRGB(testimg,stretch="lin")

### Runs GLCM. Window of 25*25 seems to give approximately optimal results for both Palmyra and Teraina.
#However, window of 11*11 doesn't discard all the class edges.
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

#writeRaster(testimg,"8-24-11x11wateryFanning.tif",overwrite=TRUE)

############# WATER MASKING ###############
crappyTrainingData <- readOGR(dsn = "FanningTraining.shp", layer = "FanningTraining")
#This shapefile for some reason was saved with the wrong CRS refernece - QGIS for some reason didn't convert any points before setting the CRS. 
proj4string(crappyTrainingData) <- CRS("+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#crappyTrainingData = spTransform(crappyTrainingData,"+proj=utm +zone=4 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(crappyTrainingData,add=TRUE,col="red")
#You get a warning here for some NA points. I'm not sure why this happens but I just remove them and then it works fine. Perhaps one of the points
#was accidentally classified as water and removed.
dataSet <- as.data.frame(extract(testimg, crappyTrainingData))
crappyTrainingData@data = cbind(crappyTrainingData@data,crappyTrainingData@data=="4")
colnames(crappyTrainingData@data) <- c("Class","isWater")
crappyTrainingData@data = data.frame(crappyTrainingData@data, dataSet[match(rownames(crappyTrainingData@data), rownames(dataSet)),])


## Removes NAs
crappyTrainingData@data = crappyTrainingData@data[complete.cases(crappyTrainingData@data),]

### Classify based on bands: RGB, IR,
#rf.mdl.mask <- randomForest(x=crappyTrainingData@data[,c(3:6,28:34)], y=as.factor(crappyTrainingData@data[,"isWater"]), ntree=200, importance=TRUE, progress="window")

# Classify the image with the above RF model that targets only LAND vs WATER
#crappyLandvWater = predict(testimg, rf.mdl.mask, filename="8.21-FanningWaterMask.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
#plot(crappyLandvWater)
#varImpPlot(rf.mdl.mask, sort=TRUE, type=2, scale=TRUE)
#View(importance(rf.mdl))
crappyLandvWater = raster("8.21-FanningWaterMask.tif")

#This kind of takes forever and idk why
crappyLandOnly = raster::mask(testimg,crappyLandvWater,filename="8.22-WaterTrimmedFanning.tif",maskvalue=2,updatevalue=NA,overwrite=TRUE)
names(crappyLandOnly) <- names(testimg)
plotRGB(crappyLandOnly,r=1,b=2,g=3,stretch="hist")
#subset(crappyLandOnly,1)
#dev.off()

############# START TRAINING FOREST TYPES ###############
#trainingData <- readOGR(dsn = "/volumes/Seagate 4tb/Palmyra Remote Sensing/palmyra-2016-truthing-points-v2.shp", layer = "palmyra-2016-truthing-points-v2")
#trainingData <- trainingData[,-1] #removing some extra coordinate columns...
#trainingData <- trainingData[,-1]
#trainingData = spTransform(trainingData,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
crappyTrainingData.2 <- subset(crappyTrainingData, isWater == FALSE)

#plot(crappyTrainingData.2,add=TRUE,cex=0.3)

#subset(trainingData.2@data,landcover==5)
#Reads in info containing how important each band is in determining accuracy 
bandOrderInfo <- read.csv("8.22OrderOfImportanceFanningBands.csv")
#as.character(bandOrderInfo[c(1:24),1])
#Try out different mtry values?
rf.mdl <-randomForest(x=crappyTrainingData.2@data[,as.character(bandOrderInfo[c(1:22),1])],y=as.factor(droplevels(crappyTrainingData.2@data[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")

# Check error convergence. These "Out of bag" errors are a built in feature of random forest that tells you roughly how well your algorithm is doing
plot(rf.mdl, main="Out-of-bag errors for 16-feature RF model")#, xlab="Number of trees grown", ylab="OOB error")

crappyForestPred <- predict(crappyLandOnly , rf.mdl, type="response", filename="8.23-11x11ClassifiedFanning.tif",index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
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
#write.csv(var.score,"8.22OrderOfImportanceFanningBands.csv")

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

## PlanetScope Data ##
allLocations = data.frame(matrix(ncol=4,nrow=1))
colnames(allLocations) = c("Cocos","Natives","Scaevola","Unveg")
rownames(allLocations) = c("Fanning")
allLocationsPS = allLocations

terainapts = rasterToPoints(crappyForestPred)
totalavailhab = table(terainapts[,3])
allLocationsPS[1,"Cocos"] = totalavailhab[1]/sum(totalavailhab)
allLocationsPS[1,"Natives"] = totalavailhab[2]/sum(totalavailhab)
allLocationsPS[1,"Scaevola"] = totalavailhab[3]/sum(totalavailhab)
allLocationsPS[1,"Unveg"] = totalavailhab[4]/sum(totalavailhab)

allLocationsPS

