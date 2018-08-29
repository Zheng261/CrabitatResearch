setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)

testimg <- brick("TaiaroClipped.tif")
names(testimg) <- c("Red","Green","Blue","IR")
plotRGB(testimg,stretch="lin")
red <- subset(testimg,1)


####### GLCM ON RED ########
glcmtestimg <- glcm(testimg$Red ,window=c(17,17))
names(glcmtestimg) <- paste("Red.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

####### GLCM ON GREEN ########
glcmtestimg <- glcm(testimg$Green ,window=c(17,17))
names(glcmtestimg) <- paste("Green.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

####### GLCM ON BLUE########
glcmtestimg <- glcm(testimg$Blue,window=c(17,17))
names(glcmtestimg) <- paste("Blue.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

####### GLCM ON INFRARED ########
glcmtestimg <- glcm(testimg$IR,window=c(17,17))
names(glcmtestimg) <- paste("IR.",names(glcmtestimg))
glcmtestimg <- dropLayer(glcmtestimg,8)
testimg <- addLayer(testimg,glcmtestimg)

############# WATER MASKING ###############
#crappyTrainingData <- readOGR(dsn = "PalmyraWater.shp", layer = "PalmyraWater")
#crappyTrainingData = spTransform(crappyTrainingData,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(crappyTrainingData,add=TRUE,col="red")
#rf.mdl.mask <- randomForest(x=crappyTrainingData@data[,c(2:5,27:31)], y=as.factor(crappyTrainingData@data[,"isWater"]), ntree=200, importance=TRUE, progress="window")

# Classify the image with the above RF model that targets only LAND vs WATER
crappyLandvWater = predict(testimg, rf.mdl.mask, filename="8.13-MaskForCrappyTaiaro.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
#plot(crappyLandvWater)
#varImpPlot(rf.mdl.mask, sort=TRUE, type=2, scale=TRUE)
#View(importance(rf.mdl))
plot(crappyLandvWater)

#This kind of takes forever and idk why
crappyLandOnly = raster::mask(testimg,crappyLandvWater,maskvalue=0,updatevalue=NA,overwrite=TRUE)
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

crappyForestPred <- predict(crappyLandOnly , rf.mdl, filename="8.14TaiaroPrediction17x17.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
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


