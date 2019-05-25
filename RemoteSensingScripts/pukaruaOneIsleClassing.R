### Classification of Pukarua ###
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)
glcmnames <- readRDS("GLCMNames.rdat")

### Imports Pukarua image ###
Pukarua<-brick("PukaruaClipped.tif")

names(Pukarua) <- c("Blue","Green","Red","IR")
Pukarua <- subset(Pukarua, order(c(3,2,1,4)))
plotRGB(Pukarua,stretch="lin")

####### GLCM ON RED ########
glcmPukarua <- glcm(Pukarua$Red ,window=c(11,11))
names(glcmPukarua) <- paste("Red.",names(glcmPukarua))
glcmPukarua <- dropLayer(glcmPukarua,8)
Pukarua <- addLayer(Pukarua,glcmPukarua)

####### GLCM ON GREEN ########
glcmPukarua <- glcm(Pukarua$Green ,window=c(11,11))
names(glcmPukarua) <- paste("Green.",names(glcmPukarua))
glcmPukarua <- dropLayer(glcmPukarua,8)
Pukarua <- addLayer(Pukarua,glcmPukarua)

####### GLCM ON BLUE########
glcmPukarua <- glcm(Pukarua$Blue,window=c(11,11))
names(glcmPukarua) <- paste("Blue.",names(glcmPukarua))
glcmPukarua <- dropLayer(glcmPukarua,8)
Pukarua <- addLayer(Pukarua,glcmPukarua)

####### GLCM ON INFRARED ########
glcmPukarua <- glcm(Pukarua$IR,window=c(11,11))
names(glcmPukarua) <- paste("IR.",names(glcmPukarua))
glcmPukarua <- dropLayer(glcmPukarua,8)
Pukarua <- addLayer(Pukarua,glcmPukarua)

writeRaster(Pukarua,"8-29-11x11wateryPukarua.tif")


Pukarua <- brick("8-29-11x11wateryPukarua.tif")
names(Pukarua) <- glcmnames

rf.mdl.mask <- readRDS("8.28WATERNORMrandomForestSMPalmTerrFann.RDS")

pukaruaData <- readOGR(dsn = "PukaruaTraining.shp", layer = "PukaruaTraining")
#This shapefile for some reason was saved with the wrong CRS refernece - QGIS for some reason didn't convert any points before setting the CRS. 
pukaruaData <- spTransform(pukaruaData, "+proj=utm +zone=8 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

#You get a warning here for some NA points. I'm not sure why this happens but I just remove them and then it works fine. Perhaps one of the points
#was accidentally classified as water and removed.
dataSet <- as.data.frame(extract(Pukarua, pukaruaData))
pukaruaData@data = cbind(pukaruaData@data,pukaruaData@data=="3")
colnames(pukaruaData@data) <- c("Class","isWater")
pukaruaData@data = data.frame(pukaruaData@data, dataSet[match(rownames(pukaruaData@data), rownames(dataSet)),])

## Removes NAs
pukaruaData@data = pukaruaData@data[complete.cases(pukaruaData@data),]

### Classify based on bands: RGB, IR,
rf.mdl.mask <- randomForest(x=pukaruaData@data[,c(3:6,28:34)], y=as.factor(pukaruaData@data[,"isWater"]), ntree=200, importance=TRUE, progress="window")

#plotRGB(Pukarua,r=1,b=2,g=3,stretch="hist")
PukaruaMask = predict(Pukarua, rf.mdl.mask, type="response", index=1, na.rm=TRUE, progress="window")
plot(PukaruaMask)

crappyLandOnly = raster::mask(Pukarua,PukaruaMask,filename="10.8-SingleIslePukaruaMasked.tif",maskvalue=2,updatevalue=NA,overwrite=TRUE)
names(crappyLandOnly) <- names(Pukarua)
plotRGB(crappyLandOnly,r=1,b=2,g=3,stretch="hist")

Pukarua <- brick("10.8-SingleIslePukaruaMasked.tif")
names(Pukarua) <- glcmnames

pukaruaData.2 <- subset(pukaruaData, isWater == FALSE)

#Reads in info containing how important each band is in determining accuracy 
#We just use fanning temporarily
bandOrderInfo <- read.csv("8.22OrderOfImportanceFanningBands.csv")
#as.character(bandOrderInfo[c(1:24),1])
#Try out different mtry values?
rf.mdl <-randomForest(x=pukaruaData.2@data[,as.character(bandOrderInfo[c(1:22),1])],y=as.factor(droplevels(pukaruaData.2@data[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")

  
PukaruaClassed = predict(Pukarua, rf.mdl, filename="10-8-OneIslePukaruaClassed.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(PukaruaClassed)


##### Calculates confusion matrix - OOB #######
# OOB calculation
nvariables = 3
conf <- rf.mdl$confusion
conf <- data.frame(conf)
conf$Accuracy = 0
conf$Precision = 0
colnames(conf) = c("Cocos","Native Trees","Sand/Infrastructure", "Error", "Accuracy", "Precision")
rownames(conf) = c("Cocos","Native Trees","Sand/Infrastructure")
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

#### TODO: VALIDATION TESTING. Import multi isle classing and see how it does on this new data.

# Assign classification values to corresponding validation pixels
valData$classified <- as.data.frame(extract(PukaruaClassed, pukaruaData.2))
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
