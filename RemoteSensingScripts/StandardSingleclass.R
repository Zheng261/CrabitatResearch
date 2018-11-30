######## Change the island name here! ########
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
#Island = "Nauru"
#Island = "Nanumanga"
#Island = "Niutao"
#Island = "Pukarua"
######## Convention - file name of the trimmed island is *island*Clipped.tif, where island is the island name ###

appendDate = ""
addDateToStart = FALSE
if (addDateToStart) {
  appendDate = Sys.Date()
}
glcmnames <- readRDS("GLCMNames.rdat")

singleWaterTrim = TRUE

##### Run code for results! ######
TrimmedIsland <- brick(paste0(appendDate,Island,"-11x11wateryIsland.tif"))
names(TrimmedIsland) <- glcmnames
#plotRGB(TrimmedIsland)

Data <- readOGR(dsn = paste0(Island,"Training.shp"), layer = paste0(Island,"Training"))

#This shapefile for some reason was saved with the wrong CRS refernece - QGIS for some reason didn't convert any points before setting the CRS. 
Data <- spTransform(Data, proj4string(TrimmedIsland))
#You get a warning here for some NA points. I'm not sure why this happens but I just remove them and then it works fine. Perhaps one of the points
#was accidentally classified as water and removed.

### singleWaterTrim is true if we want to classify using our own individually collected data.
if (!singleWaterTrim) {
  colnames(Dataset) <- c("Class")
  Dataset <- as.data.frame(extract(TrimmedIsland, Data))
  Dataset = cbind(Data@data,Dataset)
  Dataset = Dataset[complete.cases(Dataset),]
  rf.mdl.mask <- readRDS("11.4WATERNNrandomForestSMPalmTerrFann.RDS")
} else {
  DataWithWater = cbind(Data@data,Data@data=="3")
  colnames(DataWithWater) <- c("Class","isWater")
  Dataset <- as.data.frame(extract(TrimmedIsland, Data))
  Dataset = cbind(DataWithWater,Dataset)
  ## Removes NAs
  Dataset = Dataset[complete.cases(Dataset),]
  ### Classify based on bands: RGB, IR,
  rf.mdl.mask <- randomForest(x=Dataset[,c(3:6,28:34)], y=as.factor(Dataset[,"isWater"]), ntree=200, importance=TRUE, progress="window")
}

### Figure out mask ###
Mask = predict(TrimmedIsland, rf.mdl.mask, type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(Mask)
### Mask island

raster::mask(TrimmedIsland,Mask,filename=paste0(appendDate,Island,"-SingleClassMasked.tif"),maskvalue=2,updatevalue=NA,overwrite=TRUE)
MaskedIsland <- brick(paste0(appendDate,Island,"-SingleClassMasked.tif"))
names(MaskedIsland) <- names(TrimmedIsland)

if (singleWaterTrim) {
  Dataset <- subset(Dataset, isWater == FALSE)
}

#Reads in info containing how important each band is in determining accuracy 
#We just use fanning temporarily
bandOrderInfo <- read.csv("8.22OrderOfImportanceFanningBands.csv")
#as.character(bandOrderInfo[c(1:24),1])
#Try out different mtry values?
rf.mdl <-randomForest(x=Dataset[,as.character(bandOrderInfo[c(1:22),1])],y=as.factor(droplevels(Dataset[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")

IslandClassed = predict(MaskedIsland, rf.mdl, filename=paste0(appendDate,Island,"-SingleClassClassed.tif"), type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(IslandClassed)

##### Calculates confusion matrix - OOB #######
# OOB calculation
nvariables = 5
conf <- rf.mdl$confusion
conf <- data.frame(conf)
conf$Accuracy = 0
conf$Precision = 0
colnames(conf) = c("Cocos","Native Trees","Low Vegetation","Sand/Infrastructure", "Error", "Accuracy", "Precision")
rownames(conf) = c("Cocos","Native Trees","Low Vegetation","Sand/Infrastructure")
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
valData$classified <- as.data.frame(extract(IslandClasses, Dataset))
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
