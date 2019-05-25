setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")

Islands = c("Niutao","Nauru","Pukarua","Nanumanga")

appendDate = ""
addDateToStart = FALSE
if (addDateToStart) {
  appendDate = Sys.Date()
}
glcmnames <- readRDS("GLCMNames.rdat")

## Separates out land and water training, since we only need to compute IR GLCM for water
PTFtrainingData = readRDS("10.18AllTrainingData.rdat")
PTFLandtrainingData = readRDS("10.18AllLandTrainingData.rdat")

allTrainingData = PTFtrainingData
allLandTrainingData = PTFLandtrainingData

#### 
for (Island in Islands) {
  
  TrimmedIsland <- brick(paste0(appendDate,Island,"-11x11wateryIsland.tif"))
  names(TrimmedIsland) <- glcmnames
  Data <- readOGR(dsn = paste0(Island,"Training.shp"), layer = paste0(Island,"Training"))
  #This shapefile for some reason was saved with the wrong CRS refernece - QGIS for some reason didn't convert any points before setting the CRS. 
  Data <- spTransform(Data, proj4string(TrimmedIsland))
  DataWithWater = cbind(Data@data, (Data@data == 3 | Data@data == 4))
  colnames(DataWithWater) <- c("Class","isWater")
  Dataset <- as.data.frame(extract(TrimmedIsland, Data))
  Dataset = cbind(DataWithWater,Dataset)
  ## Removes NAs
  Dataset = Dataset[complete.cases(Dataset),]
  ### Classify based on bands: RGB, IR,
  allLandTrainingData = rbind(allLandTrainingData,subset(Dataset,isWater != TRUE))
  allTrainingData = rbind(allTrainingData,Dataset)
}

rf.mdl.mask <- randomForest(x=allTrainingData[,c(6:13)], y=as.factor(allTrainingData[,"isWater"]), ntree=400, importance=TRUE, progress="window")
saveRDS(rf.mdl.mask,"11.4WATERNNrandomForestSMPalmTerrFann.RDS")

bandOrderInfo <- read.csv("8.22OrderOfImportanceALLISLANDBands.csv")
rf.mdl <-randomForest(x=allLandTrainingData[,as.character(bandOrderInfo[c(1:24),1])],y=as.factor(droplevels(allLandTrainingData[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")
saveRDS(rf.mdl,"1.15LANDrandomForestSMPalmTerrFann.RDS")

