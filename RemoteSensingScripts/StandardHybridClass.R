######## Change the island name here! ########
######## Convention - file name of the trimmed island is *island*Clipped.tif, where island is the island name ###
#### May 12, 2019 ####
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
#Island = "Pukarua"
#Island = "Niutao"
#Island = "Nauru"
#Island = "Nanumanga"
#Island = "Fangataufa"
dir()
allLandTrainingData <- readRDS("allAggregateLandTrainingData.RDS")


allLandTrainingData = subset(allLandTrainingData,as.numeric(as.character(Class)) < 6)
ncol(allLandTrainingData)
rf <- randomForest(x=allLandTrainingData[,c(as.character(bandOrderInfo[c(1:24),1]))],y=as.factor(droplevels(allLandTrainingData[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")

cocosOnly = subset(allLandTrainingData, Class == 0)
nativesOnly = subset(allLandTrainingData, Class == 1)
scaevolaOnly = subset(allLandTrainingData, Class == 2)
unvegOnly = subset(allLandTrainingData, Class == 5)

cocosLimited = cocosOnly[sample(nrow(cocosOnly),2000),]
nativesLimited = nativesOnly[sample(nrow(nativesOnly),2000),]
scaevolaLimited = scaevolaOnly[sample(nrow(scaevolaOnly),1000),]
unvegLimited = unvegOnly[sample(nrow(unvegOnly),1000),]

limitedAllLandTraining = rbind(cocosLimited,nativesLimited,scaevolaLimited,unvegLimited)
  

#randomF <- randomForest(x=limitedAllLandTraining[,c(as.character(bandOrderInfo[c(1:24),1]),"RedAverage","GreenAverage","BlueAverage")],y=as.factor(droplevels(limitedAllLandTraining[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")

###### CS231N ONLY ######

trainSet = allLandTrainingData[1:3000,]
testSet = allLandTrainingData[3001:nrow(allLandTrainingData),]


allLandTrainingData = allLandTrainingData[sample(nrow(allLandTrainingData)),]
trainSet = allLandTrainingData[1:7560,]
testSet = allLandTrainingData[7560:nrow(allLandTrainingData),]

randomF <- randomForest(x=trainSet[,c(as.character(bandOrderInfo[c(1:24),1]))],y=as.factor(droplevels(trainSet[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")
quartz()

varImpPlot(randomF, main="Variable importance for 24-feature land cover classification RF model")
randomF


wotmat <- randomF$confusion
wotmat

predictions = predict(randomF,testSet)
confusionMatrix(as.factor(as.character(testSet$Class)),predictions)
######

nrow(cocosOnly)
nrow(nativesOnly)
nrow(scaevolaOnly)
nrow(unvegOnly)
  
#Islands = c("Ahunui","Anuanuraro","Anuarunga","Nukutepipi","Banaba","Fangataufa","Morane")
allIslands = dir()
allIslandsTraining = allIslands[which(grepl("Training.shp",allIslands))]
Islands = str_replace(allIslandsTraining, "Training.shp","")
bandOrderInfo <- read.csv("8.22OrderOfImportanceALLISLANDBands.csv")

NeedsTrim= FALSE
Normalize = FALSE

newIslandStats = list()
Islands


for (Island in Islands[30:length(Islands)]) {
  print(Island)
  
  ##### Run code for results! ######
  
  appendDate = ""
  addDateToStart = FALSE
  if (addDateToStart) {
    appendDate = Sys.Date()
  }
  
  glcmnames <- readRDS("GLCMNames.rdat")
  
  if (Normalize) {
    rf.mdl.mask <- readRDS("8.28WATERNORMrandomForestSMPalmTerrFann.RDS")
  } else {
    rf.mdl.mask <- readRDS("11.4WATERNNrandomForestSMPalmTerrFann.RDS")
  }
 
  TrimmedIsland <- brick(paste0(appendDate,Island,"-11x11wateryIsland.tif"))
  names(TrimmedIsland) <- glcmnames
  
  print("Getting train data")
  Data <- readOGR(dsn = paste0(Island,"Training.shp"), layer = paste0(Island,"Training"))
  #In case some shapefiles were saved with the wrong CRS refernece 
  DataWithWater = cbind(Data@data, (Data@data == 3 | Data@data == 4))
  colnames(DataWithWater) <- c("Class","isWater")
  Dataset <- as.data.frame(extract(TrimmedIsland, Data))
  Dataset = cbind(DataWithWater,Dataset)
  ## Removes NAs
  Dataset = Dataset[complete.cases(Dataset),]
  Dataset = subset(Dataset, isWater != TRUE)
  
  DatasetTreesOnly = subset(Dataset, Class != 5)
  
  redMean = mean(DatasetTreesOnly$Red)
  greenMean = mean(DatasetTreesOnly$Green)
  blueMean = mean(DatasetTreesOnly$Blue)
  
  Dataset$RedAverage = redMean
  Dataset$GreenAverage = greenMean
  Dataset$BlueAverage = blueMean
  
  numCocos = sum(Dataset$Class == 0)
  numNatives = sum(Dataset$Class == 1)
  numScaevola = sum(Dataset$Class == 2)
  numNonveg = sum(Dataset$Class == 5)
  
  tryCatch({
    Dataset = rbind(Dataset, cocosOnly[sample(nrow(cocosOnly),200-numCocos),], nativesOnly[sample(nrow(nativesOnly),200-numNatives),], scaevolaOnly[sample(nrow(scaevolaOnly),100-numScaevola),], unvegLimited = unvegOnly[sample(nrow(unvegOnly),100-numScaevola),])
  }, error = function(e) {print(Island)
    print("Sufficient Data Already")})
  
  rf.mdl <- randomForest(x=Dataset[,c(as.character(bandOrderInfo[c(1:24),1]),"RedAverage","GreenAverage","BlueAverage")],y=as.factor(droplevels(Dataset[,"Class"])),ntree=2000,na.action=na.omit, importance=TRUE, progress="window")
  #rf.mdl <- readRDS("11.4LANDrandomForestSMPalmTerrFann.RDS")
  
  if (Normalize) {
    #### NORMALIZATION ####
    colorNormFrame <- read.csv("8-28-18PalmTerainaFanningColorsStatistics.csv")
    rownames(colorNormFrame) <- colorNormFrame$X
    for (color in c("Red","Green","Blue","IR")) {
      print(color)
      #Multiplies every value by the ratio in standard deviation
      TrimmedIsland[[color]] = TrimmedIsland[[color]] * (cellStats(TrimmedIsland[[color]],"sd")/mean(colorNormFrame[c("STDevPalmyra","STDevTeraina","STDevFanning"),color]))
      
      #Shifts down every value by the average mean of the islands
      TrimmedIsland[[color]] = TrimmedIsland[[color]] - (cellStats(TrimmedIsland[[color]],"mean") - mean(colorNormFrame[c("MeanPalmyra","MeanTeraina","MeanFanning"),color]))
      #}
      
      rf.mdl.mask <- readRDS("8.28WATERNORMrandomForestSMPalmTerrFann.RDS")
    }
  }
  
  if (NeedsTrim) {
    print("Trimming")
    print(Island)
    ### Predict water v land
    Mask = predict(TrimmedIsland, rf.mdl.mask, type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
    plot(Mask)
    ### Mask water from land
    raster::mask(TrimmedIsland,Mask,filename=paste0(appendDate,Island,"-SMMasked.tif"),maskvalue=2,updatevalue=NA,overwrite=TRUE)
  }
  ### Get masked island
  MaskedIsland <- brick(paste0(appendDate,Island,"-SMMasked.tif"))
  
  names(MaskedIsland) <- names(TrimmedIsland)
  MaskedIsland$RedAverage = redMean
  MaskedIsland$BlueAverage = blueMean
  MaskedIsland$GreenAverage = greenMean
  
  ### Predict island
  #IslandClassed = predict(MaskedIsland, rf.mdl, filename=paste0(appendDate,Island,"-SMClassed.tif"), type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
  print("Predicting")
  IslandClassed = predict(MaskedIsland, rf.mdl, filename=paste0(appendDate,Island,"-HBMRGBClassed.tif"), type="response", index=1, na.rm=TRUE, overwrite=TRUE)
  IslandClassedPoints = rasterToPoints(IslandClassed)
  newIslandStats[[Island]] = table(IslandClassedPoints[,3])
  #plot(IslandClassed)
  saveRDS(newIslandStats, "allIslandStatisticsRGBMeans.RDS")
}

#newIslandStats <- readRDS("allIslandStatisticsRGBMeans.RDS")




