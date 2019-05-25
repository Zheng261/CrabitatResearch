pukOne <- brick("10-8-OneIslePukaruaClassed.tif")
pukMulti <- brick("8.28-SMPukaruaClassed.tif")
names(pukOne) <- "Classed"
names(pukMulti) <- "Classed"

corrPukMulti <- pukMulti[which(!is.na(values(pukOne)))]
pts <- extract(corrPukMulti,)


pukaruaData <- readOGR(dsn = "PukaruaTraining.shp", layer = "PukaruaTraining")
#This shapefile for some reason was saved with the wrong CRS refernece - QGIS for some reason didn't convert any points before setting the CRS. 
pukaruaData <- spTransform(pukaruaData, "+proj=utm +zone=8 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")


dataSet <- as.data.frame(extract(pukMulti, pukaruaData))
pukaruaData@data = cbind(pukaruaData@data,dataSet)
colnames(pukaruaData@data) <- c("ZhengPicked","MultiTrained")
View(pukaruaData@data)
length(which(pukaruaData@data$ZhengPicked == pukaruaData@data$MultiTrained))
length(pukaruaData@data$ZhengPicked)


accFrame = data.frame(matrix(nrow=4,ncol=3))
rownames(accFrame) <- c("Cocos","Natives","Water","Unveg")
colnames(accFrame) <- c("NumCorrect","NumTotal","Acc%")
pukaruaCocosData = subset(pukaruaData@data,ZhengPicked == 0)
pukaruaNativesData = subset(pukaruaData@data,ZhengPicked == 1)
pukaruaWaterData = subset(pukaruaData@data,ZhengPicked == 3)
pukaruaUnvegData = subset(pukaruaData@data,ZhengPicked == 5)

accFrame[1,1] = length(which(pukaruaCocosData$ZhengPicked == pukaruaCocosData$MultiTrained))
accFrame[1,2] = length(pukaruaCocosData$ZhengPicked)

accFrame[2,1] = length(which(pukaruaNativesData$ZhengPicked == pukaruaNativesData$MultiTrained))
accFrame[2,2] = length(pukaruaNativesData$ZhengPicked)

accFrame[3,1] = length(which(is.na(pukaruaWaterData$MultiTrained)))
accFrame[3,2] = length(pukaruaWaterData$ZhengPicked)

accFrame[4,1] = length(which(pukaruaUnvegData$ZhengPicked == pukaruaUnvegData$MultiTrained))
accFrame[4,2] = length(pukaruaUnvegData$ZhengPicked)

accFrame$`Acc%` <- accFrame$NumCorrect/accFrame$NumTotal
