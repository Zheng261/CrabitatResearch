img <- brick("20180112Lampsar_Orthomosaicforlance.tif") #change filename as needed. TIF or IMG images should work fine
names(img) <- c(paste0("T",1:8, coll=""), paste0("B",1:8, coll="")) #renames bands to shortened designations
#names(img) <- c("R","G","B","Brightness")
trainingData <- readOGR(dsn = "lolwaterlandalgaestuff.shp", layer = "lolwaterlandalgaestuff")
trainingData@data = trainingData@data[!is.na(trainingData@data[,"IsLand"]),]

dataSet <- as.data.frame(extract(img, trainingData))
trainingData@data = data.frame(trainingData@data, dataSet[match(rownames(trainingData@data), rownames(dataSet)),])
trainingData@data = trainingData@data[,-1]
colnames(trainingData@data) <- c("isLand","R","G","B","Brightness")
View(trainingData@data)
rf.mdl.mask <- randomForest(x=trainingData@data[,2:4], y=as.factor(trainingData@data[,"isLand"]), ntree=100, importance=TRUE, progress="window")
landvwater = predict(img, rf.mdl.mask, filename="7.15-lolrgbtest.img", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

#This kind of takes forever and idk why
landOnly = raster::mask(img,landvwater,filename="7.15maskedlalgae.tif",maskvalue=0,updatevalue=NA)


