install.packages("glcm")
install.packages("imager")
install.packages("tmap")
library(glcm)
library(imager)
library(tmap)
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")

quartz()
testimg <- brick("RawakiSkySat.tif")
names(testimg) <- c("Blue","Green","Red","IR")
testimg <- subset(testimg, order(c(3,2,1,4)))

plotRGB(testimg , r=1, g=2, b=3, stretch="lin") #This doesn't quite get the colors right but the data is there, at least
#dev.off()

testimgbw <- ( testimg$Red + testimg$Green + testimg$Blue ) / 3
#testimg <- addLayer(testimg, gray_raster)
#names(testimg)[which(names(testimg)=="layer")] = "Grayscale"
#plot(gray_raster,col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
glcmtestimg <- glcm(testimgbw,window=c(3,3))
testimg<-addLayer(testimg,glcmtestimg)

trainingData <- readOGR(dsn = "RawakiIsWaterFilled.shp", layer = "RawakiIsWaterFilled")
#trainingData = spTransform(trainingData,"+proj=utm +zone=2 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot(trainingData,add=TRUE,cex=0.2)
dataSet <- as.data.frame(extract(testimg, trainingData))
trainingData@data = data.frame(trainingData@data, dataSet[match(rownames(trainingData@data), rownames(dataSet)),])
colnames(trainingData@data)[which(colnames(trainingData@data)=="id")] = "isWater"
trainingData@data = trainingData@data[which(!is.na(trainingData@data$isWater)),]
trainingData@data$IR2 = trainingData@data$IR
View(trainingData)


plot(testimg)
#testimg <- brick("palmyrahdquestion.jpg")
#plotRGB(testimgbw)
###########################
###### WATER MASKING ######
###########################
# Now, to improve our ultimate classification, use the infrared bands on our satellite to mask out all water from the image
# infrared light is particularly sensitive to water so it can be used quite well. However, when I did this for Palmyra I had to manually
# clean up a few spots here and there, so for a project where we expand to lots of islands we may just not bother water masking and simply
# include a water class in our final classification

# Run the Random Forest model using your training data, but using the land binary column as your class list and bands B7 and B8 only
# note that the indices in the line below are to be edited to adjust which bands you train the RF on. For this we are using the two NIR bands
rf.mdl.mask <- randomForest(x=trainingData@data[,c("Green","Blue","IR")], y=as.factor(trainingData@data[,"isWater"]), ntree=80, importance=TRUE, progress="window")

# Classify the image with the above RF model that targets only LAND vs WATER
landvwater = predict(testimg, rf.mdl.mask, filename="8.8-MaskForRawakiSkySat.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
# NOTE: from here, I took my classified image (which had values of 0 for water and 1 for land) and used it to "mask" my original image,
# turning all pixels that are classified as 0 into "NA" pixels. I did this in ENVI but it's definitely possible in R if you take the time
# to code it.
plot(landvwater) 

#This kind of takes forever and idk why
landOnly = raster::mask(testimg,landvwater,filename="8.8-WaterMaskedRawakiSkySat.tif",maskvalue=1,updatevalue=NA,overwrite=TRUE)
names(landOnly) <- names(testimg)
plotRGB(landOnly, r=1, g=2, b=3, stretch="lin")
dev.off()

testing <- readOGR(dsn = "RawatiIsWater.shp", layer = "RawatiIsWater")
testing@data
testing@data = data.frame(testing@data, dataSet[match(rownames(testing@data), rownames(dataSet)),])
colnames(testing@data)[which(colnames(testing@data)=="id")] = "isWater"
testing@data = testing@data[which(!is.na(testing@data$isWater)),]
testingAcc = as.data.frame(extract(landvwater, testing))


plot(rf.mdl.mask , main="Out-of-bag errors for 16-feature RF model")#, xlab="Number of trees grown", ylab="OOB error")

# Here I like to average together the MDA and MDG accuracy scores and use that ranking as my new basis for feature selection
var.score <- data.frame(importance(rf.mdl)[,5],importance(rf.mdl)[,6]) # make new dataframe to combine mda and mdg scores
var.score$mdarank   <- rank(var.score$importance.rf.mdl....5.)
var.score$mdgrank   <- rank(var.score$importance.rf.mdl....6.)
var.score$avgrank   <- ( var.score$mdarank + var.score$mdgrank ) / 2
View(var.score) # Higher ranking is better

