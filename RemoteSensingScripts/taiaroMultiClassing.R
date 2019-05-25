setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)
glcmnames <- readRDS("GLCMNames.rdat")

testimg <- brick("TaiaroClipped.tif")

names(testimg) <- c("Blue","Green","Red","IR")
testimg <- subset(testimg, order(c(3,2,1,4)))

#plotRGB(testimg,r=1,b=2,g=3,stretch="lin")
dev.off()

#Reads in normalization stuff
colorNormFrame <- read.csv("8-28-18PalmTerainaFanningColorsStatistics.csv")
rownames(colorNormFrame) <- colorNormFrame$X
colorNormFrame = colorNormFrame[-1]

#Generates color statistics for Taiaro
taiaroNormFrame = colorNormFrame[1:2,]
rownames(taiaroNormFrame) <- c("MeanTaiaro","STDevTaiaro")

#Moves
for(color in colnames(colorNormFrame)) {
  print(color)
  taiaroNormFrame["MeanTaiaro",color] = cellStats(testimg[[color]],"mean")
  taiaroNormFrame["STDevTaiaro",color] = cellStats(testimg[[color]],"sd")
}

for(color in colnames(colorNormFrame)) {
  #Shifts down every value by the average mean of the islands
  #Multiplies every value by the ratio in standard deviation
  testimg[[color]] = testimg[[color]] * (colorNormFrame[c("STDevPalmyra"),color]/taiaroNormFrame["STDevTaiaro",color])
  
  testimg[[color]] = testimg[[color]] - (taiaroNormFrame["MeanTaiaro",color] - colorNormFrame[c("MeanPalmyra"),color])
}



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

writeRaster(testimg,"10-13-11x11NORMwateryTaiaro.tif",overwrite=TRUE)


testimg <- brick("10-13-11x11NORMwateryTaiaro.tif")
names(testimg) <- glcmnames
plotRGB(testimg, r=1,g=2,b=3,stretch="lin")

#Reads in super water mask
rf.mdl.mask <- readRDS("8.28WATERNORMrandomForestSMPalmTerrFann.RDS")

TaiaroMask = predict(testimg, rf.mdl.mask, filename="10-13-SMTaiaroWaterMask.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(TaiaroMask)
#Saves masked island
Taiaro = raster::mask(testimg,TaiaroMask,filename="10-13-SMTaiaroMasked.tif",maskvalue=2,updatevalue=NA,overwrite=TRUE)
names(Taiaro) <- names(testimg)

#Plots masked island
plotRGB(Taiaro,r=1,b=2,g=3,stretch="lin")

#Reads in island classifier
rf.mdl <- readRDS("8.24randomForestSMPalmTerrFann.RDS")

Taiaro <- brick("10-13-SMTaiaroMasked.tif")
names(Taiaro) <- glcmnames

TaiaroClassed = predict(Taiaro, rf.mdl, filename="10-13-SMTaiaroClassed.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(TaiaroClassed)



varImpPlot(rf.mdl, sort=TRUE, type=2, scale=TRUE)
#View(importance(rf.mdl))

# Here I like to average together the MDA and MDG accuracy scores and use that ranking as my new basis for feature selection
var.score <- data.frame(importance(rf.mdl)[,5],importance(rf.mdl)[,6]) # make new dataframe to combine mda and mdg scores
var.score$mdarank   <- rank(var.score$importance.rf.mdl....5.)
var.score$mdgrank   <- rank(var.score$importance.rf.mdl....6.)
var.score$avgrank   <- ( var.score$mdarank + var.score$mdgrank ) / 2
var.score = var.score[order(var.score$avgrank,decreasing=TRUE),]
View(var.score) # Higher ranking is better


