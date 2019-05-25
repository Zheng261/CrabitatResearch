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
plotRGB(Pukarua,r=1,g=2,b=3,stretch="lin")

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

#writeRaster(Pukarua,"8-29-11x11wateryPukarua.tif")

Pukarua <- brick("8-29-11x11wateryPukarua.tif")
names(Pukarua) <- glcmnames

rf.mdl.mask <- readRDS("10.13WATERNNrandomForestSMPalmTerrFann.RDS")
#rf.mdl.mask <- readRDS("8.28WATERNORMrandomForestSMPalmTerrFann.RDS")

plotRGB(Pukarua,r=1,b=2,g=3,stretch="lin")
#PukaruaMask = predict(Pukarua, rf.mdl.mask, filename="8.28-SMPukaruaWaterMask.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
PukaruaMask = predict(Pukarua, rf.mdl.mask, type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(PukaruaMask)

#crappyLandOnly = raster::mask(Pukarua,PukaruaMask,filename="8.28-SMPukaruaMasked.tif",maskvalue=2,updatevalue=NA,overwrite=TRUE)
crappyLandOnly = raster::mask(Pukarua,PukaruaMask,maskvalue=2,updatevalue=NA,overwrite=TRUE)
names(crappyLandOnly) <- names(Pukarua)
plotRGB(crappyLandOnly,r=1,b=2,g=3,stretch="lin")

rf.mdl <- readRDS("8.24randomForestSMPalmTerrFann.RDS")
Pukarua <- brick("8.28-SMPukaruaMasked.tif")
names(Pukarua) <- glcmnames
  
PukaruaClassed = predict(Pukarua, rf.mdl, filename="8.28-SMPukaruaClassed.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(PukaruaClassed)


