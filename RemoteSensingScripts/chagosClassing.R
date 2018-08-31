### Classification of Chagos ###
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
library(glcm)
library(imager)
library(tmap)
glcmnames <- readRDS("GLCMNames.rdat")

### Imports chagos image ###
Chagos<-brick("ChagosVerySmallClipped.tif")

names(Chagos) <- c("Blue","Green","Red","IR")
Chagos <- subset(Chagos, order(c(3,2,1,4)))
plotRGB(Chagos,stretch="lin")

####### GLCM ON RED ########
glcmChagos <- glcm(Chagos$Red ,window=c(11,11))
names(glcmChagos) <- paste("Red.",names(glcmChagos))
glcmChagos <- dropLayer(glcmChagos,8)
Chagos <- addLayer(Chagos,glcmChagos)

####### GLCM ON GREEN ########
glcmChagos <- glcm(Chagos$Green ,window=c(11,11))
names(glcmChagos) <- paste("Green.",names(glcmChagos))
glcmChagos <- dropLayer(glcmChagos,8)
Chagos <- addLayer(Chagos,glcmChagos)

####### GLCM ON BLUE########
glcmChagos <- glcm(Chagos$Blue,window=c(11,11))
names(glcmChagos) <- paste("Blue.",names(glcmChagos))
glcmChagos <- dropLayer(glcmChagos,8)
Chagos <- addLayer(Chagos,glcmChagos)

####### GLCM ON INFRARED ########
glcmChagos <- glcm(Chagos$IR,window=c(11,11))
names(glcmChagos) <- paste("IR.",names(glcmChagos))
glcmChagos <- dropLayer(glcmChagos,8)
Chagos <- addLayer(Chagos,glcmChagos)

writeRaster(Chagos,"8-26-11x11wateryChagos.tif")

Chagos <- brick("8-26-11x11wateryChagos.tif")
names(Chagos) <- glcmnames

#### NORMALIZATION ####
colorNormFrame <- read.csv("8-28-18PalmTerainaFanningColorsStatistics.csv")
rownames(colorNormFrame) <- colorNormFrame$X
#for (color in c("Red","Green","Blue","IR")) {
#    print(color)
    #Multiplies every value by the ratio in standard deviation
#    Chagos[[color]] = Chagos[[color]] * (cellStats(Chagos[[color]],"sd")/mean(colorNormFrame[c("STDevPalmyra","STDevTeraina","STDevFanning"),color]))
   
    #Shifts down every value by the average mean of the islands
#    Chagos[[color]] = Chagos[[color]] - (cellStats(Chagos[[color]],"mean") - mean(colorNormFrame[c("MeanPalmyra","MeanTeraina","MeanFanning"),color]))
#}

rf.mdl.mask <- readRDS("8.28WATERNORMrandomForestSMPalmTerrFann.RDS")
plotRGB(Chagos,r=1,b=2,g=3)
chagosMask = predict(Chagos, rf.mdl.mask, filename="8.28-SMChagosWaterMask.tif", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
plot(chagosMask)
quartz()
crappyLandOnly = raster::mask(Chagos,chagosMask,filename="8.28-SMChagosMasked.tif",maskvalue=2,updatevalue=NA,overwrite=TRUE)
names(crappyLandOnly) <- names(Chagos)
plotRGB(crappyLandOnly,r=1,b=2,g=3,stretch="hist")

