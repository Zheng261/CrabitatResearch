### Imports chagos image ###
Chagos<-brick("mergedChagos.tif")
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


