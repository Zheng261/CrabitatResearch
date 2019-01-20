######## Change the island name here! ########
######## Convention - file name of the trimmed island is *island*Clipped.tif, where island is the island name ###
setwd("/volumes/Seagate 4tb/Pacific-islands-planet-imagery")
#Island = "Pukarua"
#Island = "Niutao"
#Island = "Nauru"
#Island = "Nanumanga"
#Island = "Fangataufa"
Islands = c("Ahunui","Anuanuraro","Anuarunga","Nukutepipi","Banaba","Fangataufa","Morane")
NeedsTrim= FALSE
Normalize = FALSE
for (Island in Islands) {
  print(Island)
  ##### Run code for results! ######
  
  appendDate = ""
  addDateToStart = FALSE
  if (addDateToStart) {
    appendDate = Sys.Date()
  }
  
  glcmnames <- readRDS("GLCMNames.rdat")
  rf.mdl <- readRDS("11.4LANDrandomForestSMPalmTerrFann.RDS")
  
  if (NeedsTrim) {
    ##Imports all stuff##
    TrimmedIsland = brick(paste0(Island,"Clipped.tif"))
    if (Normalize) {
      rf.mdl.mask <- readRDS("8.28WATERNORMrandomForestSMPalmTerrFann.RDS")
    } else {
      rf.mdl.mask <- readRDS("11.4WATERNNrandomForestSMPalmTerrFann.RDS")
    }
    ### Imports TrimmedIsland image ###
    names(TrimmedIsland) <- c("Blue","Green","Red","IR")
    TrimmedIsland <- subset(TrimmedIsland, order(c(3,2,1,4)))
    #plotRGB(TrimmedIsland,r=1,g=2,b=3,stretch="lin")
    
    ####### GLCM ON RED ########
    glcmTrimmedIsland <- glcm(TrimmedIsland$Red ,window=c(11,11))
    names(glcmTrimmedIsland) <- paste("Red.",names(glcmTrimmedIsland))
    glcmTrimmedIsland <- dropLayer(glcmTrimmedIsland,8)
    TrimmedIsland <- addLayer(TrimmedIsland,glcmTrimmedIsland)
    
    ####### GLCM ON GREEN ########
    glcmTrimmedIsland <- glcm(TrimmedIsland$Green ,window=c(11,11))
    names(glcmTrimmedIsland) <- paste("Green.",names(glcmTrimmedIsland))
    glcmTrimmedIsland <- dropLayer(glcmTrimmedIsland,8)
    TrimmedIsland <- addLayer(TrimmedIsland,glcmTrimmedIsland)
    
    ####### GLCM ON BLUE########
    glcmTrimmedIsland <- glcm(TrimmedIsland$Blue,window=c(11,11))
    names(glcmTrimmedIsland) <- paste("Blue.",names(glcmTrimmedIsland))
    glcmTrimmedIsland <- dropLayer(glcmTrimmedIsland,8)
    TrimmedIsland <- addLayer(TrimmedIsland,glcmTrimmedIsland)
    
    ####### GLCM ON INFRARED ########
    glcmTrimmedIsland <- glcm(TrimmedIsland$IR,window=c(11,11))
    names(glcmTrimmedIsland) <- paste("IR.",names(glcmTrimmedIsland))
    glcmTrimmedIsland <- dropLayer(glcmTrimmedIsland,8)
    TrimmedIsland <- addLayer(TrimmedIsland,glcmTrimmedIsland)
    
    
    
    writeRaster(TrimmedIsland,paste0(appendDate,Island,"-11x11wateryIsland.tif"))
  }
 
  TrimmedIsland <- brick(paste0(appendDate,Island,"-11x11wateryIsland.tif"))
  names(TrimmedIsland) <- glcmnames
  
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
    ### Predict water v land
    Mask = predict(TrimmedIsland, rf.mdl.mask, type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
    plot(Mask)
    ### Mask water from land
    raster::mask(TrimmedIsland,Mask,filename=paste0(appendDate,Island,"-SMMasked.tif"),maskvalue=2,updatevalue=NA,overwrite=TRUE)
  }
  ### Mask island
  MaskedIsland <- brick(paste0(appendDate,Island,"-SMMasked.tif"))
  names(MaskedIsland) <- names(TrimmedIsland)
  
  ### Predict island
  IslandClassed = predict(MaskedIsland, rf.mdl, filename=paste0(appendDate,Island,"-SMClassed.tif"), type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)
  #plot(IslandClassed)
}

