# Michael Burnett
# Random Forest classification of Palmyra v1

# see <https://gis.stackexchange.com/questions/39021/how-to-perform-random-forest-land-cover-classification> for a basic tutorial on the randomForest package


install.packages("randomForest") #for machine learning
install.packages("raster")       #for manipulating image data
install.packages("rgdal")        #more image manipulation functions
#setwd("/volumes/Seagate 4tb/Palmyra Remote Sensing/antarctica-latest-free.shp");
install.packages("caret")        #TBH don't remember what this does
install.packages("e1071")        #TBH don't remember what this does
install.packages("hrbrthemes")

#Set this to whatever you like--I have it so D:/ always points to my portable hard drive with all the data on it
setwd("/volumes/Seagate 4tb/Palmyra Remote Sensing")
library("randomForest")
library("raster")
library("rgdal")
library("caret")
library("e1071")
library(maptools)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(sf)
library(adehabitatHS)
library(plyr)
library(reshape)
library(pracma)
library(lme4)
library("Hmisc")
############################################
###### IMPORT IMAGE AND TRAINING DATA ######
############################################

### Here we'll go ahead and import the image we want to classify, as well as import training data for our algorithm from an external file.

## Note: This script is written so that you load in a file that already has the GLCM texture layers, if you want to do texture analysis.
## It's also possible, and maybe sensible, to automate the calculation of GLCM textures in R using the glcm package, but I instead
## calculated my GLCM textures in ENVI. ENVI is an expensive program only on Stanford's geospatial computers, so not the ideal option...

# Import raster image with 8 visual and 8 texture layers; make a single raster "brick" with all 16 layers:
img <- brick("palmyra-2016-17x17-ms-ALL-BANDS.tif") #change filename as needed. TIF or IMG images should work fine
names(img) <- c(paste0("T",1:8, coll=""), paste0("B",1:8, coll="")) #renames bands to shortened designations

### This is an example shortened band list for DigitalGlobe 8-band imagery plus 8 textural bands
### You can find band lists for other satellites (e.g. PlanetScope) online
### Band structure:
#
# Layer     Designation     Data
# 1         T1              Mean
# 2         T2              Variance
# 3         T3              Homogeneity
# 4         T4              Contrast
# 5         T5              Dissimilarity
# 6         T6              Entropy
# 7         T7              Second Moment
# 8         T8              Correlation
# 9         B1              Coastal
# 10        B2              Blue
# 11        B3              Green
# 12        B4              Yellow
# 13        B5              Red
# 14        B6              Red-edge
# 15        B7              NIR-1
# 16        B8              NIR-2


# plot RGB rendering of the image just to check the layout of the image, forcing all pixel values >255 to equal 255. Note this is resource intensive:
#plotRGB(img * (img <= 255), r=13, g=11, b=10) #This doesn't quite get the colors right but the data is there, at least


### The code is currently written to read in a "shapefile" of points, which basically has a bunch of entries (points) with X and Y coordinates,
### as well as an attribute called "landcover" that says what kind of landcover that point is. You can have as many landcover classes as you want, although
### it'll impact the indexing numbers in the rest of this script.


#readOGR("palmyra-2016-truthing-points-v2.shp")
#ogrInfo(dsn="palmyra-2016-truthing-points-v2.shp", layer="palmyra-2016-truthing-points-v2")
#ogrListLayers("palmyra-2016-truthing-points-v2.shp")
trainingData <- readOGR(dsn = "palmyra-2016-truthing-points-v2.shp", layer = "palmyra-2016-truthing-points-v2")
#file.exists("palmyra-2016-truthing-points-v2.shp")
#trainingData <- shapefile("palmyra-2016-truthing-points-v2.shp") #Whether we need to do this for every island, or train the algorithm on one island and run the Random Forest model on all the rest, remains to be seen...

trainingData <- trainingData[,-1] #removing some extra coordinate columns...
trainingData <- trainingData[,-1]

# this section adds a "land" column to trainingData with a simple binary 0=water, 1=land. We'll use this for masking out pixels that are water
# using a very simple randomForest, since we don't want all the water in the image in our final classification's statistics. Kind of an optional
# step but it doesn't take much
trainingData$land <- 0
for (i in 1:nrow(trainingData)){
  if (trainingData$landcover[i] != 3 & trainingData$landcover[i] != 4){
    trainingData$land[i] = 1
  }
}

# Assign raster values to training data points
dataSet <- as.data.frame(extract(img, trainingData))
trainingData@data = data.frame(trainingData@data, dataSet[match(rownames(trainingData@data), rownames(dataSet)),])
View(trainingData)


###########################
###### WATER MASKING ######
###########################
# Now, to improve our ultimate classification, use the infrared bands on our satellite to mask out all water from the image
# infrared light is particularly sensitive to water so it can be used quite well. However, when I did this for Palmyra I had to manually
# clean up a few spots here and there, so for a project where we expand to lots of islands we may just not bother water masking and simply
# include a water class in our final classification

# Run the Random Forest model using your training data, but using the land binary column as your class list and bands B7 and B8 only
# note that the indices in the line below are to be edited to adjust which bands you train the RF on. For this we are using the two NIR bands
rf.mdl.mask <- randomForest(x=trainingData@data[,17:18], y=as.factor(trainingData@data[,"land"]), ntree=100, importance=TRUE, progress="window")

# Classify the image with the above RF model that targets only LAND vs WATER
landvwater = predict(img, rf.mdl.mask, filename="6.26-Palmyra-RF-mask-v1.img", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

# NOTE: from here, I took my classified image (which had values of 0 for water and 1 for land) and used it to "mask" my original image,
# turning all pixels that are classified as 0 into "NA" pixels. I did this in ENVI but it's definitely possible in R if you take the time
# to code it.

#This kind of takes forever and idk why
landOnly = mask(img,landvwater,filename="6.27-Palmyra-ALL-BANDS-MASKED-v2.tif",maskvalue=0,updatevalue=NA,overwrite=TRUE)
names(landOnly) <- c(paste0("T",1:8, coll=""), paste0("B",1:8, coll="")) #renames bands to shortened designations

############################
###### CLASSIFICATION ######
############################

# Assign raster values to training data
dataSet <- as.data.frame(extract(landOnly, trainingData))
trainingData@data = data.frame(trainingData@data, dataSet[match(rownames(trainingData@data), rownames(dataSet)),])
View(trainingData)

# Remove rows with land=0.
trainingData.2 <- subset(trainingData, land!= 0)

# You can remove layers from trainingData.2 if you want to do some feature selection, meaning selectively paring down your number of bands
# in such a way that can improve classification accuracy:
keeps <- c("landcover", "land", "T5","T8","T6","B1","B5","B7")
trainingData.2 <- trainingData.2[, keeps, drop = FALSE]
View(trainingData.2)

# Run the Random Forest model using your training data; this should omit all pixels with NaN data values (i.e. water)
rf.mdl <- randomForest(x=trainingData.2@data[,3:8], y=as.factor(droplevels(trainingData.2@data[,"landcover"])), ntree=5400, na.action=na.omit, importance=TRUE, progress="window")

# Check error convergence. These "Out of bag" errors are a built in feature of random forest that tells you roughly how well your algorithm is doing
plot(rf.mdl, main="Out-of-bag errors for 16-feature RF model")#, xlab="Number of trees grown", ylab="OOB error")

# Plot variable importance (mean decrease in accuracy is type=1; mean decrease Gini is type=2)
# These two metrics tell you which of your bands are the most important for making an accurate classification, information you can then
# use for variable selection, if you like
varImpPlot(rf.mdl, sort=TRUE, type=2, scale=TRUE)
View(importance(rf.mdl)) # see right-hand columns for importance measures

# Here I like to average together the MDA and MDG accuracy scores and use that ranking as my new basis for feature selection
var.score <- data.frame(importance(rf.mdl)[,5],importance(rf.mdl)[,6]) # make new dataframe to combine mda and mdg scores
var.score$mdarank   <- rank(var.score$importance.rf.mdl....5.)
var.score$mdgrank   <- rank(var.score$importance.rf.mdl....6.)
var.score$avgrank   <- ( var.score$mdarank + var.score$mdgrank ) / 2
View(var.score) # Higher ranking is better


# Classify the image with all variables. This is the step that produces the real classification!
predict(landOnly, rf.mdl, filename="6.27MASKED.Palmyra-RF-classification-v3.img", type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)


################################
###### MAJORITY FILTERING ######
################################
# Since the output product is pretty patchy, many classification projects take their computer-generated output and smooth it out. For
# continuous data you'd want to use a median or mean filter, but since we have discrete classes we use a majority (aka modal) filter here.
# 3x3 or 5x5 pixel smoothing windows are common

# read the finished classification file in again
img.1 <- raster("6.27MASKED.Palmyra-RF-classification-v3.img")

img.f <- focal(img.1, w=matrix(1,5,5), fun=modal)
writeRaster(img.f, filename = "6.27MASKEDPalmyra-RF-classification-v3-5x5-modal.tif", format = 'GTiff', overwrite = T)
#Note: these lines of code take forever to run and I'm not really sure why...

############################
###### ACCURACY TESTS ######
############################

# Read in validation points shapefile; get rid of extra coordinate columns as with above
valData <- readOGR(dsn = "palmyra-2016-validation-points-v2.shp", layer = "palmyra-2016-validation-points-v2")
ogrInfo(dsn = "palmyra-2016-validation-points-v2.shp",layer="palmyra-2016-validation-points-v2")
#valData <- shapefile("palmyra-2016-validation-points-v2.shp")
valData <- subset(valData, select = landcover)
View(valData)

# Import smoothed classification image:
classed <- raster("6.26MASKEDPalmyra-RF-classification-v3-5x5-modal.tif") # Took 5x5 MODAL average

# Assign classification values to corresponding validation pixels
valData$classified <- as.data.frame(extract(classed, valData))
pred <- list("0")
truth<- list("0")

# Now, loop through classification values and reassign to their landcover equivalents based on 6 classes. This shouldn't be necessary if the above steps were done right lol
# These numbers are specific to my case and you can probably figure out how to get around doing this hacked-together thing that doesn't really work
# 0  -> 0 - Cocos
# 51 -> 1 - Native Trees
# 102-> 2 - Scaevola
# 154-> 3 - Shallows/reefs
# 205-> 4 - Open Water
# 255-> 5 - Sand/infrastructure
for (i in 1:nrow(valData)){
  if (!is.na(valData@data[i,2])) {
    if (valData@data[i,2] == 51){
      valData@data[i,2] <- 1
    }
    if (valData@data[i,2] == 102){
      valData@data[i,2] <- 2
    }
    if (valData@data[i,2] == 154){
      valData@data[i,2] <- 3
    }
    if (valData@data[i,2] == 205){
      valData@data[i,2] <- 4
    }
    if (valData@data[i,2] == 255){
      valData@data[i,2] <- 5
    }
    pred[i] <- valData@data[i,2]
    truth[i]<- as.numeric(levels(valData@data[i,1])[valData@data[i,1]])
  }
}

testData = data.frame(matrix(unlist(pred), nrow=496, byrow=T)) %>% cbind(data.frame(matrix(unlist(truth), nrow=496, byrow=T)))
colnames(testData) = c("Predicted","Truth")
write.table(valData$landcover, "clipboard-2048", sep=",", row.names=FALSE)
write.table(valData$classified, "clipboard-2048", sep=",", row.names=FALSE)

#validateTable <- merge(pred, truth, all=TRUE)
#confusionMatrix(validateTable)

# OOB calculation
nvariables = 4
conf <- rf.mdl$confusion
conf <- data.frame(conf)
conf$Accuracy = 0
conf$Precision = 0
colnames(conf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure", "Error", "Accuracy", "Precision")
rownames(conf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure")
for (i in 1:nrow(conf)) {
  numSamples = 0
  for (j in 1:nvariables) {
    numSamples = numSamples + conf[i,j]
  }
  conf$Accuracy[i] = conf[i,i]/numSamples
  conf$Precision[i] = conf[i,i]/sum(conf[,i])
}

View(conf)
mean(conf$Accuracy)
mean(conf$Precision)

# Cross validation calculation
testconf <- data.frame(matrix(ncol=nvariables+3,nrow=nvariables))
colnames(testconf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure", "Error", "Accuracy","Precision")
rownames(testconf) = c("Cocos","Native Trees","Scaevola","Sand/Infrastructure")

for (i in 1:nrow(testconf)) {
  Total = testData[which(testData$Truth == sort(unique(testData$Truth))[i]),]
  numSamples = 0
  for (j in 1:nvariables) {
    testconf[i,j] = nrow(Total[which(Total$Predicted == sort(unique(testData$Truth))[j]),])
    numSamples = numSamples + testconf[i,j]
  }
  testconf$Accuracy[i] = testconf[i,i]/numSamples
}
for (i in 1:nrow(testconf)) {
  testconf$Error[i] = 1-testconf$Accuracy[i]
  testconf$Precision[i] = testconf[i,i]/sum(testconf[,i])
}
View(testconf)
mean(testconf$Accuracy)
mean(testconf$Precision)



