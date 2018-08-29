install.packages("randomForest") #for machine learning
install.packages("raster")       #for manipulating image data
install.packages("rgdal")        #more image manipulation functions
install.packages("caret")        #TBH don't remember what this does
install.packages("e1071")        #TBH don't remember what this does
install.packages("hrbrthemes")
install.packages("rgeos")
install.packages("lemon")
install.packages("knitr")
install.packages("prettymapr")
install.packages("RStoolbox")


#Run this file before running anything else!!!! Imports all the files needed for remote sensing and GPS data processing
setwd("/volumes/Seagate 4tb/Palmyra Remote Sensing")
library("randomForest")
library("rgdal")
library("caret")
library("e1071")
library(stringr)
library(maptools)
library(rgeos)
library(plyr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(sf)
library(adehabitatHS)
library(reshape)
library(pracma)
library(lme4)
library("Hmisc")
library(lemon)
library(knitr)
library(prettymapr)
library("raster")