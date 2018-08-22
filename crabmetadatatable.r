crabs201X <- read.csv("crabs201XAllNP.csv")
crabs201X$Island = as.character(crabs201X$Island)
taggedMetaData16 <- read.csv("../Palmyra Crab Research/Crab tagging/tagged coconut crab metadata 2016.csv")
taggedMetaData17 <- read.csv("../Palmyra Crab Research/Crab tagging/tagged coconut crab metadata 2017.csv")
DFAreas <- read.csv("7.27KUDAreas.csv")

taggedMetaData16 = taggedMetaData16[,which(colnames(taggedMetaData16)%in%c("Animal..","Sex","Island","Carapace.width..cm.","Thoracic.length..cm.","Body.mass..kg."))]
colnames(taggedMetaData16) <- c("Crab Number","Sex","Island","Carapace width (cm)","Thoracic length (cm)","Body Mass (kg)")
taggedMetaData16$Year <- "2016"
  
taggedMetaData17 = taggedMetaData17[,which(colnames(taggedMetaData17)%in%c("Animal..","Sex","Island","Carapace.width..cm.","Thoracic.length..cm.","Body.mass..kg."))]
colnames(taggedMetaData17) <- c("Crab Number","Sex","Island","Carapace width (cm)","Thoracic length (cm)","Body Mass (kg)")
taggedMetaData17$Year <- 2017
taggedMetaData17$"Crab Number" = taggedMetaData17$"Crab Number" + 100

taggedMetaDataAll <- rbind(taggedMetaData16,taggedMetaData17)
taggedMetaDataAll = taggedMetaDataAll[which(!is.na(taggedMetaDataAll$"Crab Number")),]

taggedMetaDataAll$"Tracking Start" = NA
taggedMetaDataAll$"Tracking End" = NA
taggedMetaDataAll$"Total Days Tracked" = NA
taggedMetaDataAll = taggedMetaDataAll[which(taggedMetaDataAll$"Crab Number"%in%unique(crabs201X$CrabNum)),]

for (crab in unique(crabs201X$CrabNum)) {
  thisCrabTrax = subset(crabs201X,CrabNum==crab) 
  thisDateVec = as.POSIXct(paste(thisCrabTrax$Date,thisCrabTrax$Time),format="%m/%d/%y %H:%M:%S")
  taggedMetaDataAll[which(taggedMetaDataAll$`Crab Number`==crab),c("Tracking Start","Tracking End")] = as.character(range(thisDateVec))
  taggedMetaDataAll[which(taggedMetaDataAll$`Crab Number`==crab),c("Total Days Tracked")] = as.numeric(thisDateVec[length(thisDateVec)] - thisDateVec[1])
}
#### One of the crabs had its entries split into two periods for some reason - we fix this manually
taggedMetaDataAll[2,"Tracking End"] = taggedMetaDataAll[3,"Tracking End"]
taggedMetaDataAll = taggedMetaDataAll[-3,]
###  Adds home range and core area size for each crab
taggedMetaDataAll$'Home Range Size (km²)' = DFAreas$KUD95Area
taggedMetaDataAll$'Core Area Size (km²)' = DFAreas$KUD50Area

write.csv(taggedMetaDataAll, "8.16AllCrabMetaData.csv")


crabMetaDataCondensed <- data.frame(matrix(nrow=3,ncol=9))
colnames(crabMetaDataCondensed) <- c("Total Crabs","# 2017","Sex Ratio (M:F)","Avg. Carapace Width (cm)","Avg. Body Mass (kg)","Median Days Tracked","Total GPS Hits","Median KUD95 Size (km²)", "Median KUD50 Size (km²)")
rownames(crabMetaDataCondensed) <- c("cooper","eastern","sand")
for (island in c("cooper","eastern","sand")) {
  thisIslandTrax = subset(crabs201X,Island==island)
  thisIslandMeta = subset(taggedMetaDataAll,tolower(Island)==island)
  thisIslandDFAreas = subset(DFAreas,Island==island)
  crabMetaDataCondensed[island,1] = length(unique(thisIslandTrax$CrabNum))
  crabMetaDataCondensed[island,2] = nrow(subset(thisIslandMeta,Year==2017))
  crabMetaDataCondensed[island,3] = nrow(subset(thisIslandMeta,Sex=="Male"))/nrow(subset(thisIslandMeta,Sex=="Female"))
  crabMetaDataCondensed[island,4] = mean(thisIslandMeta$`Carapace width (cm)`)
  crabMetaDataCondensed[island,5] = mean(thisIslandMeta$`Body Mass (kg)`)
  crabMetaDataCondensed[island,6] = median(thisIslandMeta$`Total Days Tracked`)
  crabMetaDataCondensed[island,7] = nrow(thisIslandTrax)
  crabMetaDataCondensed[island,8] = median(thisIslandDFAreas$KUD95Area)
  crabMetaDataCondensed[island,9] = median(thisIslandDFAreas$KUD50Area)
}
print.data.frame(crabMetaDataCondensed)
#install.packages("stargazer")
#library(stargazer)
#stargazer(crabMetaDataCondensed,summary=FALSE)
write.csv(crabMetaDataCondensed,"8.16crabmetadatacondensed.csv")




