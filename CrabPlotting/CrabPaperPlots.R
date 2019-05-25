### Run through CompiledCrabStats first before coming here

### Some large raster lists may cause R to report a vector memory exhausted error. We fixed this by
### parsing the large raster sets into individual files, importing them, plotting them, and immediately removing them.

#BEGINS PDF FOR ALL THE PAPER PLOTS
#pdf("8.1EditedNormCompiledAnalysis.pdf",width=9,height=5)

### Essential files for crab plotting. Both of these are on github 
### - just add the directory Zheng261/CrabitatResearch/FastTrack

### AllLocations is a RDS file containing a data frame of all of the Palmyra islands' habitat distributions
allLocations <- readRDS("palmyraHabDist.rdat")
### crabHRList is an RDS file containing a list of all of the home range rasters (water trimmed).
crabHRList <-readRDS("7.27crabHRdata.rds")

###Crabmedrasterlist is an RDS file containing a list of all the raster borders of the home ranges
crabMedRasterList <- readRDS("8.3KUDborderLONGTIME.RDS")

###crabColorsList contains all of the crabs' colors - they are organized according to a rainbow on each island in 
###descending order of KUD size
crabColorsList <- readRDS("8.2crabColorsByIsland.RDS")

### Crab survey data - data frame created by Mike and RDS-ified by Zheng
summed <- readRDS("8.30CrabHabitatFound.RDS")

### Offsets for indexing crabHRList
kudoffset = 1000
fiftyoffset = 500

#Analysis of crab selection by island
# Converts data frame of MCP hab. selection ratios into ggplot-friendly format
meltmcpframeall = melt(mcpmedframe[,2:10],id=c("Island"))
colnames(meltmcpframeall)[2] ="HabitatType"
colnames(meltmcpframeall)[3] ="SelectionRatio"
meltmcpframeall$type = revalue(substr(meltmcpframeall$HabitatType,1,2),c("95" = "mcp95","50" = "mcp50"))
meltmcpframeall$HabitatType = substr(meltmcpframeall$HabitatType,3,100)

# Converts data frame of KUD hab. selection ratios into ggplot-friendly format
meltkudframeall = melt(kudmedframe[,2:10],id=c("Island"))
colnames(meltkudframeall)[2] ="HabitatType"
colnames(meltkudframeall)[3] ="SelectionRatio"
meltkudframeall$type = revalue(substr(meltkudframeall$HabitatType,1,2),c("95" = "kud95","50" = "kud50"))
meltkudframeall$HabitatType = substr(meltkudframeall$HabitatType,3,100)

### Changes Sand to Unveg because that's more accurate
meltmcpframeall$HabitatType = revalue(meltmcpframeall$HabitatType,c("Sand"="Unveg."))
meltkudframeall$HabitatType = revalue(meltkudframeall$HabitatType,c("Sand"="Unveg."))

### Finds the average selection ratio for each island and habitat type
relativemcp <- data_summary(meltmcpframeall,varname="SelectionRatio",groupnames= c("type","Island","HabitatType"))
relativekud <- data_summary(meltkudframeall,varname="SelectionRatio",groupnames= c("type","Island","HabitatType"))

### Changes island to character in case it was a factor before (don't know what causes this but it happened before, maybe 
### just an issue with the way that csv is imported into excel)
relativekud$Island = as.character(relativekud$Island)
relativemcp$Island = as.character(relativemcp$Island)
relativeall = rbind(relativemcp,relativekud)
meltcraball = rbind(meltmcpframeall,meltkudframeall)
for (isle in 1:nrow(allLocations)) {
  if (rownames(allLocations)[isle] != "paradise") {
    meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Cocos",allLocations[isle,"Cocos"],"actual")
    meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Natives",allLocations[isle,"Natives"],"actual")
    meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Scaevola",allLocations[isle,"Scaevola"],"actual")
    meltcraball[nrow(meltcraball)+1,] = c(rownames(allLocations)[isle],"Sand",allLocations[isle,"Sand"],"actual")
  }
}

# Selection ratio is converted to numeric
meltcraball$SelectionRatio = as.numeric(meltcraball$SelectionRatio)
meltcraball$varIsle = paste(meltcraball$HabitatType,meltcraball$Island)
meltcrabactual <- subset(meltcraball,type=="actual")
allLocations = allLocations[c("sand","cooper","eastern"),]

### Grabs all the actual habitat percentages and adds them to the data frame
for (isle in 1:nrow(allLocations)) {
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Cocos",allLocations[isle,"Cocos"],0)
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Natives",allLocations[isle,"Natives"],0)
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Scaevola",allLocations[isle,"Scaevola"],0)
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Unveg.",allLocations[isle,"Sand"],0)
}
### Makes sure everything numeric is the a numeric data type 
relativeall$varIsle = paste(relativeall$HabitatType,relativeall$Island)
relativeall$sd[is.na(relativeall$sd)] = 0
relativeall$SelectionRatio = as.numeric(relativeall$SelectionRatio)
relativeall$sd = as.numeric(relativeall$sd)
meltcrabkud = subset(meltcraball,type=="kud95")

######### ALL CRAB PLOTS #############

##### Table 1 was obtained directly from crabmetadata.R. The excel file that contains the final product is also in the
##### FastTrack folder. (8.16crabmetadatacondensed)
##### Supplemental Figure Table 2 also comes from crabmetadata.R, same as above. (8.16allcrabmetadata)


##### Figure 1: Plot of the whole island with all the crab tracks
#imgtest <- brick("lowqualpalmyra.tif")
#pdf("8.3MDWholeIslandTracksv2.pdf")
#### This is Mike's big classification tif. I condensed it down to a RDS file but it seems way to small
#### to actually hold all of the information. ### WHAT IT WORKS ###
#img <- brick("palmyra-2016-17x17-ms-ALL-BANDS.tif") #change filename as needed. TIF or IMG images should work fine

img <- readRDS("8-29palmyra-2016-17x17-ms-ALL-BANDS.RDS")
names(img) <- c(paste0("T",1:8, coll=""), paste0("B",1:8, coll=""))
plotRGB(img, r=13, g=11, b=10,stretch="hist")
colors = rainbow(length(unique(HourlyMedianDF$CrabNum)))
iter = 0
for (crab in unique(HourlyMedianDF$CrabNum)) {
  print(crab)
  crabList = list()
  thisCrabTrax = HourlyMedianDF[which(HourlyMedianDF$CrabNum == crab),]
  thisCrabTrax$Date = substr(thisCrabTrax$DateTime,1,10)
  for (date in 1:length(unique(thisCrabTrax$Date))) {
    crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
  }
  testSL = SpatialLines((crabList[1]))
  testSL@lines <- crabList
  proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1)
  plot(testUTMSL,col=crabColorsList[[crab]],pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
  iter = iter+1
}
addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.5,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                        unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                        style = "bar", bar.cols = c("black", "white"), lwd = 1,
                        linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                        label.col = "black", pos = "bottomright")

#dev.off()

##### Figure 2: Plot of the crab shelter habitat distribution
ggplot(summed, aes(x=General.habitat.type, y=Crab.found, fill=General.habitat.type)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  theme_bw() +   # removes grey background. Keep this line towards the beginning to keep this look as the parent theme
  geom_errorbar(aes(ymin=Crab.found-se, ymax=Crab.found+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Habitat type (ten surveys per bar)", y="Crabs found per 30-minute survey") +
  theme(axis.text=element_text(size=16),      #adjust text sizes, etc. for presentations. These lines are altering theme_bw above
        axis.title=element_text(size=18),plot.title = element_text(size=22,hjust = 0.5),panel.grid.major.x = element_blank(),strip.text = element_text(size=16))+
  #title=element_text(size=20)+                          #move legend to inside of plot
  ggtitle(str_to_title("Crabs found versus habitat type (2016-17)")) +
  scale_fill_manual(values=c("#009E73", "#00819D", "#0072B2")) + theme(legend.position="none")+
  facet_wrap(~str_to_title(Day.night)) # , panel.grid.minor = element_blank()    #removes unnecessary gr

##### Figure 3: Plot of the each island with all the crab KUD raster borders ####
pdf("8.6CooperCrabKUDBorders.pdf",width=10,height=7)
for (island in c("cooper","eastern","sand")) {
  imgtest <- raster(paste0(island,".tif"))
  image(imgtest, col="black",axes=FALSE)
  allCrabsInIsland = subset(k95meltRO,Island==island)
  for (crab in unique(allCrabsInIsland$CrabNum)) {
    print(crab)
    rastTest <- crabMedBorderRasterList[[crab]]
    image(rastTest,col=crabColorsList[[crab]],add=TRUE)
  }
  
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomright")
  
  addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.5,
                lwd = 1, border = "black", cols = c("white", "black"),
                text.col = "black")
  legend(title=str_to_title(island),x="bottomleft",legend=unique(allCrabsInIsland$CrabNum),col= unlist(crabColorsList[unique(allCrabsInIsland$CrabNum)]),lty=1,cex=0.7)
}
dev.off()


###### Figure 4: Box plot of actual habitat distribution on each island vs. crab habitat utilization #####
#pdf("8.8KUDBoxBarPlots.pdf",width=10,height=7)
p <- ggplot(data=meltcrabkud,aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_boxplot(position=position_dodge()) +
  stat_summary(fun.data="mean_sdl",geom="pointrange") + ggtitle("% Crab Home Range in Each Habitat Type By Island") + facet_wrap(~str_to_title(Island)) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type") 
p = p + theme_bw() + geom_bar(stat="identity",position=position_dodge(),data=relativeall[relativeall$type=="actual",],aes(x=HabitatType,y=SelectionRatio,col="black"),alpha=0.4) 
p = p + theme(legend.position="none",axis.text=element_text(size=14),      #adjust text sizes, etc. for presentations. These lines are altering theme_bw above
              axis.title=element_text(size=17),axis.text.x=element_text(size=12,angle=35,hjust=1), plot.title = element_text(size=20,hjust = 0.5),panel.grid.major.x = element_blank(),strip.text = element_text(size=15))+
        scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00", "#CC79A7")) + scale_colour_manual(values=c("black"))
p 
#dev.off()


###### Figure 5: Distribution of selection ratios between cocos and natives #######
#pdf("8.8CocoNatKUDSelectionRatio.pdf",width=10,height=7)
kud95stats = subset(meltkudallwiframe,type=="kud95" & (HabitatType == "CocosWI" | HabitatType=="NativesWI"))
statString = paste("F-test for comparison of variances: P =",round(unlist(var.test(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)[3]),3))
statString = paste(statString,"\n","Ratio of variances (Cocos/Natives) =",round(var(subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)/var(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio),3))
statString = paste(statString,"\n","Wilcoxon signed-rank test for comparison of population means: P = ",round(unlist(wilcox.test(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)[3]),3))
p <- ggplot(data=subset(meltall,type=="kud95" & (HabitatType == "Cocos" | HabitatType == "Natives")),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00", "#CC79A7")) + geom_violin(position=position_dodge()) + 
  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17, KUD95") + ylab("Selection Ratio") + xlab("Habitat Type") + theme_bw()+
  theme(legend.position="none",axis.text=element_text(size=17),      #adjust text sizes, etc. for presentations. These lines are altering theme_bw above
        axis.title=element_text(size=18),plot.title = element_text(size=20,hjust = 0.5),panel.grid.major.x = element_blank(),strip.text = element_text(size=15)) 
p
#dev.off()



#### Supplemental Figure 6 - representative plots ####
#### Starts representative crab plots ####
goodCrabList = list()
goodCrabList[["cooper"]] = c(10,119,131)
goodCrabList[["sand"]] = c(117,120,115)
goodCrabList[["eastern"]] = c(15,126,128)
for (island in c("cooper","sand","eastern")) {
  if (island=="cooper") {
    pdf("8.17CooperRepresentativeCrabPlots.pdf",height=8,width=10)
  } else if (island=="sand") {
    dev.off()
    pdf("8.17SandEasternRepresentativeCrabPlots.pdf",height=10,width=10)
  }
  imgtest <- brick(paste0(island,".tif"))
  image(imgtest, col="black",axes=FALSE,main="",xlab="",ylab="")
  allCrabsInIsland = subset(HourlyMedianDF,CrabNum%in%goodCrabList[[island]])
  maxDaysTracked = 0
  for (crab in goodCrabList[[island]]) {
    daysTracked = length(unique(substr(subset(allCrabsInIsland,CrabNum==crab)$Date,1,10)))
    maxDaysTracked = max(maxDaysTracked,daysTracked)
  }
  colors = rainbow(maxDaysTracked)
  # Prints crab tracks (median filtered)
  for (crab in goodCrabList[[island]]) {
    #Prints tracks on top of island
    print(crab)
    crabList = list()
    thisCrabTrax = allCrabsInIsland[which(allCrabsInIsland$CrabNum == crab),]
    thisCrabTrax$Date = substr(thisCrabTrax$DateTime,1,10)
    for (date in 1:length(unique(thisCrabTrax$Date))) {
      crabList[[date]] = Lines(list(Line(thisCrabTrax[which(thisCrabTrax$Date == unique(thisCrabTrax$Date)[date]),c("Longitude","Latitude")])),ID=date)
    }
    testSL = SpatialLines((crabList[1]))
    testSL@lines <- crabList
    proj4string(testSL) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    testUTMSL = spTransform(testSL,"+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(testUTMSL,col=colors,pch=15,cex=0.3,lend=15,ljoin=4,lwd=2,lty=1,add=TRUE)
  }
  legendDates = paste("Day",c(1:length(colors)))
  legend(title=str_to_title(paste(island,"Island")),x="bottomright",legend=legendDates,col=colors,lty=1,cex=0.75)
  
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomleft")
  
  addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = 0.5,
                lwd = 0.5, border = "black", cols = c("white", "black"),
                text.col = "black")
  # Prints crab KUDs (watertrimmed)
  imgtest <- brick(paste0(island,".tif"))
  image(imgtest, col="black",axes=FALSE,main="",xlab="",ylab="")
  for (crab in goodCrabList[[island]]) {
    print(crab)
    ### Reads in each crab
    thisCrabHomeRangeRaster = readRDS(paste0("CrabRasterRDAT/",crab,"_95KUD.rdat"))
    thisCrabCoreAreaRaster = readRDS(paste0("CrabRasterRDAT/",crab,"_50KUD.rdat"))
    ### KUD 95 ###
    
    plot(thisCrabHomeRangeRaster,col="red",add=TRUE,legend=FALSE)
    ### KUD 50 ### 
    #maskedver = raster::mask(imgtest,crabHRList[[crabNum+kudoffset+fiftyoffset]])
    #maskedvertransformed = projectRaster(maskedver,crs=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))
    plot(thisCrabCoreAreaRaster,col="yellow",add=TRUE,legend=FALSE)
    
    #remove(thisCrabHomeRangeRaster)
    #remove(thisCrabCoreAreaRaster)
  }
  prettymapr::addscalebar(plotunit = NULL, plotepsg = "32603", widthhint = 0.25,
                          unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
                          style = "bar", bar.cols = c("black", "white"), lwd = 1,
                          linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
                          label.col = "black", pos = "bottomleft")
  
  addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = 0.5,
                lwd = 0.5, border = "black", cols = c("white", "black"),
                text.col = "black")
  legend(title=paste("Island:",str_to_title(island)),x="bottomright",legend=c("KUD95","KUD50"),fill=c("red","yellow"),lty=1,cex=0.75)
}
dev.off()


#### Supplemental Figure 7 #####
#pdf("7.25HomeRangeAndCrabHitsPerHour.pdf")
breaks <- function(lim) {
  return(seq( floor(lim[1]),ceil(lim[2]),6))
}
abacus<-function(state_rec, times,states=NULL,labels=NULL,add=FALSE,xlim=NULL,tunit="month", format="%m/%y",col="black",
                 ylab="Station",xlab="date",yline=4,xline=3,xcex=1.5,ycex=1.5,cex.yaxis=.75,cex.xaxis=.75,pch=15,main="Crab"){
  length.out<-length(state_rec)
  if(is.null(states)){
    states<-unique(state_rec)
  }
  nstates<-length(states)
  order<-rep(1,length.out)
  for(i in 2:nstates){
    order[which(state_rec==states[i])]<-i
  }
  if(add==FALSE){
    if(is.null(xlim)){
      xlim<-c(min(times,na.rm=TRUE),max(times,na.rm=TRUE))
    } 
    if(is.null(labels)){
      labels<-states
    }
    ylim<-c(.5,(nstates+.5))
    
    plot(main=main,0,type="n",ylim=ylim,xlim=xlim,yaxt="n",xaxt="n",xlab=" ",ylab=" ")
    axis(2,at=seq(1:nstates),labels=labels,las=2,cex.axis=cex.yaxis)
    axis.POSIXct(1, at=seq(xlim[1], xlim[2], by=tunit)
                 ,format=format,las=2,cex.axis=cex.xaxis)
    mtext(text = xlab,side = 1,line = xline, cex = xcex)
    mtext(text = ylab,side = 2,line = yline, cex = ycex)
  }
  points(order ~ times,pch=pch,col=col)
}
for (crab in unique(HourlyMedianDF$CrabNum)) {
  thisCrabTrax = subset(HourlyMedianDF,CrabNum==crab) 
  thisDateVec = as.POSIXct(thisCrabTrax$DateTime,format="%Y-%m-%d %H:%M:%S")
  abacus(state_rec=thisCrabTrax$CrabNum,times=thisDateVec,states=unique(thisCrabTrax$CrabNum),format="%m/%d, %H:%M",tunit="21600 s",xcex=1,xlab="",ylab="Entries",col=revalue(thisCrabTrax$Island,c("cooper"="red","sand"="yellow","eastern"="blue")),main=paste("Crab No.",crab))
  g <- ggplot(k50melt[which(k50melt$CrabNum==crab),],aes(x=variable,y=value)) + geom_line() + ggtitle(paste("Area of 50% KUD over time, crab number:",crab)) + xlab("Time Elapsed (Hours)") + ylab("Area (km^2)") + scale_x_continuous(breaks = breaks)
  print(g)
  g <- ggplot(k95melt[which(k95melt$CrabNum==crab),],aes(x=variable,y=value)) + geom_line()+ ggtitle(paste("Area of 95% KUD over time, crab number:",crab)) + xlab("Time Elapsed (Hours)") + ylab("Area (km^2)")
  print(g)
}
#dev.off()



#Supplemental Figure 8 - Area of 50%/95% KUD over time, One Point Every 6 Hours Per Crab
kernel95Area <- read.csv("7.29kernel95area.csv")
kernel95Area = kernel95Area[,which(colnames(kernel95Area)!="X")]
k95melt <- melt(kernel95Area,id=c("Island","CrabNum"))
k95melt$variable = as.numeric(k95melt$variable)*6
k95melt = subset(k95melt,value>0)
colnames(k95melt) <- c("Island","CrabNum","HoursElapsed","Area")
k95meltRO = k95melt[order(k95melt$HoursElapsed,decreasing=TRUE),]
k95meltRO = k95meltRO[!duplicated(k95meltRO$CrabNum),]
p <- ggplot(k95melt,aes(x=HoursElapsed,y=Area,color=as.factor(CrabNum))) + geom_line()+ coord_cartesian(xlim=c(0,200)) + ggtitle("Area of 95% KUD Over Time, by Individual Crab") + theme_light()+ xlab("Time Elapsed") + ylab("Area (km²)")  + theme(legend.position="none")
p

##### Supplemental Figure 9 - Crab Range Area over Time, Linear Model, One Point Per Crab
# Create multiple linear model - we assume that the area of the crab's KUD depends on hour elapsed since
# crab tracks began. Its slope and intercept is also affected by the island of the crab. 
lmer_fit <- lmer(Area ~ HoursElapsed + (HoursElapsed|Island), data=k95meltRO)
summary(lmer_fit)
predicted_df <- data.frame(lmer_pred = predict(lmer_fit, k95meltRO), variable=k95meltRO$HoursElapsed)
ftable = drop1(lmer_fit, ~. ,test="Chisq")
ftable 
#### Crab range over time, only taking into account last crab
k95meltRO$Island = str_to_title(k95meltRO$Island)
p <- ggplot(k95meltRO,aes(x=HoursElapsed,y=Area,color=Island)) + geom_point() +
  geom_line(color='red',data = predicted_df[which(k95meltRO$Island == "Cooper"),], aes(x=variable, y=lmer_pred)) + 
  geom_line(color='blue',data = predicted_df[which(k95meltRO$Island == "Sand"),], aes(x=variable, y=lmer_pred)) +
  geom_line(color='green',data = predicted_df[which(k95meltRO$Island == "Eastern"),], aes(x=variable, y=lmer_pred)) +
  xlab("Time Elapsed (Hours)") + ylab("Area (km^2)") + ggtitle("Area of Home Range vs. Hours Tracked, by Island") + theme_light()
#p$labels$colour <- paste("P(time) =",ftable$`Pr(Chi)`[2])                                                                                                                                    
p 






### Unused plots
#
#
#
#
#

#p <- ggplot(data=meltall,aes(x=HabitatType,y=SelectionRatio,fill=type)) + geom_violin(position=position_dodge()) + 
#  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17") + coord_cartesian(ylim = c(0, 5)) + ylab("Selection Ratio") + xlab("Habitat Type")
#p
#p <- ggplot(data=dfall[which(dfall$type=="kud95"),],aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_bar(stat="identity",position=position_dodge()) +
#  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
#                position=position_dodge(.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17, KUD") + coord_cartesian(ylim = c(0, 3)) + ylab("Selection Ratio") + xlab("Habitat Type")
#p
#All crabs ever, plotting by year
#dfmcpallvy <- data_summary(meltmcpallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Year"))
#dfkudallvy <- data_summary(meltkudallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Year"))
#dfallvy = rbind(dfmcpallvy,dfkudallvy)
#All crabs ever, plotting by island 
#dfmcpallvi <- data_summary(meltmcpallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Island"))
#dfkudallvi <- data_summary(meltkudallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Island"))
#dfallvi = rbind(dfmcpallvi,dfkudallvi)
#dfallvi = dfallvi[which(dfallvi$HabitatType=="CocosWI" | dfallvi$HabitatType=="NativesWI"),]
#dfallvi$HabitatType = revalue(dfallvi$HabitatType,c("NativesWI"="Natives","CocosWI"="Cocos"))
#p <- ggplot(data=dfallvi[which(dfallvi$type=="kud95" | dfallvi$type=="kud50"),],aes(x=HabitatType,y=SelectionRatio,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
#  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
#                position=position_dodge(.9)) + theme(text = element_text(size=10),
#                                                     axis.text.x = element_text(angle=60, hjust=1)) +  ggtitle("Average Habitat Selection Ratio for Coconut Crabs By Island, KUD") + coord_cartesian(ylim = c(0, 4)) + facet_wrap(~Island) + ylab("Selection Ratio") + xlab("Habitat Type")
#p

#p <- ggplot(data=subset(relativeall,type=="kud95" | type=="actual"),aes(x=HabitatType,y=SelectionRatio,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
#  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
#                position=position_dodge(.9)) + theme(text = element_text(size=10),
#                                                     axis.text.x = element_text(angle=60, hjust=1)) + ggtitle("% Crab Home Range (KUD95) in Each Habitat Type by Island, \nCompared to Real Habitat Ratio") + facet_wrap(~Island) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type")
#p
#p <- ggplot(data=meltcrabactual,aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_bar(stat="identity",position=position_dodge()) +
#  ggtitle("% Habitat Availability by Island") + facet_wrap(~Island) + ylab("% Available Habitat") + xlab("Habitat Type") + coord_cartesian(ylim = c(0, 1))
#p
#
### KUD ONLY
#p <- ggplot(data=subset(meltall,type=="kud95"),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_violin(position=position_dodge()) + 
#  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17, KUD95") + ylab("Selection Ratio") + xlab("Habitat Type")
#p
#p <- ggplot(data=subset(meltall,type=="kud95" & (HabitatType == "Cocos" | HabitatType == "Natives")),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_violin(position=position_dodge()) + 
#  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17 By Island, KUD95") + ylab("Selection Ratio") + xlab("Habitat Type") + facet_wrap(~Island)
#p
#p <- ggplot(data=subset(dfallvi,type=="kud95"),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_bar(stat="identity",position=position_dodge()) +
#  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
#                position=position_dodge(.9)) + theme(text = element_text(size=10),
#                                                     axis.text.x = element_text(angle=60, hjust=1)) +  ggtitle("Average Habitat Selection Ratio for Coconut Crabs By Island, KUD95") + coord_cartesian(ylim = c(0, 1.5)) + facet_wrap(~Island) + ylab("Selection Ratio") + xlab("Habitat Type")
#p
#
#meltcrabkud = subset(meltcraball,type=="kud95")
#p <- ggplot(data=meltcrabkud,aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_violin(position=position_dodge()) +
#  stat_summary(fun.data="mean_sdl",geom="pointrange") + ggtitle("% Crab Home Range in Each Habitat Type By Island") + facet_wrap(~Island) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type") 
#p = p + geom_bar(stat="identity",position=position_dodge(),data=relativeall[relativeall$type=="actual",],aes(x=HabitatType,y=SelectionRatio,fill="purple"),alpha=0.4)
#p

#for (crab in unique(kudmedframe$CrabNum)) {
#  print(crab)
#  thisCrabCoreAreaRaster = readRDS(paste0("CrabRasterRDAT/",crab,"_50KUD.rdat"))
#  saveRDS(projectRaster(thisCrabCoreAreaRaster,crs=CRS("+proj=utm +zone=3 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")),file=paste0("CrabRasterRDAT/",crab,"_50KUD.rdat"))
#  remove(thisCrabCoreAreaRaster)
#}

