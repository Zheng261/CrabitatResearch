### Run through CompiledCrabStats first before coming here

### Some large raster lists may cause R to report a vector memory exhausted error. We fixed this by
### parsing the large raster sets into individual files, importing them, plotting them, and immediately removing them.
#pdf("8.1EditedNormCompiledAnalysis.pdf",width=9,height=5)

allLocations <- readRDS("palmyraHabDist.rdat")
crabHRList <-readRDS("7.27crabHRdata.rds")
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

meltmcpframeall$HabitatType = revalue(meltmcpframeall$HabitatType,c("Sand"="Unveg."))
meltkudframeall$HabitatType = revalue(meltkudframeall$HabitatType,c("Sand"="Unveg."))

relativemcp <- data_summary(meltmcpframeall,varname="SelectionRatio",groupnames= c("type","Island","HabitatType"))
relativekud <- data_summary(meltkudframeall,varname="SelectionRatio",groupnames= c("type","Island","HabitatType"))

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
meltcraball$SelectionRatio = as.numeric(meltcraball$SelectionRatio)
meltcraball$varIsle = paste(meltcraball$HabitatType,meltcraball$Island)
meltcrabactual <- subset(meltcraball,type=="actual")
allLocations = allLocations[c("sand","cooper","eastern"),]

for (isle in 1:nrow(allLocations)) {
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Cocos",allLocations[isle,"Cocos"],0)
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Natives",allLocations[isle,"Natives"],0)
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Scaevola",allLocations[isle,"Scaevola"],0)
    relativeall[nrow(relativeall)+1,] = c("actual",rownames(allLocations)[isle],"Unveg.",allLocations[isle,"Sand"],0)
}
relativeall$varIsle = paste(relativeall$HabitatType,relativeall$Island)
relativeall$sd[is.na(relativeall$sd)] = 0
relativeall$SelectionRatio = as.numeric(relativeall$SelectionRatio)
relativeall$sd = as.numeric(relativeall$sd)
meltcrabkud = subset(meltcraball,type=="kud95")
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


#pdf("8.8KUDBoxBarPlots.pdf",width=10,height=7)
p <- ggplot(data=meltcrabkud,aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_boxplot(position=position_dodge()) +
  stat_summary(fun.data="mean_sdl",geom="pointrange") + ggtitle("% Crab Home Range in Each Habitat Type By Island") + facet_wrap(~str_to_title(Island)) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type") 
p = p + theme_bw() + geom_bar(stat="identity",position=position_dodge(),data=relativeall[relativeall$type=="actual",],aes(x=HabitatType,y=SelectionRatio,col="black"),alpha=0.4) 
p = p + theme(legend.position="none",axis.text=element_text(size=14),      #adjust text sizes, etc. for presentations. These lines are altering theme_bw above
              axis.title=element_text(size=17),axis.text.x=element_text(size=12,angle=35,hjust=1), plot.title = element_text(size=20,hjust = 0.5),panel.grid.major.x = element_blank(),strip.text = element_text(size=15))+
        scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00", "#CC79A7")) + scale_colour_manual(values=c("black"))
p 
#dev.off()

#Area of 50%/95% KUD over time
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

