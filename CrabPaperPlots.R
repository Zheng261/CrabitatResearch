pdf("8.1EditedNormCompiledAnalysis.pdf",width=9,height=5)

meltmcpallwiframe <- melt(mcpmedwiframe,id=c("CrabNum","Island","Year"))
meltmcpallwiframe$type = revalue(substr(meltmcpallwiframe$variable,1,2),c("95" = "mcp95","50" = "mcp50"))
meltmcpallwiframe$variable = substr(meltmcpallwiframe$variable,3,100)
dfmcpall <- data_summary(meltmcpallwiframe,varname="value",groupnames=c("type","variable"))
meltkudallwiframe <- melt(kudmedwiframe,id=c("CrabNum","Island","Year"))
meltkudallwiframe$type = revalue(substr(meltkudallwiframe$variable,1,2),c("95" = "kud95","50" = "kud50"))
meltkudallwiframe$variable = substr(meltkudallwiframe$variable,3,100)
colnames(meltkudallwiframe)[5] = 'SelectionRatio'
colnames(meltkudallwiframe)[4] = "HabitatType"

dfkudall <- data_summary(meltkudallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType"))
dfall = rbind(dfmcpall,dfkudall)

meltall = rbind(meltmcpallwiframe,meltkudallwiframe)
dfall$HabitatType = revalue(dfall$HabitatType,c("NativesWI" = "Natives", "SandWI"="Unveg.","ScaevolaWI"="Scaevola","CocosWI"="Cocos"))
meltall$HabitatType = revalue(meltall$HabitatType,c("NativesWI" = "Natives", "SandWI"="Unveg.","ScaevolaWI"="Scaevola","CocosWI"="Cocos"))

p <- ggplot(data=meltall,aes(x=HabitatType,y=SelectionRatio,fill=type)) + geom_violin(position=position_dodge()) + 
  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17") + coord_cartesian(ylim = c(0, 5)) + ylab("Selection Ratio") + xlab("Habitat Type")
p


p <- ggplot(data=dfall[which(dfall$type=="kud95"),],aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
                position=position_dodge(.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17, KUD") + coord_cartesian(ylim = c(0, 3)) + ylab("Selection Ratio") + xlab("Habitat Type")
p

#All crabs ever, plotting by year
dfmcpallvy <- data_summary(meltmcpallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Year"))
dfkudallvy <- data_summary(meltkudallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Year"))
dfallvy = rbind(dfmcpallvy,dfkudallvy)

#All crabs ever, plotting by island 
dfmcpallvi <- data_summary(meltmcpallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Island"))
dfkudallvi <- data_summary(meltkudallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType","Island"))
dfallvi = rbind(dfmcpallvi,dfkudallvi)
dfallvi = dfallvi[which(dfallvi$HabitatType=="CocosWI" | dfallvi$HabitatType=="NativesWI"),]
dfallvi$HabitatType = revalue(dfallvi$HabitatType,c("NativesWI"="Natives","CocosWI"="Cocos"))

p <- ggplot(data=dfallvi[which(dfallvi$type=="kud95" | dfallvi$type=="kud50"),],aes(x=HabitatType,y=SelectionRatio,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
                position=position_dodge(.9)) + theme(text = element_text(size=10),
                                                     axis.text.x = element_text(angle=60, hjust=1)) +  ggtitle("Average Habitat Selection Ratio for Coconut Crabs By Island, KUD") + coord_cartesian(ylim = c(0, 4)) + facet_wrap(~Island) + ylab("Selection Ratio") + xlab("Habitat Type")
p

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

p <- ggplot(data=subset(relativeall,type=="kud95" | type=="actual"),aes(x=HabitatType,y=SelectionRatio,fill=type)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
                position=position_dodge(.9)) + theme(text = element_text(size=10),
                                                     axis.text.x = element_text(angle=60, hjust=1)) + ggtitle("% Crab Home Range (KUD95) in Each Habitat Type by Island, \nCompared to Real Habitat Ratio") + facet_wrap(~Island) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type")
p



p <- ggplot(data=meltcrabactual,aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_bar(stat="identity",position=position_dodge()) +
  ggtitle("% Habitat Availability by Island") + facet_wrap(~Island) + ylab("% Available Habitat") + xlab("Habitat Type") + coord_cartesian(ylim = c(0, 1))
p


### KUD ONLY

p <- ggplot(data=subset(meltall,type=="kud95"),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_violin(position=position_dodge()) + 
  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17, KUD95") + ylab("Selection Ratio") + xlab("Habitat Type")
p




pdf("8.8CocoNatKUDSelectionRatio.pdf",width=10,height=7)
kud95stats = subset(meltkudallwiframe,type=="kud95" & (HabitatType == "CocosWI" | HabitatType=="NativesWI"))
statString = paste("F-test for comparison of variances: P =",round(unlist(var.test(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)[3]),3))
statString = paste(statString,"\n","Ratio of variances (Cocos/Natives) =",round(var(subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)/var(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio),3))
statString = paste(statString,"\n","Wilcoxon signed-rank test for comparison of population means: P = ",round(unlist(wilcox.test(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)[3]),3))
p <- ggplot(data=subset(meltall,type=="kud95" & (HabitatType == "Cocos" | HabitatType == "Natives")),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00", "#CC79A7")) + geom_violin(position=position_dodge()) + 
  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17, KUD95") + ylab("Selection Ratio") + xlab("Habitat Type") + theme_bw()+
  theme(legend.position="none",axis.text=element_text(size=17),      #adjust text sizes, etc. for presentations. These lines are altering theme_bw above
              axis.title=element_text(size=18),plot.title = element_text(size=22,hjust = 0.5),panel.grid.major.x = element_blank(),strip.text = element_text(size=15)) 
p
dev.off()

p <- ggplot(data=subset(meltall,type=="kud95" & (HabitatType == "Cocos" | HabitatType == "Natives")),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_violin(position=position_dodge()) + 
  stat_summary(fun.data=mean_sdl,geom="pointrange",position=position_dodge(0.9)) + ggtitle("Average Habitat Selection Ratio for Coconut Crabs 16-17 By Island, KUD95") + ylab("Selection Ratio") + xlab("Habitat Type") + facet_wrap(~Island)
p


p <- ggplot(data=subset(dfallvi,type=="kud95"),aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=SelectionRatio-sd, ymax=SelectionRatio+sd), width=.2,
                position=position_dodge(.9)) + theme(text = element_text(size=10),
                                                     axis.text.x = element_text(angle=60, hjust=1)) +  ggtitle("Average Habitat Selection Ratio for Coconut Crabs By Island, KUD95") + coord_cartesian(ylim = c(0, 1.5)) + facet_wrap(~Island) + ylab("Selection Ratio") + xlab("Habitat Type")
p

meltcrabkud = subset(meltcraball,type=="kud95")
p <- ggplot(data=meltcrabkud,aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_violin(position=position_dodge()) +
  stat_summary(fun.data="mean_sdl",geom="pointrange") + ggtitle("% Crab Home Range in Each Habitat Type By Island") + facet_wrap(~Island) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type") 
p = p + geom_bar(stat="identity",position=position_dodge(),data=relativeall[relativeall$type=="actual",],aes(x=HabitatType,y=SelectionRatio,fill="purple"),alpha=0.4)
p

pdf("8.8KUDBoxBarPlots.pdf",width=10,height=7)
p <- ggplot(data=meltcrabkud,aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_boxplot(position=position_dodge()) +
  stat_summary(fun.data="mean_sdl",geom="pointrange") + ggtitle("% Crab Home Range in Each Habitat Type By Island") + facet_wrap(~str_to_title(Island)) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type") 
p = p + theme_bw() + geom_bar(stat="identity",position=position_dodge(),data=relativeall[relativeall$type=="actual",],aes(x=HabitatType,y=SelectionRatio,col="black"),alpha=0.4) 
p = p + theme(legend.position="none",axis.text=element_text(size=14),      #adjust text sizes, etc. for presentations. These lines are altering theme_bw above
              axis.title=element_text(size=17),axis.text.x=element_text(size=12,angle=35,hjust=1), plot.title = element_text(size=20,hjust = 0.5),panel.grid.major.x = element_blank(),strip.text = element_text(size=15))+
        scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00", "#CC79A7")) + scale_colour_manual(values=c("black"))
p 
dev.off()

p <- ggplot(data=meltcrabkud[which(meltcrabkud$HabitatType=="Cocos" | meltcrabkud$HabitatType=="Natives"),],aes(x=HabitatType,y=SelectionRatio,fill=HabitatType)) + geom_violin(position=position_dodge()) +
  stat_summary(fun.data="mean_sdl",geom="pointrange") + ggtitle("% Crab Home Range in Each Habitat Type By Island") + facet_wrap(~Island) + coord_cartesian(ylim = c(0, 1)) + ylab("% Home Range in Habitat") + xlab("Habitat Type") 
p = p + geom_bar(stat="identity",position=position_dodge(),data=relativeall[relativeall$type=="actual" & (relativeall$HabitatType=="Cocos" | relativeall$HabitatType=="Natives"),],aes(x=HabitatType,y=SelectionRatio,fill="purple"),alpha=0.4)
p

#Area of 50%/95% KUD over time
ggplot(k95melt,aes(x=HabitatType,y=SelectionRatio,color=as.factor(CrabNum))) + geom_line() + coord_cartesian(xlim=c(0,200)) + ggtitle("Area of 95% KUD over time, by crab") + xlab("Time Elapsed") + ylab("Area (km^2)")


dev.off()
