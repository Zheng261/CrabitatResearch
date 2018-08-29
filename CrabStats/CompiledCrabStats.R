#############################################################################
########## Begin compiling statistical tests for crab analysis ##############
################  Zheng Yan & Tim White, August 7 2018 ######################
#############################################################################

###############  Starts importing Data   ##################

## Imports median-filtered crab tracks, removes CSV columns and paradise crab
HourlyMedianDF = read.csv("7.25hourlymediandf.csv")
HourlyMedianDF = HourlyMedianDF[,which(colnames(HourlyMedianDF)!='X')]
HourlyMedianDF = subset(HourlyMedianDF,Island!="paradise")

## Reads in MCP and KUD areas, removes CSV columns and paradise crab
mcpmedframe <- read.csv("7.25normMCPFrame.csv")
kudmedframe <- read.csv("7.25normKUDFrame.csv")
mcpmedframe = subset(mcpmedframe,Island!="paradise")
kudmedframe = subset(kudmedframe,Island!="paradise")
mcpmedframe = mcpmedframe[,which(colnames(mcpmedframe)!='X')]
kudmedframe = kudmedframe[,which(colnames(kudmedframe)!='X')]

## Ensures islands are not factors, for usage of string methods later on
mcpmedframe$Island = as.character(mcpmedframe$Island)
kudmedframe$Island = as.character(kudmedframe$Island)
colnames(mcpmedframe) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")
colnames(kudmedframe) <- c("CrabNum","Island","95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand","AvailCocos","AvailNatives","AvailScaevola","AvailSand")

#Adds 0.0001 to each proportion of habitats observed to satisfy WidesIII package preconditions without affecting result
mcpmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")] = mcpmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]/rowSums(mcpmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]) + 0.0001
mcpmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")] = mcpmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]/rowSums(mcpmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]) + 0.0001
kudmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")] = kudmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]/rowSums(kudmedframe[,c("95Cocos","95Natives","95Scaevola","95Sand")]) + 0.0001
kudmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")] = kudmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]/rowSums(kudmedframe[,c("50Cocos","50Natives","50Scaevola","50Sand")]) + 0.0001

#Creates new data frames to hold individual selection ratios
mcpmedwiframe = data.frame(matrix(nrow=length(unique(HourlyMedianDF$CrabNum)),ncol=8))
kudmedwiframe = data.frame(matrix(nrow=length(unique(HourlyMedianDF$CrabNum)),ncol=8))
colnames(mcpmedwiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")
colnames(kudmedwiframe) <- paste0(c("95Cocos","95Natives","95Scaevola","95Sand","50Cocos","50Natives","50Scaevola","50Sand"),"WI")


#Calculates individual selection ratios for each crab 
for (crab in 1:nrow(mcpmedwiframe)) {
  mcp1695ratios = widesIII(mcpmedframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],mcpmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1695ratios = widesIII(kudmedframe[crab,c("95Cocos","95Natives","95Scaevola","95Sand")],kudmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  mcp1650ratios = widesIII(mcpmedframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],mcpmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  kud1650ratios = widesIII(kudmedframe[crab,c("50Cocos","50Natives","50Scaevola","50Sand")],kudmedframe[crab,c("AvailCocos","AvailNatives","AvailScaevola","AvailSand")])
  
  mcpmedwiframe[crab,"95CocosWI"] = mcp1695ratios$wi[1]
  mcpmedwiframe[crab,"95NativesWI"] = mcp1695ratios$wi[2]
  mcpmedwiframe[crab,"95ScaevolaWI"] = mcp1695ratios$wi[3]
  mcpmedwiframe[crab,"95SandWI"] = mcp1695ratios$wi[4]
  
  mcpmedwiframe[crab,"50CocosWI"] = mcp1650ratios$wi[1]
  mcpmedwiframe[crab,"50NativesWI"] = mcp1650ratios$wi[2]
  mcpmedwiframe[crab,"50ScaevolaWI"] = mcp1650ratios$wi[3]
  mcpmedwiframe[crab,"50SandWI"] = mcp1650ratios$wi[4]
  
  kudmedwiframe[crab,"95CocosWI"] = kud1695ratios$wi[1]
  kudmedwiframe[crab,"95NativesWI"] = kud1695ratios$wi[2]
  kudmedwiframe[crab,"95ScaevolaWI"] = kud1695ratios$wi[3]
  kudmedwiframe[crab,"95SandWI"] = kud1695ratios$wi[4]
  
  kudmedwiframe[crab,"50CocosWI"] = kud1650ratios$wi[1]
  kudmedwiframe[crab,"50NativesWI"] = kud1650ratios$wi[2]
  kudmedwiframe[crab,"50ScaevolaWI"] = kud1650ratios$wi[3]
  kudmedwiframe[crab,"50SandWI"] = kud1650ratios$wi[4]
}

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#Fills in crab information (number, island tracked, year)
mcpmedwiframe$CrabNum = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$CrabNum
mcpmedwiframe$Island = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Island
mcpmedwiframe$Year = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Year
kudmedwiframe$CrabNum = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$CrabNum
kudmedwiframe$Island = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Island
kudmedwiframe$Year = HourlyMedianDF[which(!duplicated(HourlyMedianDF$CrabNum)),]$Year

#Melts data frame into ggplot-friendly format 
### Melts median MCP frame into something that ggplot can use
meltmcpallwiframe <- melt(mcpmedwiframe,id=c("CrabNum","Island","Year"))
meltmcpallwiframe$type = revalue(substr(meltmcpallwiframe$variable,1,2),c("95" = "mcp95","50" = "mcp50"))
meltmcpallwiframe$variable = substr(meltmcpallwiframe$variable,3,100)
colnames(meltmcpallwiframe)[5] = 'SelectionRatio'
colnames(meltmcpallwiframe)[4] = "HabitatType"
dfmcpall <- data_summary(meltmcpallwiframe,varname="SelectionRatio",groupnames=c("type","HabitatType"))

### Melts median KUD frame into something that ggplot can use
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

colnames(meltkudallwiframe)[5] = 'SelectionRatio'
colnames(meltkudallwiframe)[4] = "HabitatType"
colnames(meltmcpallwiframe)[5] = 'SelectionRatio'
colnames(meltmcpallwiframe)[4] = "HabitatType"

#Grabs all selection ratios (for all habitat types)
kud95statsAll = subset(meltkudallwiframe,type=="kud95")

#Grabs only native and cocos habitat selection ratios
kud95stats = subset(meltkudallwiframe,type=="kud95" & (HabitatType == "CocosWI" | HabitatType=="NativesWI"))




######STARTS STATS HERE #######
CrabStatsList <- list()

#### Mixed model to detect differences in selection ratio for habitat type - Does habitat type impact selection ratio? (Yes)
#### Chi sq. test to see how selection ratio depends on habitat type
fit <- lmer(SelectionRatio ~ HabitatType + (1|Island) + (1|Year), data= kud95statsAll)
summary(fit)
ftable = drop1(fit, ~. ,test="Chisq")
ftable
CrabStatsList[["MixedModel"]] <- ftable

#### Mixed model to detect differences in selection ratio for habitat type, cocos and natives only 
#### Does habitat type impact selection ratio if we ignore sand/scaevola? (No)
#### Chi sq. test to see how selection ratio depends on habitat type
fit <- lmer(SelectionRatio ~ HabitatType + (1|Island) + (1|Year), data= kud95stats)
summary(fit)
ftable = drop1(fit, ~. ,test="Chisq")
ftable
CrabStatsList[["CocoNatsMixedModel"]] <- ftable


#### ANOVA test to test for differences in selection ratio among islands - Does taking island differences into account make a difference?
### No island random intercept term
fit1 <- lmer(SelectionRatio ~ HabitatType + (1|Island), data= kud95stats, REML=FALSE)
summary(fit1)
### Island random intercept term
fit2 <- lmer(SelectionRatio ~ HabitatType + (1|Island) + (1|Year), data= kud95stats, REML=FALSE)
summary(fit2)
anova(fit1,fit2)
CrabStatsList[["ANOVAMixedModel"]] <- anova(fit1,fit2)

### T test between selection ratios between natives and coco trees. No significant findings
CrabStatsList[["TTest"]] <- t.test(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)

### Wilcoxon test between selection ratios between natives and coco trees. No significant findings
CrabStatsList[["WilcoxTest"]] <- wilcox.test(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)

### Wilcoxon test between selection ratios between natives and coco trees in all islands. Only significant finding on Eastern 
CrabStatsList[["EasternWilcoxTest"]] <-wilcox.test(subset(kud95stats,HabitatType=="NativesWI" & Island=="eastern")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI" & Island=="eastern")$SelectionRatio)
CrabStatsList[["SandWilcoxTest"]] <-wilcox.test(subset(kud95stats,HabitatType=="NativesWI" & Island=="sand")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI" & Island=="sand")$SelectionRatio)
CrabStatsList[["CooperWilcoxTest"]] <-wilcox.test(subset(kud95stats,HabitatType=="NativesWI" & Island=="cooper")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI" & Island=="cooper")$SelectionRatio)

# F test for differences in variance between natives and coco trees. Significant finding - cocos have lower variance
CrabStatsList[["FTest"]] <- var.test(subset(kud95stats,HabitatType=="NativesWI")$SelectionRatio, subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio)


# Testing for differences in male/female crabs
MaleCrabs = taggedMetaDataAll[which(taggedMetaDataAll$Sex == "Male"),"Crab Number"]
FemaleCrabs = taggedMetaDataAll[which(taggedMetaDataAll$Sex == "Female"),"Crab Number"]


CrabStatsList[["MFNatives"]] <- wilcox.test(subset(kudmedwiframe,CrabNum%in%MaleCrabs)$'95NativesWI',
            subset(kudmedwiframe,CrabNum%in%FemaleCrabs)$'95NativesWI')

CrabStatsList[["MFCocos"]] <- wilcox.test(subset(kudmedwiframe,CrabNum%in%MaleCrabs)$'95CocosWI',
            subset(kudmedwiframe,CrabNum%in%FemaleCrabs)$'95CocosWI')

CrabStatsList[["MFAreas"]] <- wilcox.test(subset(k95meltRO,CrabNum%in%MaleCrabs)$'value',
            subset(k95meltRO,CrabNum%in%FemaleCrabs)$'value')


mean(subset(kudmedwiframe,CrabNum%in%MaleCrabs)$'95NativesWI')
mean(subset(kudmedwiframe,CrabNum%in%FemaleCrabs)$'95NativesWI')

wilcox.test(subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio, mu = 1, alternative = "two.sided")
#t.test(subset(kud95stats,HabitatType=="CocosWI")$SelectionRatio, mu = 1, alternative = "two.sided")

#saveRDS(CrabStatsList,"8.8CrabStatsList.RDS")

crabStatsList <- readRDS("8.8CrabStatsList.RDS")
