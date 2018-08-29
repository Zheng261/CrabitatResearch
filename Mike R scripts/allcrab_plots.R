# Surveyed coconut crab barplots 
# July 27 2017
# Mike Burnett - mburnett@stanford.edu

## Plotting number of crabs found per survey across habitat types

# Read in data
getwd()
#setwd("/Users/Michael/OneDrive/Documents/Stanford Schoolwork/2017 - Palmyra")

# Install manipulation and plotting packages
#detach(package:dplyr) #clear out dplyr to avoid causing conflicts when installing plyr
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

# Create and view dataframe.
dat = read.csv("all surveyed crabs palmyra 2016 2017.csv", header = T)
head(dat)
str(dat)
View(dat)

# Trim and filter dataframe down to relevant variables; add columns for parsing out new variables
trimDat = dat[,c(3,4,5,8,9,10,13)]
actualResults = subset(trimDat, (Year == 2017 | Year == 2016) & (!is.na(Shelter.or.out))) 
View(actualResults)
actualResults["simpleShelter"] <- NA #new column for simplified in/out shelter label
actualResults["treeShelter"] <- NA   #new column for tree associations
actualResults$Shelter.or.out = as.character(actualResults$Shelter.or.out)
dat
datcocos = subset(dat,Gen..habitat=="Cocos")
View(datcocos)
View(dat)
######## Barplot showing crabs per survey across habitat and night/day ########
colnames(datcocos)
#loop to populate new simpleShelter and treeShelter columns based on column 7
for (i in 1:nrow(actualResults)){
  actualResults[i,7] <- tolower(actualResults[i,7]) #Make shelter info lowercase before running tests on it
  if (actualResults[i,7] != "out") actualResults[i,8]="in"
  else {actualResults[i,8]="out"
        actualResults[i,9]="out"
  }
  if (grepl("tournefortia",actualResults[i,7]) || grepl("pisonia",actualResults[i,7]) || grepl ("pandanus",actualResults[i,7]))    actualResults[i,9]="native"
  else {
  if (grepl("calophyllum",actualResults[i,7]) || grepl("terminalia",actualResults[i,7]))  actualResults[i,9]="non-native trees"
  else {
  if (grepl("ground",actualResults[i,7]) || grepl("asplenium",actualResults[i,7]) || grepl ("phymilosaurus",actualResults[i,7]))    actualResults[i,9]="other"
  else {
    if (actualResults[i,7] != "in" && actualResults[i,7] != "out") actualResults[i,9]="other"
    if (grepl("coco",actualResults[i,7])) actualResults[i,9]="coconut"
  }
  }
  }
}

# Create actualResultsin by filtering out crabs that were not in shelters
actualResultsin = subset(actualResults,simpleShelter=="in" & !is.na(treeShelter))
View(actualResultsin)

nrow(subset(actualResults,simpleShelter=="in" & Year == 2017))
nrow(subset(actualResultsin,simpleShelter=="in" & Year == 2017))
nrow(subset(actualResults,simpleShelter=="in" & Year == 2016))
nrow(subset(actualResultsin,simpleShelter=="in" & Year == 2016))
#############STILL NEED TO FIX: re-ordering levels before plotting
#levels(actualResultsin$treeShelter) <- c("coconut","native","non-native trees","ground burrow/understory","manmade","NA")

# summary: number of crabs per category combination
datasummary <- ddply(actualResultsin, c("Gen..habitat", "treeShelter"), summarise,
               N    = length(treeShelter)
)
datasummary


### Create the plot--here each column adds up to 1.0 (shows relative proportions, not total quantities)

ggplot(aes(x=Gen..habitat, weight=N, fill=treeShelter), data=datasummary)+
  geom_bar(position='fill') + 
  scale_fill_discrete("Shelter type") + 
  labs(x="Forest type", y="Proportion of all crab shelters observed", title="Shelter types used by crabs in each forest type")+
  theme(axis.text=element_text(size=13),      #adjust text sizes, etc. for presentations.
    axis.title=element_text(size=15),
    title=element_text(size=16),
    legend.text=element_text(size=12)
    )
    #legend.position=c(.09,.9))                        #move legend to inside of plot
### Stacked bar - total # crabs by habitat type, with subhabitat charted #######

ggplot(data = datasummary, aes(x = treeShelter,y=N,fill=treeShelter)) + geom_bar(stat="identity",position=position_dodge()) + facet_wrap(~Gen..habitat,scales="free_y") +
  theme_bw() +
  #coord_flip() + 
  labs(x="Habitat type", y="Sheltered crabs surveyed")+ #title="Crabs found in shelters in each forest type") +  
  #scale_fill_brewer(palette = "Set1") +
  #guides(fill=guide_legend(title="Shelter type")) +     # legend title
  ggtitle("Crab Shelter Types By Predominant Area Habitat Type") +
  theme(panel.grid.major.x = element_blank(),
        axis.text=element_text(size=10),      #adjust text sizes, etc. for presentations.
        axis.text.x = element_text(angle=25,hjust=1),
        axis.title=element_text(size=18),
        title=element_text(size=20),
        #legend.text=element_text(size=18),
        #legend.position=c(.22,.80)
        )  + theme(legend.position="none")






######## Simple total in/out barplot ########

#loop to populate new simpleShelter and treeShelter columns based on column 7
for (i in 1:nrow(actualResults)){
  actualResults[i,7] <- tolower(actualResults[i,7]) #Make shelter info lowercase before running tests on it
  if (actualResults[i,7] != "out" && actualResults[i,7] != "Out") actualResults[i,8]="in"
  else {actualResults[i,8]="out"
  }
}

datasummary <- ddply(actualResults, c("Gen..habitat", "simpleShelter", "Day.night"), summarise,
                     N    = length(treeShelter)
)
datasummary

# Create the plot--here each column adds up to 1.0 (shows relative proportions, not total quantities)
ggplot(aes(x=Gen..habitat, weight=N, fill=simpleShelter), data=datasummary)+
  geom_bar(position='fill') + 
  scale_fill_discrete("Crab in shelter?") + 
  labs(x="Forest type", y="Proportion of all crabs surveyed", title="Shelter in/out ratio in each forest type")

## Plot grouped by both day/night and habitat type (just discovered faceting! whoa!)
ggplot(aes(x=Day.night, y=N, fill=simpleShelter), data=datasummary)+
  geom_bar(position='fill', stat='identity') +
  facet_grid(~Gen..habitat) +
  scale_fill_discrete("Crab in shelter?") + 
  labs(x="Time of day; forest type", y="Proportion of crabs surveyed", title="Shelter in/out ratio in each forest type and time of day")
  #+theme(axis.text=element_text(size=17),      #adjust text sizes, etc. for presentations
  #axis.title=element_text(size=20,margin = margin(t = 1, r = 20, b = 0, l = 0)))


