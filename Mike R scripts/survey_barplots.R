# Coconut crab survey summary barplots 
# July 27 2017
# Mike Burnett - mburnett@stanford.edu

######## Plotting number of crabs found per survey across habitat types ########

# Read in data
getwd()
setwd("/volumes/Seagate 4tb/Palmyra Crab Research/Crab surveys")

# Install manipulation and plotting packages
#detach(package:dplyr) #clear out dplyr to avoid causing conflicts when installing plyr
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

# Create and view dataframe.
dat = read.csv("palmyra crab surveys 2016 2017 summary trimmed.csv", header = T)
head(dat)
str(dat)
View(dat)

# Trim and filter dataframe down to relevant variables
trimDat = dat[,c("Date","Year","Location","Crab.found","General.habitat.type","Day.night")] 
actualResults = subset(trimDat, Year == 2017 | Year == 2016)
View(trimDat)
View(actualResults)

## Using dplyr grouping function for summarizing data...(not really an essential step, I think)
# grouped = group_by(actualResults,General.habitat.type) %>% group_by(Day.night) # data unchanged, but now grouped by habitat type
# View(grouped)
# summarise(grouped,mean = mean(Crab.found))       # calculate mean num of crab found per survey in each habitat type

## More grouping stuff, doesn't really work...
# class(grouped$Crab.found)
# 
# native = filter(actualResults, General.habitat.type == "Native Trees") 
# head(native)
# class(native$Day.night)
# 
# native = group_by(native,Day.night) # mean number of crabs found at day/night in different habitat types
# summarise(native,mean = mean(Crab.found)) 
# 
# native = filter(actualResults, General.habitat.type == "Native Trees") %>%  group_by(Day.night) %>% summarise(mean = mean(Crab.found))
# 
# mixed = filter(actualResults, General.habitat.type == "Mixed") %>%  group_by(Day.night)
# summarise(mixed,mean = mean(Crab.found))
# 
# cocos = filter(actualResults, General.habitat.type == "Cocos")%>%  group_by(Day.night)
# summarise(cocos,mean = mean(Crab.found))



#### Now to make the plot:
#### Copied code below from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# The next 30-something lines create summarySE function, which allows us to calculate and plot the standard errors of our data.
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  detach(package:dplyr) #clear out dplyr to avoid causing conflicts when installing plyr
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# Now to apply summarySE to our data, currently found in the dataframe actualResults
summed <- summarySE(actualResults, measurevar="Crab.found", groupvars=c("General.habitat.type","Day.night"))
head(summed)

### Simple barplot using ggplot:
# Barplot of crabs/survey by general habitat type, subdivided by day/night:
# Here error bars represent standard error of the mean
#pdf("8.6CrabFoundPlot.pdf",width=10,height=7)
saveRDS(summed, "8.30CrabHabitatFound.RDS")
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
  facet_wrap(~str_to_title(Day.night)) # , panel.grid.minor = element_blank()    #removes unnecessary gridlines
#dev.off()
