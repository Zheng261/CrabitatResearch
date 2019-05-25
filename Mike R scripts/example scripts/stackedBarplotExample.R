# stacked barplot example

getwd()
setwd("/Users/timwhite/Documents/Kiribati Sharks/SPOT tag paper/SharkPaper_SkyTruthAnalyses")
lessinfo = read.csv("/Users/timwhite/Documents/Kiribati Sharks/SPOT tag paper/SharkPaper_SkyTruthAnalyses/anonymousCV.csv", header = T)

# Homogenizing up the Country names from different data sets (i.e. clav says South Korea and ffa say Korea, Republic of)
lessinfo$flag[which(lessinfo$flag == "Taiwan, Republic of China")] = "Taiwan"  # convert the first string to the second string for uniformity
lessinfo$flag[which(lessinfo$flag == "Korea, Republic of")] = "South Korea"
lessinfo$flag[which(lessinfo$flag == "Republic of Kiribati")] = "Kiribati"
lessinfo$flag = as.character(lessinfo$flag)   # I think this is necessary to fully remove the old categories ('Korea, republic of') from showing up in the pie chart
lessinfo$flag[which(lessinfo$flag == "United States of America")] = "USA"

# Homegenizing the Vessel Type (i.e. turning Tuna Longliners in to just Longliners)
lessinfo$shiptype[which(lessinfo$shiptype == "Single Purse Seine")] = "Purse seiners"
lessinfo$shiptype[which(lessinfo$shiptype == "Tuna purse seiners")] = "Purse seiners"
lessinfo$shiptype[which(lessinfo$shiptype == "Tuna longliners")] = "Longliners"


# barplot of number of ships by nation and by fishing vessel type
quartz()

ggplot(data = lessinfo, aes(x = lessinfo$flag, fill = shiptype)) + geom_bar() + 
  scale_x_discrete(limits = names(otab)) + 
  theme_classic() +
  coord_flip() + 
  labs(x="Country", y="Number of fishing vessels") +  
  scale_fill_brewer(palette = "Set1") +
  guides(fill=guide_legend(title="Vessel type")) +     # legend title
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 16), title = element_text(size=16, face = 'bold'), axis.text=element_text(size=13, color = "black"), axis.title=element_text(size=14,face="bold"))
# removed from labs line for Biological Conservation format (no title): title="Fishing vessels by country and vessel type"  

# GREYSCALE FIGURE FOR BIOLOGICAL CONSERVATION (Remove red and blue bars)

quartz()
ggplot(data = lessinfo, aes(x = lessinfo$flag, fill = shiptype)) + geom_bar(colour = "black") + #colour = black adds black border to bars
  scale_x_discrete(limits = names(otab)) + 
  coord_flip() + 
  labs(x="Country", y="Number of fishing vessels") +  
  theme_bw() + 
  scale_fill_grey() +
  guides(fill=guide_legend(title="Vessel type")) +     # legend title
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 15), title = element_text(size=16, face = 'bold'), axis.text=element_text(size=13, color = "black"), axis.title=element_text(size=14,face="bold"),panel.grid.minor = element_blank(), panel.grid.major = element_blank())


