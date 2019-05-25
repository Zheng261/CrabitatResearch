# Coconut crab habitat analysis v1
# Sept 4 2017
# Mike Burnett - mburnett@stanford.edu

######## Habitat Analysis on Sand Island crude habitat trace (adehabitatHS) ########

# Read in data
getwd()
setwd("/Users/Michael/OneDrive/Documents/Stanford Schoolwork/2017 - Palmyra")

# Install manipulation, habitat analysis, and plotting packages
detach(package:dplyr) #clear out dplyr to avoid causing conflicts when installing plyr
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("adehabitatHS")
library(adehabitatHS)

# Create and view dataframes for available and used habitat.
avail = read.csv("sandTrace-HabitatAvail.csv", header = T)
used = read.csv("sandTrace-HabitatUsed.csv", header = T)
avail = avail[,c("Native","Cocos")]  #turns out adehabitatHS doesn't like the crab numbers as their own column, so trim them here
used = used[,c("Native","Cocos")]
head(avail)
head(used)


### Compositional analysis

compana(used, avail, test = c("randomisation", "parametric"),
        rnv = 0.001, nrep = 1000, alpha = 0.01)
#show(compana)



### Manly selection ratios and eigenanalysis

tav = c(.9329858, .06701491)
names(tav) = c("native","cocos")
head(tav)

WiII <- widesII(used, tav)   # Selection ratio computation
show(WiII)

eis=eisera(used, avail, scannf = TRUE, nf=2)   # Attempt at eigenanalysis; didn't really work for this data?
show(eis)
barplot(eis$eig)
scatter(eis)
