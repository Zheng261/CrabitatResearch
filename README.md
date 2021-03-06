##################################################################
Comprehensive guide to all the crab and island remote sensing code
	Zheng Yan & Tim White & Michael Burnett, August 2018
##################################################################

Before doing anything, run the InitiateWkdirAndImportPackages.R file. This file sets up the working directory (currently this hard drive, but can be configured to be whatever) as well as loads all the required libraries, of which there are many.

The following describes the folders and their contents in "CrabitatResearch", listed in order of presentation in the upcoming paper.



0) FastTrack 
This folder contains a variety of RDS files and CSV files in order to faithfully reproduce all of the figures we have so far in the paper while having to run as little code as possible. 

IF YOU JUST NEED TO SLIGHTLY ADJUST A FIGURE:
	- Run the InitiateWkdirAndImportPackages.R
	- Change work directory to CrabitatResearch/FastTrack, or wherever the GitHub repository has been cloned to
	- Run compiledcrabstats up to the point as marked
	- Run crabpaperplots up to figure 1
	- Find the figure in the file (listed in order) and run it. Adjust as needed


1) CrabTracking

This folder contains all code relevant to cleaning and analyzing the crab telemetry shapefiles obtained by Michael and Tim in 2016/2017.
Prior to using code from this folder, the crab tracks were imported into QGIS and manually trimmed away tracks that were obviously either before the tag was put on or after the tag was taken off. We also use the Palmyra raster that was previously generated by Mike Burnett in his paper. 

A) CrabHabitatAssociation.R and crabitatHomeRange.R are both early versions of the selection ratio calculation and KUD estimates, tested exclusively on 2017 crab tracking data. These files were initially also used to generate/speed-trim CSV files from the raw shapefiles, remove crabs with less than a day tracked, and a removing all tracks that were not on land. If these files still exist, these scripts can be ignored since the selection ratio and KUD calculation code was based on un-normalized data, which we decided to not use. 

B) Crabs2016cleanup.R and crabs2016homerange.R were slightly improved versions of the above scripts, used for 2016 data. They did everything above, but for crabs 2016 data. If the water/speed-trimmed tracks exist in any form, these scripts should be ignroed for the same reason. 

C) Crabs1617crossover.r analyzes both sets of tracks and determines whether correction is needed (it was). It is useful for generating abacus plots for hits over time, as well as producing the final crab track data frame, containing both 2016 and 2017 tracking data minus the paradise island crab. 

D) NormalizedCrabData.r produces the results of 2 approaches on the filtered 2016 and 2017 tracking data: 
	I) Normalizing habitat utilization to produce a single entry per hour (the entry containing the % of time that the crab was in each habitat in that hour), which we did not end up using very much.
	II) Taking the geographic median of tracks hourly and calculating habitat utilization through the 95% KUD, which we did end up using.
Both approaches include using widesIII to find selection ratios. Approach II generates data frames (kudmedframe and mcpframe) containing all habitat distribution information based on the approach. All KUDs (95/50) are also saved into RDS files (7.27HRdata.RDS) for easy import to be plotted.



2) CrabPlotting

A) CrabitatVisual.R is the first script to visualize trimmed crab tracks, and could be useful for comparison with the later versions. Contains whole-island (cooper, sand, palmyra) visualzations for crab tracks both before and after geographic median/time filtering. Crabs are colored by dates tracked. 

B) PrettyCrabMaps.R contains the code to make representative crab tracks and KUDs overlaid with each other for each island. This specific script colors crab tracks by absolute date. An adapted version of this can be found in crabpaperplots.R, and this script might be useful to look back on if its functionality is needed in the future.

C) KUDWaterTrim.R contains scripts for both water-trimming and edge-detecting the crab KUDs, and both stores and plots the final KUDs and calculates their areas. There is also code to analyze home range sufficiency, though the most updated/applicable code has been moved to its separate folder in the stats section.

D) CrabPaperPlots.R (Run through compiledcrabstats.R first!) allows visualization of all important crab statistics (selection ratios, habitat utilization numbers, etc.), the asymptote KUD plot with linear regression, and the most up-to-date representative crab tracks.

E) CrabGifCode.R - Creates a fun gif of crab tracks over time.


3) CrabStats

A) StatisticalTestsCrab.R is the oldest attempt at crab statistics. It includes statistics comparing habitat utilization on both approaches I) and II), as described above. Parts may be useful for reference, but most likely should be generally ignored.

B) GapAnalysis.R produces a shapefile (gapvisualtenhours.shp) that analyzes crab activity before and after gaps (1-6 hours). We did not end up doing anything with this, either.

C) KUDHomeRangeSufficiency.R produces a CSV file of the areas of all water-masked 95 KUDs for crabs, every six hours. This is to determine whether our home range approach was done over a sufficient time range.  

D) CompiledCrabStats.R contains basically every statistical test that was done for the paper, including but not limited to: One sample wilcoxon tests for selection ratio, two sample wilcoxon tests for selection ratios, chi-sq for goodness of fit to determine effects of habitat type on selection ratio, ANOVA to determine differences in model selection, and F-test for differences in variance. All stats are stored in 8.8CrabsStatsList.RDS. This file also constructs the data frames with the stats information within for easy plotting in CrabPaperPlots.R

E) CrabMetaDataTable.R analyzes the survey metadata taken from the 16-17 metadata file from the Crab Tracking folder, combines the metadata with crab tracking statistics (days at liberty, KUD areas - home range/core area, etc) , and exports it into a CSV file. Contains both condensed and non-condensed data. 

4) Mike R Scripts

A) AllCrabPlots.R plots information related to shelter data - crabs in shelter found in each habitat, in/out statistics, etc.

B) sandHabitatAnalysisv1 is a first-attempt at calculating selection ratio for the crabs on Sand island. 

C) survey_barplots contains scripts for generating barplots for crabs found in each area during the day/night 30 minute 2016/17 surveys. 



5) RemoteSensingScripts

A) PalmyraRF-v1-OLDEST is Mike's first draft at classifying Palmyra. I'm not sure exactly how it performs to the current standard - I might have run it once, but I think it didn't work on my computer so I immediately made a copy of it and edited the other copy to work on my machine (should've used git first). PalmyraRF-H2OFriendly-v2 is the second version, which is much less water-trimmed than the first one. It didn't end up performing very well, so I re-implemented the water masking in R (previously done in ENVI), which led to PalmyraRF-v3-MOSTUSEFUL.R, which is the current best version for Palmyra classification. 

B) algaeclassificationtest.R is the first attempt I had at expanding the randomforest algorithm to other locations. I made this script to help Lance with water-masking his rasters. It actually worked decently well - otherwise for this project the script isn't ver useful. 

C) CrappyPalmyra, CrappyTeraina, CrappyTerainaScav, and FanningClassing are all scripts used to classify each of those islands independently with separate training data (the two different scaevola scripts correspond to whether scaevola was included as a variable). Currently, we always use 11x11 GLCM since the higher squares increase accuracy but may lead to significant anti-class edge bias. CrappyTaiaro is based on a visual file (255 int cap) instead of analytic file for RGB values, which I found out after I made the file. The island may be more difficult to classify because of this. 

D) PCApalmyra was my attempt at making GLCM better, since we condense down information into the dimension of highest maintained variance. Turns out, al this did was decrease accuracy, so this script isn't too useful. 

E) MoreIslandClassing is a test case attempting to classify Rawaki. I stopped working on this to work on Teraina instead at the time, but I plan to come back to this.

F) SuperIslandModel and SuperWaterModel are both scripts used to train and produce a randomforest that utilizes all three training data islands to create a generalizable model. ChagosClassing.R is my first attempt at generalizing this model to another island not previously trained on before. It hasn't worked yet, but hopefully that changes in a few days as of August 29/2018. Update: I'm giving up on this for now and trying a different island.

G) SuperCloudModel is a cloud mask trained by ~600 data points spread across all islands to mask out all clouds. It does not work very well, so we'll just try leaving the clouds in and moving on to a different island that doesn't have many clouds. 

H) PukauraClassing uses both the SuperIslandModel and SuperWaterModel to watermask and classify the island of Pukarua. 

