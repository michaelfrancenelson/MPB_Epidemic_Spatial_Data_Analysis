plotSeriesName = "MPB_survival_tree_kill_scatterplots"


if(!dir.exists(paste0(figureDir, plotSeriesName))) dir.create(paste0(figureDir, plotSeriesName))
plotSeriesDescription = "
# Scatterplots of number of red stage trees vs. modeled MPB survival for one winter (not a multi-year average).
#
# Each point represents a 1-sq km cell in the given study area.
#
# x-axis: MPB survival for the winter in the cell
# y-axis: Number of new red stage trees observed in the cell.
#
# Each plot shows the points for a different pair of winter (mpb survival) and summer (tree kill) seasons,
# the years of which are given in the plot heading.  
# 
# Different lags are represented inthe different series of plots.  For example, a plot with winter 1997/1998 
# and summer 2001 shows trees that were killed by MPB in summer 2000, i.e. in the third year following the 
# winter corresponding to the survival values.
"
writeLines(plotSeriesDescription, paste0(figureDir, plotSeriesName, "/00_plot_descriptions.txt"))


# Plots of survival vs killed trees for Western US ---------------------------------------
cex = 0.3
plotTitle = "Western US"
plotDir = paste0(figureDir, plotSeriesName, "/Western_US/")
if(!dir.exists(plotDir)) dir.create(plotDir)
treeKillYears = 1998:2010
lagMin = -4; lagMax = 4
filenamePrefix = "Western_US_Survival_Kill_"
plotLagSeriesKillSurvival(mpbSurvivalTreeKill_WesternUs, lagMin, lagMax, treeKillyears, plotTitle, plotDir,colMap, cex, filenamePrefix)
# plotLagSeriesKillSurvival(mpbSurvivalTreeKill_WesternUs, lagMin, lagMax, 
                          # treeKillyears, plotTitle, plotDir,colMap, cex, paste0("Delta_", filenamePrefix), delta = T)

# Plots of survival vs killed trees for combined study sites US ---------------------------------------
cex = 0.2
plotTitle = "Study Sites Combined"
plotDir = paste0(figureDir, plotSeriesName, "/Study_Sites_Combined/")
if(!dir.exists(plotDir)) dir.create(plotDir)
filenamePrefix = "StudySitesCombined_Survival_Kill_"
plotLagSeriesKillSurvival(mpbSurvivalTreeKill_StudySites, lagMin, lagMax, 
                          treeKillyears, plotTitle, plotDir, colMap, cex, filenamePrefix, delta = F)
# plotLagSeriesKillSurvival(mpbSurvivalTreeKill_StudySites, lagMin, lagMax, 
                          # treeKillyears, plotTitle, plotDir,colMap, cex, paste0("Delta_", filenamePrefix), delta = T)

# Plots of survival for individual forests --------------------------------
cex = 0.7
for(row in 1:nrow(colMap)){
  plotTitle = colMap$studySiteName[row]
  forestName = gsub(" ", "_", plotTitle, perl = T)
  plotDir = paste0(figureDir, plotSeriesName, "/", forestName, "/")
  if (!dir.exists(plotDir)) dir.create(plotDir)
  filenamePrefix = paste0(forestName, "_Survival_Kill_")
  
  paste0("Delta_Kill_", forestName)
  
  spdf = subset(mpbSurvivalTreeKill_StudySites, studySiteName == colMap$studySiteName[row])
  nrow(spdf)
  nrow(mpbSurvivalTreeKill_StudySites)
  plotLagSeriesKillSurvival(spdf, lagMin, lagMax, treeKillyears, 
                            plotTitle, plotDir, colMap, cex, filenamePrefix, delta = F)
}
