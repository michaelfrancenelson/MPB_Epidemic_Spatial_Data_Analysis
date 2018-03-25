source("LoadData.R")



# Plot color-coded study sites --------------------------------------------
x = factor(studySites$FOREST)
pdf(file = paste0(figureDir, "study_sites.pdf"))
plot(studySites, col = match(studySites$FORCODE, colMap$forcodes), border = NA); plot(states, add = T, border = gray(0.5))
legend("bottomleft", legend = colMap$fornames, col = colMap$col, pch = 16, bg = "white")
dev.off()




# Plots of survival vs killed trees for Western US ---------------------------------------
cex = 0.3
plotTitle = "Western US"
plotDir = paste0(figureDir, "Western_US/")
treeKillYears = 1998:2010
lagMin = -4; lagMax = 4
filenamePrefix = "Western_US_Survival_Kill_"
plotLagSeriesKillSurvival(treeKill_MpbSurvival_US, lagMin, lagMax, treeKillyears, plotTitle, plotDir,colMap, cex, filenamePrefix)
plotLagSeriesKillSurvival(treeKill_MpbSurvival_US, lagMin, lagMax, 
                          treeKillyears, plotTitle, plotDir,colMap, cex, paste0("Delta_", filenamePrefix), delta = T)


# Plots of survival vs killed trees for combined study sites US ---------------------------------------
plotTitle = "Study Sites Combined"
plotDir = paste0(figureDir, "Study_Sites_Combined/")
filenamePrefix = "StudySitesCombined_Survival_Kill_"
plotLagSeriesKillSurvival(treeKill_MpbSurvival_StudySites, lagMin, lagMax, 
                          treeKillyears, plotTitle, plotDir,colMap, cex, filenamePrefix, delta = F)
plotLagSeriesKillSurvival(treeKill_MpbSurvival_StudySites, lagMin, lagMax, 
                          treeKillyears, plotTitle, plotDir,colMap, cex, paste0("Delta_", filenamePrefix), delta = T)



# Plots of survival for individual forests --------------------------------
cex = 0.7
for(row in 1:nrow(colMap)){
  plotTitle = colMap$fornames[row]
  forestName = gsub(" ", "_", plotTitle, perl = T)
  plotDir = paste0(figureDir, forestName, "/")
  if (!dir.exists(plotDir)) dir.create(plotDir)
  filenamePrefix = paste0(forestName, "_Survival_Kill_")
  
  paste0("Delta_Kill_", forestName)
  forcode = colMap$forcodes[row]
  
  plotLagSeriesKillSurvival(treeKill_MpbSurvival_StudySites, lagMin, lagMax, treeKillyears, 
                            plotTitle, plotDir, colMap, cex, filenamePrefix, forcode = forcode, delta = F)
}



