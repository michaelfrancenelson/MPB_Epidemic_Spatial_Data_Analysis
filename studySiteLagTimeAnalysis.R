source("LoadData.R")
source("plottingFunctions.R")


# Plot color-coded study sites --------------------------------------------
pdf(file = paste0(figureDir, "study_sites.pdf"))
plot(studySites, col = colMap$col[match(studySites$studySiteName, colMap$studySiteName)], border = NA)
plot(states, add = T, border = gray(0.5))
legend("bottomleft", legend = colMap$studySiteName, col = colMap$col, pch = 15, bg = "white")
dev.off()


# Plots of survival vs killed trees for Western US ---------------------------------------
cex = 0.3
plotTitle = "Western US"
plotDir = paste0(figureDir, "Western_US/")
treeKillYears = 1998:2010
lagMin = -4; lagMax = 4
filenamePrefix = "Western_US_Survival_Kill_"
plotLagSeriesKillSurvival(mpbSurvivalTreeKill_WesternUs, lagMin, lagMax, treeKillyears, plotTitle, plotDir,colMap, cex, filenamePrefix)
plotLagSeriesKillSurvival(mpbSurvivalTreeKill_WesternUs, lagMin, lagMax, 
                          treeKillyears, plotTitle, plotDir,colMap, cex, paste0("Delta_", filenamePrefix), delta = T)


# Plots of survival vs killed trees for combined study sites US ---------------------------------------
cex = 0.2
plotTitle = "Study Sites Combined"
plotDir = paste0(figureDir, "Study_Sites_Combined/")
filenamePrefix = "StudySitesCombined_Survival_Kill_"
plotLagSeriesKillSurvival(mpbSurvivalTreeKill_StudySites, lagMin, lagMax, 
                          treeKillyears, plotTitle, plotDir, colMap, cex, filenamePrefix, delta = F)
plotLagSeriesKillSurvival(mpbSurvivalTreeKill_StudySites, lagMin, lagMax, 
                          treeKillyears, plotTitle, plotDir,colMap, cex, paste0("Delta_", filenamePrefix), delta = T)



# Plots of survival for individual forests --------------------------------
cex = 0.7
for(row in 1:nrow(colMap)){
  plotTitle = colMap$studySiteName[row]
  forestName = gsub(" ", "_", plotTitle, perl = T)
  plotDir = paste0(figureDir, forestName, "/")
  if (!dir.exists(plotDir)) dir.create(plotDir)
  filenamePrefix = paste0(forestName, "_Survival_Kill_")
  
  paste0("Delta_Kill_", forestName)
  
  spdf = subset(mpbSurvivalTreeKill_StudySites, studySiteName == colMap$studySiteName[row])
  nrow(spdf)
  nrow(mpbSurvivalTreeKill_StudySites)
  plotLagSeriesKillSurvival(spdf, lagMin, lagMax, treeKillyears, 
                            plotTitle, plotDir, colMap, cex, filenamePrefix, delta = F)
}




# moving average of mpb survival ------------------------------------------

nrows = 400; ncols = 400

colMap
forNames = colMap$studySiteName
forExtents = c(extentColorado, extentBeaverhead, extentBlackHills, extentColville)

i = 2

endYears = 1995:2016
endYear = 2010


layout(mat = matrix(1:2, nrow = 1))
plot(raster(matrix(1:1000 / 1000, ncol = 1)), col = heat.colors(1000))

filename = paste0(figureDir, forNames[i], "/MpbSurvivalWinter_", endYear - 1, "_", endYear, ".pdf")
mainTitle = paste0(forNames[i], " winter ", endYear - 1, "-", endYear)
pdf(filename)
par(mar = c(0,0,2,0))
rrr = plotMeanSurvival(mpbSurvivalSpatialPoints, ext = forExtents[[i]], endYear = 2016, nYears = 1, alpha = 1, nrows = nrows, ncols = ncols, nColors = 800, axes = F, box = F, cex = 0.5)
plot(studySites, lwd = 0.1, add = T)
mtext(mainTitle, side = 3)
dev.off()

pdf(paste0(figureDir, "test2.pdf"))
plot(rrr, col = terrain.colors(800))
dev.off()


