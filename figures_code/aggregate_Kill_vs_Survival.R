plotSeriesName = "multiYear_mean_MPB_survival_tree_kill_scatterplots"
plotSeriesDir = paste0(figureDir, plotSeriesName)
if(!dir.exists(plotSeriesDir)) dir.create(plotSeriesDir)

plotSeriesDescription = "
# Plots of the mean number of trees killed vs mean overwinter survival, aggregated over the entire study area.
#
# Each data point represents means over all the raster cells in the study area for one year.
#
# The y-axis is the mean number of new red-stage trees detected in that year.
# The x-axis is the mean modeled MPB survival averaged over the previous 'nYears' years.
# There are separate plots for values of 'nYears' from 1 to 12 years.

"
writeLines(plotSeriesDescription, paste0(plotSeriesDir, "/00_plot_descriptions.txt"))





str(mpbSurvivalSitesMultiYearAvgAggregate[[1]], 1)
siteMeanKills

i = 1
ss = mpbSurvivalSitesMultiYearAvgAggregate[[i]]
plot(ss$year, ss$WesternUS, type = "l")
sapply(2:8, function(i) {
  ss = mpbSurvivalSitesMultiYearAvgAggregate[[i]]
  points(ss$year, ss$WesternUS, type = "l", col = i)  
})



str(siteMeanKills, 0)

getMultiYearMeanSurvivalKill = function(killDF, survivalDFList, nYears, siteName){
  
  ssK = subset(killDF, site == siteName)
  # Keep only the year and sum columns
  ssK = ssK[, c("year", "meanSumNonzero", "meanSum")]
  survivalDF =  survivalDFList[[nYears]]
  which(names(survivalDF) == siteName)
  names(survivalDF)
  ssS = survivalDF[, c(1, which(names(survivalDF) == siteName))]
  names(ssS)[2] = "meanSurvival"
  dfOut = merge(ssK, ssS)
  names(dfOut)[2:3] = c("meanKill_nonzeroCells", "meanKill_allCells")
  return(dfOut)
  
}



nYears = 2
site = 1

killYears = 1997:2010

for(site in 1:5){
  siteName = names(shapeSites)[site]
  print(siteName)
  filename = paste0(plotSeriesDir, "/", plotSeriesName, "_", siteName, ".pdf")
  pdf(file = filename)
  for(nYears in 1:12){
    print(nYears)
    df1 = getMultiYearMeanSurvivalKill(siteMeanKills, mpbSurvivalSitesMultiYearAvgAggregate, nYears, siteName)
    plot(df1$meanSurvival, df1$meanKill_nonzeroCells, pch = 16, xlab = "Mean MPB Overwinter Survival", ylab = "Mean trees killed per cell")
    # text(x = df1$meanSurvival, y = df1$meanKill_nonzeroCells, labels = killYears, cex = 0.5, pos = 3, offset = 0.3)
    mtext(paste0(siteName, "\nmean MPB survival in previous ", nYears, " years"))
  }
  dev.off()
}

for(site in 1:5){
  siteName = names(shapeSites)[site]
  print(siteName)
  filename = paste0(plotSeriesDir, "/", plotSeriesName, "_", siteName, "_years.pdf")
  pdf(file = filename)
  for(nYears in 1:12){
    print(nYears)
    df1 = getMultiYearMeanSurvivalKill(siteMeanKills, mpbSurvivalSitesMultiYearAvgAggregate, nYears, siteName)
    plot(df1$meanSurvival, df1$meanKill_nonzeroCells, pch = 16, xlab = "Mean MPB Overwinter Survival", ylab = "Mean trees killed per cell", type = "n")
    text(x = df1$meanSurvival, y = df1$meanKill_nonzeroCells, labels = killYears, cex = 0.7, pos = 3, offset = 0)
    mtext(paste0(siteName, "\nmean MPB survival in previous ", nYears, " years"))
  }
  dev.off()
}


