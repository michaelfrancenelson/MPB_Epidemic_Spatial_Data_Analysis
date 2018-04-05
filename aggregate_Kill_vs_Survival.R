mpbSurvivalSitesMultiYearAvgAggregate
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

for(site in 1:5){
  siteName = names(shapeSites)[site]
  print(siteName)
  filename = paste0(figureDir, "aggregate_multiYear_average_survival", siteName, ".pdf")
  pdf(file = filename)
  for(nYears in 1:12){
    print(nYears)
    df1 = getMultiYearMeanSurvivalKill(siteMeanKills, mpbSurvivalSitesMultiYearAvgAggregate, nYears, siteName)
    plot(df1$meanSurvival, df1$meanKill_nonzeroCells, pch = 16, xlab = "Mean MPB Overwinter Survival", ylab = "Mean trees killed per cell")
    mtext(paste0(siteName, "\nmean MPB survival in previous ", nYears, " years"))
  }
  dev.off()
}


