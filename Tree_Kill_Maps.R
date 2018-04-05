source("LoadData.R")
ls()

treeKillYears = 1997:2010


layer = 10
site = 1

for(site in 1:length(stackSitesContorta)){
  print(names(stackSitesContorta)[site])
  siteName = names(stackSitesContorta)[site]
  filename = paste0(figureDir, "yearly_tree_kill_", siteName, ".pdf")
  pdf(file = filename)
  zlim = c(0, 5000)
  # zlim = c(0, max(10, rangeD[2]))
  for(layer in 1:length(treeKillYears)){
    dat = subset(stackSitesContorta[[site]], layer) + subset(stackSitesPonderosa[[site]], layer)
    rangeD = range(dat[], na.rm = T)
    plot(dat, axes = F, box = F, zlim = zlim)
    plot(shapeSites[[site]], add = T, lwd = 0.4)
    title(siteName)
    mtext(paste0("red stage observed ", treeKillYears[layer]), side = 3)
  }
  dev.off()
}

getYearLayers = function(stackR, years){
  indices = c()
  for(year in years) indices = c(indices, which(grepl(year, names(stackR))))
  return(subset(stackR, indices))
}
plotKill_Vs_MultiYearMeanSurvival = function(survivalSPDF, survivalYears, killYear, killStack){
  killRaster = getYearLayers(killStack, killYear)
  indices = which(killRaster[] > 0)
  coords = coordinates(killRaster)[indices, ]
  neighbors = nabor::knn(coordinates(survivalSPDF), coords, k = 1)
  spdfColumns = sapply(survivalYears, function(x) which(grepl(x, names(survivalSPDF)))) 
  if(length(spdfColumns) > 1) survivals = apply(survivalSPDF@data[neighbors$nn.idx, spdfColumns], 1, mean) else
    survivals = survivalSPDF@data[neighbors$nn.idx, spdfColumns]
    # survivalRaster = calc(getYearLayers(survivalStack, survivalYears), mean)
  # dfOut = data.frame(treesKilled = killRaster[indices], MpbSurvival = survivalRaster[indices])
  # dfOut = data.frame(treesKilled = killRaster[indices], MpbSurvival = extract(survivalRaster, coords))
  dfOut = data.frame(treesKilled = killRaster[indices], MpbSurvival = survivals)
  plot(dfOut$MpbSurvival, dfOut$treesKilled, pch = 16, cex = 0.3, xlim = c(0, 1), xlab = "MPB Overwinter Survival", ylab = "Trees Killed per Cell")  
  return(dfOut)
}





# For testing

survivalSPDF = mpbSurvivalSpatialPoints

survivalKillLag = 2

survivalPeriod = 1
killYear = 2001
site = 5
survivalYears = (killYear - survivalKillLag - survivalPeriod + 1) : (killYear - survivalKillLag)
killStack = stackSitesContorta[[site]] + stackSitesPonderosa[[site]]

df1 = plotKill_Vs_MultiYearMeanSurvival(survivalSPDF, survivalYears, killYear, killStack)


cor(df1$treesKilled, df1$MpbSurvival)

minKill = 1000
lm1 = lm((treesKilled) ~ MpbSurvival, data = subset(df1, df1$treesKilled > minKill)); summary(lm1)
plot(log(treesKilled) ~ MpbSurvival, data = subset(df1, df1$treesKilled > 100), pch = 16, cex = 0.2)
plot(lm1, which = 1, pch = 16)



