# For testing functions ---------------------------------------------------
# {spdf = treeKill_MpbSurvival_StudySites
# spdf = treeKill_MpbSurvival_StudySites
# spd = spd_after
# head(spd_after@data)
# MPBSurvivalYear = 2005
# treeKillYear = 2006
# whichY = "contorta"
# forcode = NA
# col = 1
# pch = 16; cex = 0.1}


# Functions ---------------------------------------------------------------
# retrieve a SPointsDF containing only points corresponding to the

# input tree kill and MPB survival years.
subsetSPDF = function(spdf, MPBSurvivalYear, treeKillYear, 
                      whichY = "sum", forcode = NA, removeZeeroes = T){
  # killed trees aren't observed until the next year
  survivalIndex = which(grepl("survival", names(spdf), ignore.case = T) & grepl(MPBSurvivalYear, names(spdf)))
  killIndex_1 = which(grepl("contorta", names(spdf), ignore.case = T) & grepl(treeKillYear, names(spdf)))
  killIndex_2 = which(grepl("ponderosa", names(spdf), ignore.case = T) & grepl(treeKillYear, names(spdf)))
  
  if (length(survivalIndex) == 0 | length(killIndex_1) == 0 | length(killIndex_2) == 0) {
    print ("columns not found."); break
  }  
  
  y = c()
  if (grepl("contorta", whichY, ignore.case = T)) y = spdf@data[, killIndex_1]
  if (grepl("ponderosa", whichY, ignore.case = T)) y = spdf@data[, killIndex_2]
  if (grepl("sum", whichY, ignore.case = T) | grepl("both", whichY, ignore.case = T) | grepl("both", whichY, ignore.case = T))
    y = spdf@data[, killIndex_1] + spdf@data[, killIndex_2] 
  
  if (length(y) == 0) {print("Must specify contorta, ponderosa, or sum")}
  
  if("FORCODE" %in% names(spdf)) {
    spd = data.frame(MpbSurvival = spdf@data[, survivalIndex], treeKill = y, FORCODE = spdf@data$FORCODE)
  } else spd = data.frame(MpbSurvival = spdf@data[, survivalIndex], treeKill = y, forcode = NA)
  
  coordinates(spd) = spdf@coords
  proj4string(spd) = proj4string(spdf)
  
  if((!is.na(forcode)) & !(is.null(spd$FORCODE))){
    spd = subset(spd, FORCODE == forcode)
  }
  
  if (removeZeeroes) spd = subset(spd, treeKill > 0)
  
  # remove any NaN, NA from the numeric columns
  spd = spd[!(is.nan(spd@data[, 1]) | is.nan(spd@data[, 2])), ]
  spd = spd[!(is.na(spd@data[, 1]) | is.na(spd@data[, 2])), ]
  return(spd)  
}

plotDeltaKillSurvival = 
  function(spdf, MPBSurvivalYear, treeKillYear,
           col = 1, colMap = NA, pch = 16, cex = 0.1, forcode = NA, whichY = "sum", removeZeroes = T) {
    spd_before = subsetSPDF(spdf = spdf, MPBSurvivalYear = MPBSurvivalYear, treeKillYear = treeKillYear - 1, forcode = forcode, whichY = whichY, removeZeeroes = F)  
    spd_after = subsetSPDF(spdf = spdf, MPBSurvivalYear = MPBSurvivalYear, treeKillYear = treeKillYear, forcode = forcode, whichY = whichY, removeZeeroes = F)  
    
    names(spd_after@data)
    spd_after@data[, 2] = spd_after@data[, 2] - spd_before@data[, 2]
    if (removeZeroes)
      spd_after = subset(spd_after, treeKill != 0)
    subtitle = paste0("MPB survival: winter ", (MPBSurvivalYear - 1), " - ", MPBSurvivalYear,
                      "\nChange in red stage observed: summers ", treeKillYear - 1, " - ", treeKillYear)
    
    plotSPDF(spd_after, MPBSurvivalYear, treeKillYear, col, colMap, pch, cex, 
             ylab = "Change in trees killed", subtitle = subtitle)
  }


plotSPDF = function(spd, MPBSurvivalYear, treeKillYear, col = 1,
                    colMap = NA, pch = 16, cex = 0.1, ylab = "Trees killed", subtitle = NA){
  if (is.na(subtitle)) subtitle = paste0("MPB survival: winter ", (MPBSurvivalYear - 1), " - ", MPBSurvivalYear,
                              "\nRed stage observed: summer ", treeKillYear)
  if (is.null(spd$FORCODE)) plotCols = col else
    plotCols = match(spd$FORCODE, colMap[, 1])
  plot(spd$MpbSurvival, spd$treeKill, col = plotCols, pch = pch, cex = cex, xlab = "MPB survival", ylab = ylab, xlim = c(0, 1))
  mtext(subtitle, side = 3)  
}

plotKillSurvival = function(spdf, MPBSurvivalYear, treeKillYear, col = 1, colMap = NA, pch = 16, cex = 0.1, forcode = NA, whichY = "sum"){
  spd = subsetSPDF(spdf = spdf, MPBSurvivalYear = MPBSurvivalYear, treeKillYear = treeKillYear, forcode = forcode, whichY = whichY)  
  plotSPDF(spd, MPBSurvivalYear, treeKillYear, col, colMap, pch, cex)
}

plotLagSeriesKillSurvival = 
  function(spdf, lagMin, lagMax, treeKillyears, 
           plotTitle, plotDir, colMap, cex, filenamePrefix, forcode = NA,
           delta = F, whichY = "sum"){
    
    if (!dir.exists(plotDir)) dir.create(plotDir)
    for(lag in lagMin:lagMax){
      
      prefix = filenamePrefix
      if(lag < 0) prefix = paste0(prefix, "_negative_")
      
      pdf (file = paste0(plotDir, prefix, abs(lag), "_YearLag.pdf"))
      print(paste0("lag = ", lag))
      for(i in (treeKillYears - lag)) {
        
        print(i)
        try({
          
          if (delta == F) plotKillSurvival(spdf, i, i + lag, colMap = colMap, cex = cex, whichY = whichY, forcode = forcode)
          if (delta) plotDeltaKillSurvival(spdf, i, i + lag, colMap = colMap, cex = cex, whichY = whichY, forcode = forcode)
          title(plotTitle, line = 2.8)
        }, silent = T)
      }
      dev.off()
    }
  }
