require(sp)
require(rgeos)
require(rgdal)

# Data Paths --------------------------------------------------------------
{dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
studySiteDir = paste0(dataSaveDir, "mpb_study_sites")
statesDir = paste0(dataSaveDir, "tl_2017_us_state")
figureDir = "./figures/"}

# Load data ---------------------------------------------------------------
{source("plottingFunctions.R")
  load(paste0(dataSaveDir, "treeKill_MpbSurvival_US.Rd"))
  load(paste0(dataSaveDir, "treeKill_MpbSurvival_StudySites.Rd"))
  proj = proj4string(treeKill_MpbSurvival_US)
  studySites = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj)
  states = spTransform(readOGR(statesDir, "tl_2017_us_state"), proj)}

# Color-coding of the national forests ------------------------------------
{forcodes = c(203, 206, 210, 215, 621, 102)
fornames = treeKill_MpbSurvival_StudySites$FOREST[match(forcodes, treeKill_MpbSurvival_StudySites$FORCODE)]
unique(treeKill_MpbSurvival_StudySites$FORCODE)
colMap = data.frame(forcodes, col = 1:6, fornames)
colMap$fornames = gsub(" National Forest", "", gsub(" National Forests", "", colMap$fornames))}


# Plot color-coded study sites --------------------------------------------
x = factor(studySites$FOREST)
pdf(file = paste0(figureDir, "study_sites.pdf"))
plot(studySites, col = match(studySites$FORCODE, colMap$forcodes), border = NA); plot(states, add = T, border = gray(0.5))
legend("bottomleft", legend = colMap$fornames, col = colMap$col, pch = 16, bg = "white")
dev.off()

# What are the codes for the various forests?
unique(data.frame(survivalValsStudySites@data$FORCODE, survivalValsStudySites@data$FOREST))
plot(subset(survivalValsStudySites, FORCODE == 203), add = T)



plotLagSeriesKillSurvival = 
  function(spdf, lagMin, lagMax, treeKillyears, 
           plotTitle, plotDir, colMap, cex, filenamePrefix){
    
    if (!dir.exists(plotDir)) dir.create(plotDir)
    for(lag in lagMin:lagMax){
      
      prefix = filenamePrefix
      if(lag < 0) prefix = paste0(prefix, "_negative_")
      
      pdf (file = paste0(plotDir, prefix, abs(lag), "_YearLag.pdf"))
      print(paste0("lag = ", lag))
      for(i in (treeKillYears - lag)) {
        print(i)
        plotKillSurvival(spdf, i, i + lag, colMap = colMap, cex = cex)
        title(plotTitle, line = 2.8)
      }
      dev.off()
    }
  
}


# Plots of survival vs killed trees for Western US ---------------------------------------
cex = 0.3
plotTitle = "Western US"
plotDir = paste0(figureDir, "Western_US/")
treeKillYears = 1998:2010
lagMin = -5; lagMax = -1
filenamePrefix = "Western_US_Survival_Kill_"
plotLagSeriesKillSurvival(treeKill_MpbSurvival_US, lagMin, lagMax, treeKillyears, plotTitle, plotDir,colMap, cex, filenamePrefix)


# Plots of survival vs killed trees for combined study sites US ---------------------------------------
cex = 0.3
plotTitle = "Study Sites Combined"
plotDir = paste0(figureDir, "Study_Sites_Combined/")
treeKillYears = 1998:2010
lagMin = -4; lagMax = 4
filenamePrefix = "StudySitesCombined_Survival_Kill_"
plotLagSeriesKillSurvival(treeKill_MpbSurvival_StudySites, lagMin, lagMax, treeKillyears, plotTitle, plotDir,colMap, cex, filenamePrefix)


# Plots of survival for individual forests --------------------------------

row = 1
for(row in 1:nrow(colMap)){
  forestName = gsub(" ", "_", colMap$fornames[row], perl = T)
  forestDir = paste0(figureDir, forestName, "/")
  if (!dir.exists(forestDir)) dir.create(forestDir)
  
  forcode = colMap$forcodes[row]
  
  plotDeltaKillSurvival(treeKill_MpbSurvival_StudySites, i, i + lag, 
                        colMap = colMap, whichY = "both", forcode = forcode)
  title(plotTitle, line = 2.8)
  
  
  }









plotKillSurvival(treeKill_MpbSurvival_StudySites, i, i + 1, 
                 colMap = colMap, whichY = "both", forcode = NA)
plotDeltaKillSurvival(treeKill_MpbSurvival_StudySites, i, i + 1, 
                      colMap = colMap, whichY = "both", forcode = 210)
plotDeltaKillSurvival(treeKill_MpbSurvival_US, i, i + 1)
colMap


dev.off()

i = 1999
plotKillSurvival(contorta_SPDF_US, i, i + 1)



levels(sp$forcode)[as.numeric(unique(sp$forcode))]



i = 2002
sp = subsetSPDF(contorta_SPDF_StudySites, MPBSurvivalYear = i, treeKillYear = i)
plot(sp$MpbSurvival, sp$treeKill, pch = 16, cex = 0.1)


# load(paste0(dataSaveDir, "studySites_DeltaKilledPerCell.Rd"))
# load(paste0(dataSaveDir, "daymetList.Rd"))
# load(paste0(dataSaveDir, "survivalValsStudySites.Rd"))
# load(paste0(dataSaveDir, "westernUS_KilledPerCell.Rd"))
# proj = proj4string(daymetList[[1]]@data)
# head(studySites@data)
# subset(studySites, AREA > 0)
# unique(studySites@data$FOREST)
# class(survivalValsStudySites)