require(sp)
require(rgeos)
require(rgdal)


# Data Paths --------------------------------------------------------------
dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
studySiteDir = paste0(dataSaveDir, "mpb_study_sites")
statesDir = paste0(dataSaveDir, "tl_2017_us_state")
figureDir = "./figures/"

# Load data ---------------------------------------------------------------
load(paste0(dataSaveDir, "contorta_SPDF_US.Rd"))
load(paste0(dataSaveDir, "ponderosa_SPDF_US.Rd"))
load(paste0(dataSaveDir, "contorta_SPDF_StudySites.Rd"))
load(paste0(dataSaveDir, "ponderosa_SPDF_StudySites.Rd"))
proj = proj4string(contorta_SPDF_US)
studySites = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj)
states = spTransform(readOGR(statesDir, "tl_2017_us_state"), proj)


# Functions ---------------------------------------------------------------


# retrieve a SPointsDF containing only points corresponding to the
# input tree kill and MPB survival years.
subsetSPDF = function(spdf, MPBSurvivalYear, treeKillYear, 
                      survivalPrefix = "MPB_Survival_Winter_End_", 
                      treeKillPrefix = "TreesKilled_", forcode = NA){
  survivalName = paste0(survivalPrefix, MPBSurvivalYear)
  # killed trees aren't observed until the next year
  killName = paste0(treeKillPrefix, treeKillYear)
  
  survivalIndex = which(names(spdf) == survivalName)
  killIndex = which(names(spdf) == killName)  
  
  
  if("FORCODE" %in% names(spdf)) {
    out = data.frame(MpbSurvival = spdf@data[, survivalIndex], treeKill = spdf@data[, killIndex], FORCODE = spdf@data$FORCODE)
  } else out = data.frame(MpbSurvival = spdf@data[, survivalIndex], treeKill = spdf@data[, killIndex], forcode = NA)
  
  if(!is.na(forcode)){
    out = subset(out, FORCODE == forcode)
  }
  
  coordinates(out) = spdf@coords
  proj4string(out) = proj4string(spdf)
  
  out = subset(out, treeKill > 0)
  out$MpbSurvival = out$MpbSurvival
  return(out)  
}

plotKillSurvival = function(spdf, MPBSurvivalYear, treeKillYear, col = 1, colMap = NA, pch = 16, cex = 0.1, forcode = NA){
  spd = subsetSPDF(spdf = spdf, MPBSurvivalYear = MPBSurvivalYear, treeKillYear = treeKillYear, forcode = forcode)  
  if(is.na(colMap)[1] != TRUE & is.na(forcode)){
    cols = match(spd$FORCODE, colMap[, 1])
  } else cols = col
  plot(spd$MpbSurvival, spd$treeKill, col = cols, pch = pch, cex = cex, xlab = "MPB survival", ylab = "Trees killed")
  mtext(paste0("MPB survival: winter ", (MPBSurvivalYear - 1), " - ", MPBSurvivalYear,
               "\nRed stage observed: summer ", treeKillYear), side = 3)  
}


plotDeltaKillSurvival


i = 2005
plotKillSurvival(ponderosa_SPDF_StudySites, MPBSurvivalYear = i, treeKillYear = i + 1, colMap = colMap)
plotKillSurvival(contorta_SPDF_StudySites, MPBSurvivalYear = i, treeKillYear = i + 1, colMap = colMap)
dev.off()

i = 1999
plotKillSurvival(contorta_SPDF_US, i, i + 1)


pch = 16; cex = 0.1
forcode = NA
forcodes = c(206, 210, 215, 621, 102)
colMap = cbind(forcodes, 1:5)
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





# Plot color-coded study sites --------------------------------------------
x = factor(studySites$FOREST)
pdf(file = paste0(figureDir, "study_sites.pdf"))
plot(studySites, col = as.numeric(x), border = NA); plot(states, add = T, border = gray(0.5))
dev.off()
# What are the codes for the various forests?
unique(data.frame(survivalValsStudySites@data$FORCODE, survivalValsStudySites@data$FOREST))
plot(subset(survivalValsStudySites, FORCODE == 203), add = T)






# Plot survival vs. change in tree kill -----------------------------------
survivalValsStudySites$cols = as.numeric(factor(survivalValsStudySites$FOREST))
plotLag = function(spdf, treeKillYear, MPBsurvivalYear, forcode = NA, pow = NA, log = F)
{
  dat = subset(spdf, MPB_kill_endYear == treeKillYear)
  if(! is.na(forcode))
    dat = subset(dat, FORCODE == forcode)
  index = which(grepl(MPBsurvivalYear, names(spdf)))[1]
  x = dat@data[, index] 
  y = dat$deltaKilledTrees 
  if(!is.na(pow)) y = sign(y) * (abs(y) ^ pow)
  if(log == T) y = sign(y) * log(abs(y))
  
  plot(x, y, col = dat$cols, pch = 16, cex = 0.1, xlim = c(0, 1),
       ylab = "Increase in killed trees", xlab = "overwinter survival")
  mtext(paste0("survival: winter ", MPBsurvivalYear - 1, "/", MPBsurvivalYear, "\n",
               "tree kill: summer ", treeKillYear))
}

unique(survivalValsStudySites@data$MPB_kill_endYear)

i = 2003; plotLag(survivalValsStudySites, treeKillYear = i, MPBsurvivalYear = i, forcode = 621)
i = 2005; plotLag(survivalValsStudySites, treeKillYear = i, MPBsurvivalYear = i)
i = 2006; plotLag(survivalValsStudySites, treeKillYear = i, MPBsurvivalYear = i - 1)


# Plot numbers of trees killed vs survival --------------------------------
plotLag2 = function(spdf, treeKillYear, MPBsurvivalYear)
{
  
}

