require(sp)
require(rgeos)
require(rgdal)

# Data Paths --------------------------------------------------------------
dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
studySiteDir = paste0(dataSaveDir, "mpb_study_sites")
statesDir = paste0(dataSaveDir, "tl_2017_us_state")
figureDir = "./figures/"

# Load data ---------------------------------------------------------------
load(paste0(dataSaveDir, "treeKill_MpbSurvival_US.Rd"))
load(paste0(dataSaveDir, "treeKill_MpbSurvival_StudySites.Rd"))
proj = proj4string(treeKill_MpbSurvival_US)
studySites = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj)
states = spTransform(readOGR(statesDir, "tl_2017_us_state"), proj)

# Functions ---------------------------------------------------------------

# retrieve a SPointsDF containing only points corresponding to the
# input tree kill and MPB survival years.


spdf = treeKill_MpbSurvival_StudySites
MPBSurvivalYear = 2005
treeKillYear = 2006
whichY = "contorta"
forcode = NA
col = 1
pch = 16; cex = 0.1
forcode = T
forcodes = c(206, 210, 215, 621, 102)
colMap = cbind(forcodes, 1:5)

subsetSPDF = function(spdf, MPBSurvivalYear, treeKillYear, 
                       whichY = "sum", forcode = NA){
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
  
  if((!is.na(forcode)) & !(is.null(spd$FORCODE)){
    spd = subset(spd, FORCODE == forcode)
  }
  
  spd = subset(spd, treeKill > 0)
  
  # remove any NaN, NA from the numeric columns
  spd = spd[!(is.nan(spd@data[, 1]) | is.nan(spd@data[, 2])), ]
  spd = spd[!(is.na(spd@data[, 1]) | is.na(spd@data[, 2])), ]
  return(spd)  
}

plotKillSurvival = function(spdf, MPBSurvivalYear, treeKillYear, col = 1, colMap = NA, pch = 16, cex = 0.1, forcode = NA, whichY = "sum"){
  spd = subsetSPDF(spdf = spdf, MPBSurvivalYear = MPBSurvivalYear, treeKillYear = treeKillYear, forcode = forcode, whichY = whichY)  

  if (is.null(spd$FORCODE)) plotCols = col else
  # if (is.na(spd$forcode[1]) ) plotCols = col else
      plotCols = match(spd$FORCODE, colMap[, 1])
  plot(spd$MpbSurvival, spd$treeKill, col = plotCols, pch = pch, cex = cex, xlab = "MPB survival", ylab = "Trees killed")
  mtext(paste0("MPB survival: winter ", (MPBSurvivalYear - 1), " - ", MPBSurvivalYear,
               "\nRed stage observed: summer ", treeKillYear), side = 3)  
}


names(treeKill_MpbSurvival_StudySites)

i = 1999
plotKillSurvival(treeKill_MpbSurvival_US, MPBSurvivalYear = i, treeKillYear = i + 1, colMap = colMap)
plotKillSurvival(treeKill_MpbSurvival_StudySites, MPBSurvivalYear = i, treeKillYear = i + 1, 
                 colMap = colMap, whichY = "both", forcode =  T)


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

