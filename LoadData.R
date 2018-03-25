require(sp)
require(rgeos)
require(rgdal)
require(raster)

# Data Paths --------------------------------------------------------------
{
  dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
  studySiteDir = paste0(dataSaveDir, "mpb_study_sites")
  statesDir = paste0(dataSaveDir, "tl_2017_us_state")
  figureDir = "./figures/"
}

# Load data ---------------------------------------------------------------
{
  source("plottingFunctions.R")
  load(paste0(dataSaveDir, "treeKill_MpbSurvival_US.Rd"))
  load(paste0(dataSaveDir, "treeKill_MpbSurvival_StudySites.Rd"))
  load(paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))
  load(paste0(dataSaveDir, "colMaps.Rd"))
  load(paste0(dataSaveDir, "studySites.Rd"))
  load(paste0(dataSaveDir, "states.Rd"))
}
