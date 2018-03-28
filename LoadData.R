require(sp)
require(rgeos)
require(rgdal)
require(raster)

# Data Paths --------------------------------------------------------------
{
  dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
  figureDir = "./figures/"
}

# Load data ---------------------------------------------------------------
{
  currentDir = getwd()
  setwd(dataSaveDir)
  load("mpbSurvivalTreeKill_WesternUS.Rd")
  load("mpbSurvivalTreeKill_StudySites.Rd")
  load("mpbSurvivalSpatialPoints.Rd")
  load("colMap.Rd")
  load("studySites.Rd")
  load("states.Rd")
  load("studySiteExtents.Rd")
  setwd(currentDir)
}
