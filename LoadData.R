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
  load("colMap.Rd")
  load("contortaRasters.Rd")
  load("daymetList.Rd")
  load("extentSites.Rd")
  load("mpbSurvivalSitesMultiYearAvgAggregate.rD")
  load("mpbSurvivalSpatialPoints.Rd")
  load("mpbSurvivalStacks.Rd")
  load("mpbSurvivalTreeKill_WesternUS.Rd")
  load("mpbSurvivalTreeKill_StudySites.Rd")
  load("ponderosaRasters.Rd")
  load("shapeSites.Rd")
  load("siteMeanKills.Rd")
  load("stackSites.Rd")
  load("states.Rd")
  load("studySites.Rd")
  
  setwd(currentDir)
}
