{ require(sp)
  require(rgeos)
  require(rgdal)
  require(raster)}

# Data Paths --------------------------------------------------------------
{ print("setting data locations...")
  dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
figureDir = "./figures/"}

# Load small data ---------------------------------------------------------------
{
  currentDir = getwd()
  setwd(dataSaveDir)
  print("loading small data sets...")
  load("colMap.Rd")
  # load("contortaRasters.Rd")
  # load("daymetList.Rd")
  load("extentSites.Rd")
  # load("mpbSurvivalSitesMultiYearAvgAggregate.Rd")
  # load("mpbSurvivalSpatialPoints.Rd")
  # load("mpbSurvivalStacks.Rd")
  load("mpbSurvivalTreeKill_WesternUS.Rd")
  load("mpbSurvivalTreeKill_StudySites.Rd")
  # load("ponderosaRasters.Rd")
  load("shapeSites.Rd")
  load("siteMeanKills.Rd")
  load("siteMeanMPBSurvival.Rd")
  # load("stackSites.Rd")
  load("states.Rd")
  load("studySites.Rd")
  load("treeKill_multYearMeanMpbSurvival.Rd")
  setwd(currentDir)
}


# Load all data ---------------------------------------------------------
{ 
  currentDir = getwd()
  setwd(dataSaveDir)
  load("colMap.Rd")
  load("contortaRasters.Rd")
  load("daymetList.Rd")
  load("extentSites.Rd")
  load("mpbSurvivalSitesMultiYearAvgAggregate.Rd")
  load("mpbSurvivalSpatialPoints.Rd")
  load("mpbSurvivalSpatialPointsMasked.Rd")
  load("mpbSurvivalStacks.Rd")
  load("mpbSurvivalTreeKill_WesternUS.Rd")
  load("mpbSurvivalTreeKill_StudySites.Rd")
  load("ponderosaRasters.Rd")
  load("shapeSites.Rd")
  load("siteMeanKills.Rd")
  load("siteMeanMPBSurvival.Rd")
  load("stackSites.Rd")
  load("states.Rd")
  load("studySites.Rd")
  load("treeKill_multYearMeanMpbSurvival.Rd")
  setwd(currentDir)
  }

