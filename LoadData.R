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
  load("mpbSurvivalRastersCropped.Rd")
  load("studySitesSpatial.Rd")
  load("treeKillSpatial.Rd")
  load("yearlyAggregateData.Rd")
  setwd(currentDir)
}

