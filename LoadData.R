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
  source("plottingFunctions.R")
  load(paste0(dataSaveDir, "treeKill_MpbSurvival_US.Rd"))
  load(paste0(dataSaveDir, "treeKill_MpbSurvival_StudySites.Rd"))
  load(paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))
  load(paste0(dataSaveDir, "colMaps.Rd"))
  load(paste0(dataSaveDir, "studySites.Rd"))
  load(paste0(dataSaveDir, "states.Rd"))
}




# Set up data paths -------------------------------------------------------
{
  require(data.table)
  require(rgdal)
  require(sp)
  source("dataPrep/daymet_class.R")
  lonLatProj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
  daymetFilesDir = "E:/Data/Daymet/daymet_output/"
  DatabasinDataPath = "E:/Data/Databasin/"
}

treeKillYears = 1997:2010
MpbSurvivalYears = 1981:2017

load(paste0(dataSaveDir, "treeKillDT.Rd"))
load(paste0(dataSaveDir, "daymetList.Rd"))

nrow(treeKillDT)

breakPoints = c(1e6, 2e6, 3e6, nrow(treeKillDT))

treeKill_MpbSurvival_US_1 = getSurvival(treeKillDT[1:breakPoints[1], ], daymetList = daymetList)
save(treeKill_MpbSurvival_US_1, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US_1.Rd"))

treeKill_MpbSurvival_US_2 = getSurvival(treeKillDT[breakPoints[1]:breakPoints[2], ], daymetList = daymetList)
save(treeKill_MpbSurvival_US_2, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US_2.Rd"))

treeKill_MpbSurvival_US_3 = getSurvival(treeKillDT[breakPoints[2]:breakPoints[3], ], daymetList = daymetList)
save(treeKill_MpbSurvival_US_3, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US_3.Rd"))

treeKill_MpbSurvival_US_4 = getSurvival(treeKillDT[breakPoints[3]:breakPoints[4], ], daymetList = daymetList)
save(treeKill_MpbSurvival_US_4, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US_4.Rd"))



treeKill_MpbSurvival_US = getSurvival(treeKillDT, daymetList)
names(treeKill_MpbSurvival_US) = c("CellID",
                                   paste0("Ponderosa_Kill_Observed_", treeKillYears),
                                   paste0("Contorta_Kill_Observed_", treeKillYears),
                                   paste0("Mpb_survival_winter_ending_", MpbSurvivalYears))
save(treeKill_MpbSurvival_US, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US.Rd"))


