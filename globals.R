killYears = 1997:2010
survivalYears = 1981:2016
lonLatProj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
daymetFilesDir = "E:/Data/Daymet/daymet_output/"
DatabasinDataPath = "E:/Data/Databasin/"
studySiteDir = paste0(dataSaveDir, "mpb_study_sites")
statesDir = paste0(dataSaveDir, "tl_2017_us_state")
canadaDir = paste0(dataSaveDir, "canada/lpr_000a16a_e")
siteNames = c("Beaverhead", "BlackHills", "Colorado", "Colville", "WesternUS", "WesternUS_unmasked")
studyForestNames = c("Beaverhead", "Black_Hills", "Colorado", "Colville")
treeMapDir = "E:/Data/species_range_maps"
proj4stringDaymet = "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"
proj4stringDatabasin = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4stringLonLat = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4stringMaster = proj4stringDaymet
rangeBufferWidth = 10 * 1000
