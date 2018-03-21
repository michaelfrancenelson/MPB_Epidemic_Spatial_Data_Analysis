# https://daymet.ornl.gov/


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
# Daymet: Read all the daymet rasters ---------------------------------------------

files = paste0(daymetFilesDir, list.files(daymetFilesDir))
daymetList = vector("list", length(files))

for(i in 1:length(files)) { 
  print(i); 
  daymetList[[i]] = makeDaymet(files[i]) }
save(daymetList, file = paste0(dataSaveDir, "daymetList.Rd"))

# Daymet: Check that the lat/lon ranges make sense --------------------------------
extents = lapply(daymetList, function(dm) extent(expand.grid(y = dm@latRange, x = dm@lonRange)))
plot(0, xlim = c(-130, -100), ylim = c(25, 60), type = "n") 
lapply(extents, function(x) plot(x, add = T))

# Daymet: create a SpatialPointsDataFrame of the rasters --------------------------
mpbSurvivalSpatialPoints = rasterToPoints(daymetList[[1]]@data, spatial = T)
for (i in 2:length(daymetList)){
  print(i)
  mpbSurvivalSpatialPoints = rbind(mpbSurvivalSpatialPoints, rasterToPoints(daymetList[[i]]@data, spatial = T))
}

save(mpbSurvivalSpatialPoints, file = paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))

# Daymet: Bounding boxes ----------------------------------------------------------

# Figure out a bounding box for all the daymat objects:
rangex = c();rangey = c()
x = c(); y = c()
xInterval = yInterval = vector("numeric", length(daymetList))
counter = 1

for(dm in daymetList)
{ x = c(x, dm@x); y = c(y, dm@y); rx = range(dm@x); ry = range(dm@y)
rangex = range(c(rangex, rx));  rangey = range(c(rangey, ry))
xInterval[counter] = abs(rx[2] - rx[1]) / (length(dm@x) - 1)
yInterval[counter] = abs(ry[2] - ry[1]) / (length(dm@y) - 1)
counter = counter + 1; print(counter) }
xInterval; rangey; yInterval
plot(sort(x))



# Databasin:  Read the lodgepole and ponderosa pine rasters --------------------------- 
(USTreesPerCellDirs = list.dirs(paste0(DatabasinDataPath, "Western_Conterminous_US_Killed_Trees_Per_Grid_Cell_Mountain_Pine_Beetle"), recursive = F))
i = 1; (contortaRasterFiles = list.files(USTreesPerCellDirs[i], "w001001.adf", recursive = T, full.names = T))
i = 2; (ponderosaRasterFiles = list.files(USTreesPerCellDirs[i], "w001001.adf", recursive = T, full.names = T))
contortaRasters = lapply(contortaRasterFiles, raster)
ponderosaRasters = lapply(ponderosaRasterFiles, raster)

# Set the -9999 values to zero

for(i in 1:length(contortaRasters)){
  print(i)
  contortaRasters[[i]][contortaRasters[[i]] == -9999] = 0
  ponderosaRasters[[i]][ponderosaRasters[[i]] == -9999] = 0
}



# Databasin: get all raster cells with any tree kill activity duri --------
load(paste0(dataSaveDir, "daymetList.Rd"))
cellIDList = vector(mode = "list", length = length(contortaRasters))
i = 1
for(i in 1:length(contortaRasters)){
  print(i)
  cellIDList[[i]] =   
    c(which(ponderosaRasters[[i]][] >= 0), which(contortaRasters[[i]][] >= 0))
}

# raster cell IDs corresponding to pixels that had beetle kill in at least one of the study years
cellIDs = unique(unlist(cellIDList))

# Coordinates of the cells
cellXY = xyFromCell(ponderosaRasters[[1]], cellIDs)

treeKillYears = 1997:2010
MpbSurvivalYears = 1981:2017
i = 1
ponderosaRasters[[i]][cellIDs]

# Collect the tree killed data into a data table
treeKillDT = data.table(cellID = cellIDs, matrix(0, nrow = length(cellIDs), ncol = 2 * length(treeKillYears)))
for (i in 1:length(ponderosaRasters)){
  print(i)
  treeKillDT[, i + 1] = ponderosaRasters[[i]][cellIDs]
  treeKillDT[, i + 1 + length(treeKillYears)] = contortaRasters[[i]][cellIDs]
}

# Convert it to a spatial points dataframe
treeKillDT
coordinates(treeKillDT) = cellXY
proj4string(treeKillDT) = proj4string(ponderosaRasters[[1]])



# Combine tree kill and MPB survival data into one object -----------------
# Add the corresponding survival data
treeKill_MpbSurvival_US = getSurvival(treeKillDT, daymetList)
treeKill_MpbSurvival_US = treeKill_MpbSurvival
names(treeKill_MpbSurvival_US) = c("CellID",
              paste0("Ponderosa_Kill_Observed_", treeKillYears),
              paste0("Contorta_Kill_Observed_", treeKillYears),
              paste0("Mpb_survival_winter_ending_", MpbSurvivalYears))
save(treeKill_MpbSurvival_US, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US.Rd"))

# Extract just the study site data points:
treeKill_MpbSurvival_StudySites = treeKill_MpbSurvival_US[!is.na(over(treeKill_MpbSurvival_US, as(studySites, "SpatialPolygons")))]

forestIndices = over(ponderosa_SPDF_StudySites,as(studySites,"SpatialPolygons"))
treeKill_MpbSurvival = cbind(treeKill_MpbSurvival_StudySites, studySites[forestIndices, 3:4])
save(treeKill_MpbSurvival_StudySites, file = paste0(dataSaveDir, "treeKill_MpbSurvival_StudySites.Rd"))
