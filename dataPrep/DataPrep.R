# https://daymet.ornl.gov/

# Set up data paths -------------------------------------------------------
{
  require(data.table)
  require(rgdal)
  require(sp)
  require(nabor)
  source("dataPrep/daymet_class.R")
  lonLatProj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  dataSaveDir = "E:/Data/MPB_Overwinter_Data/"
  daymetFilesDir = "E:/Data/Daymet/daymet_output/"
  DatabasinDataPath = "E:/Data/Databasin/"
  studySiteDir = paste0(dataSaveDir, "mpb_study_sites")
  statesDir = paste0(dataSaveDir, "tl_2017_us_state")
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

save(contortaRasters, file = paste0(dataSaveDir, "contortaRasters.Rd"))
save(ponderosaRasters, file = paste0(dataSaveDir, "ponderosaRasters.Rd"))

load(file = paste0(dataSaveDir, "contortaRasters.Rd"))
load(file = paste0(dataSaveDir, "ponderosaRasters.Rd"))

# Databasin: get all raster cells with any tree kill activity duri --------
load(paste0(dataSaveDir, "daymetList.Rd"))
cellIDList = vector(mode = "list", length = length(contortaRasters))
for(i in 1:length(contortaRasters)){
  print(i)
  cellIDList[[i]] =   
    c(which(ponderosaRasters[[i]][] > 0), which(contortaRasters[[i]][] > 0))
}

# raster cell IDs corresponding to pixels that had beetle kill in at least one of the study years
cellIDs = unique(unlist(cellIDList))

# Coordinates of the cells
cellXY = data.frame(xyFromCell(ponderosaRasters[[1]], cellIDs))
head(cellXY)

coordinates(cellXY) = ~ x + y
proj4string(cellXY) = proj4string(ponderosaRasters[[1]])

cellLonLat = spTransform(cellXY, lonLatProj)

treeKillYears = 1997:2010
MpbSurvivalYears = 1981:2017

leadingColumnNames = c("cellID")
# Collect the tree killed data into a data frame
mpbSurvivalTreeKill_WesternUs = data.frame(cellID = cellIDs, matrix(0, nrow = length(cellIDs), ncol = 2 * length(treeKillYears)))
mpbSurvivalTreeKill_WesternUs = cbind(mpbSurvivalTreeKill_WesternUs, matrix(0, nrow = length(cellIDs), ncol = length(MpbSurvivalYears)))
names(mpbSurvivalTreeKill_WesternUs) = 
  c(
    leadingColumnNames, 
    paste0("Ponderosa_Kill_Observed_", treeKillYears),paste0("Contorta_Kill_Observed_", treeKillYears), 
    paste0("Mpb_survival_winter_ending_", MpbSurvivalYears))

# Convert it to a spatial points dataframe
coordinates(mpbSurvivalTreeKill_WesternUs) = cellXY@coords
proj4string(mpbSurvivalTreeKill_WesternUs) = proj4string(ponderosaRasters[[1]])
summary(mpbSurvivalTreeKill_WesternUs)

# Add the tree kill values
for (i in 1:length(ponderosaRasters)){
  pIndex = i + length(leadingColumnNames)
  cIndex = pIndex + length(treeKillYears)
  print(names(mpbSurvivalTreeKill_WesternUs)[pIndex])
  names(mpbSurvivalTreeKill_WesternUs)[cIndex]
  mpbSurvivalTreeKill_WesternUs@data[, pIndex] = ponderosaRasters[[i]][cellIDs]
  mpbSurvivalTreeKill_WesternUs@data[, cIndex] = contortaRasters[[i]][cellIDs]
}
names(mpbSurvivalTreeKill_WesternUs)
# Get the closest points with survival values
survivalIndices = (ncol(mpbSurvivalTreeKill_WesternUs) - (length(MpbSurvivalYears) - 1)):ncol(mpbSurvivalTreeKill_WesternUs)
names(mpbSurvivalTreeKill_WesternUs)[survivalIndices]
# load(paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))
mpbSurvivalTreeKill_WesternUs = spTransform(mpbSurvivalTreeKill_WesternUs, proj4string(mpbSurvivalSpatialPoints))
ddd = knn(coordinates(mpbSurvivalSpatialPoints), coordinates(mpbSurvivalTreeKill_WesternUs), k = 1)
mpbSurvivalTreeKill_WesternUs@data[, survivalIndices] = mpbSurvivalSpatialPoints@data[ddd$nn.idx, ]
save(mpbSurvivalTreeKill_WesternUs, file = paste0(dataSaveDir, "mpbSurvivalTreeKill_WesternUs.Rd"))

plot(mpbSurvivalTreeKill_WesternUs, pch = 16, cex = 0.1)



# Setup Study Sites vector data -------------------------------------------------------
proj = proj4string(mpbSurvivalTreeKill_WesternUs)
studySites = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj)

# Keep just the for code and name'
studySites@data = studySites@data[, c("FOREST", "FORCODE")]

levels(studySites$FOREST)

# Combine araphao, white river, and medicine bow into the same study site:
oldNames = list(
  "Arapaho and Roosevelt National Forests", 
  "Beaverhead-Deerlodge National Forest", 
  "Black Hills National Forest", 
  "Colville National Forest",
  "Medicine Bow National Forest", 
  "White River National Forest")
newNames = list(
  "Colorado",
  "Colorado",
  "Colorado",
  "Beaverhead", 
  "Black Hills", 
  "Colville")
newCodes = list(3, 3, 3, 2, 1, 4)

studySiteCode = vector(mode = "numeric", length = nrow(studySites))
studySiteName = vector(mode = "character", length = length(studySiteCode))

replacements = data.frame(oldNames = c("Black", "Beaverhead", "Arapaho", "Medicine", "White", "Colville"),
                          newNames = c("Black Hills", "Beaverhead", "Colorado", "Colorado", "Colorado", "Colville"),
                          newCodes = c(1, 2, 3, 3, 3, 4))
for(i in 1:length(oldNames)){
  studySiteName[grepl(replacements$oldNames[i], studySites$FOREST)] = as.character(replacements$newNames[i])
  studySiteCode[grepl(replacements$oldNames[i], studySites$FOREST)] = as.character(replacements$newCodes[i])
  
}

studySites@data = cbind(studySites@data, studySiteName, studySiteCode)

colMap = unique(studySites@data[, 3:4])
colMap$col = c(3, 2, 1, 4)
plot(studySites, col = colMap$col[match(studySites$studySiteName, colMap$studySiteName)], border = NA)

legend("bottomleft", legend = colMap$studySiteName, col = colMap$col, pch = 15, bg = "white")

save(studySites, file = paste0(dataSaveDir, "studySites.Rd"))
save(colMap, file = paste0(dataSaveDir, "colMap.Rd"))



# Extract data for study sites and set up colors/plotting names --------------------------------------------

mpbSurvivalTreeKill_StudySites = mpbSurvivalTreeKill_WesternUs[
  !is.na(over(mpbSurvivalTreeKill_WesternUs, as(studySites, "SpatialPolygons"))), ]

summary(mpbSurvivalTreeKill_StudySites)
plot(mpbSurvivalTreeKill_StudySites)
names(studySites)
forestIndices = over(mpbSurvivalTreeKill_StudySites, as(studySites, "SpatialPolygons"))
mpbSurvivalTreeKill_StudySites = cbind(mpbSurvivalTreeKill_StudySites, studySites[forestIndices, c("FOREST", "FORCODE", "studySiteName", "studySiteCode")])

save(mpbSurvivalTreeKill_StudySites, file = paste0(dataSaveDir, "mpbSurvivalTreeKill_StudySites.Rd"))

# Setup US States vector data ---------------------------------------------

states = spTransform(readOGR(statesDir, "tl_2017_us_state"), proj)
head(states@data)
states@data = states@data[, c("STUSPS", 'NAME')]
save(states, file = paste0(dataSaveDir, "States.Rd"))


# Bounding boxes for study sites ------------------------------------------
locator(2)
plot(studySites, col = colMap$col[match(studySites$studySiteName, colMap$studySiteName)], border = NA)
extentColville = extent(x = c(-1371116, -1146951), y = c(683243, 868466)); plot(extentColville, add = T)
extentColorado = extent(x = c(-748860, -325889), y = c(-392153, 83343)); plot(extentColorado, add = T)
extentBlackHills = extent(x = c(-406060, -195957), y = c(72286, 287917)); plot(extentBlackHills, add = T)
extentBeaverhead = extent(x = c(-1064015, -820737), y = c(235392, 539489)); plot(extentBeaverhead, add = T)
save(extentColorado, extentColville, extentBlackHills, extentBeaverhead, file = paste0(dataSaveDir, "studySiteExtents.Rd"))


# Raster stacks of survival for the study sites ---------------------------
lengthXY = function(ext, unit = 1000){
  lx = abs(ext@xmax - ext@xmin)
  ly = abs(ext@ymax - ext@ymin)
  return(round(data.frame(nRows = ly, nColumns = lx) / unit))
}

getSPDFfromExt = function(ext, spdf){
  bPolygon = as(ext, 'SpatialPolygons'); proj4string(bPolygon) = proj4string(spdf)
  return(spdf[rowIndices, ])
}

getStackFromExt = function(ext, spdf){
  bPolygon = as(ext, 'SpatialPolygons'); proj4string(bPolygon) = proj4string(spdf)
  lCol = lengthXY(ext)  
  spd = spdf[which(!is.na(over(spdf, bPolygon))), ]
  templateR = raster(ext, nrow = lCol$nRows, ncol = lCol$nColumns, crs = proj4string(spd))
  nn = nabor::knn(spd@coords, coordinates(templateR), k = 1)
  st = stack(x = templateR, template = templateR, nl = ncol(spd))  
  
  for(i in 1:ncol(spd)){
    print(i)
    vals = spd@data[nn$nn.idx, i]
    rst = raster(ext, nrow = lCol$nRows, ncol = lCol$nColumns, crs = proj4string(spd))
    values(rst) = vals
    st = addLayer(st, rst)
  }
  names(st) = names(spd)
  return(st)
}
MpbSurvivalStackColville = getStackFromExt(extentColville, mpbSurvivalSpatialPoints)
MpbSurvivalStackColorado = getStackFromExt(extentColorado, mpbSurvivalSpatialPoints)
MpbSurvivalStackBeaverhead = getStackFromExt(extentBeaverhead, mpbSurvivalSpatialPoints)
MpbSurvivalStackBlackHills = getStackFromExt(extentBlackHills, mpbSurvivalSpatialPoints)

save(MpbSurvivalStackBeaverhead, MpbSurvivalStackColorado, MpbSurvivalStackColville, MpbSurvivalStackBlackHills, file = paste0(dataSaveDir, "MpbSurvivalRasterStacis.Rd"))

