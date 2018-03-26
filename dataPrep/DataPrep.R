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

leadingColumnNames = c("cellID", "x", "y", "lon", "lat", "daymetTile")

# Collect the tree killed data into a data frame
treeKillSPDF = data.frame(cellID = cellIDs,x = cellXY$x, y = cellXY$y, lon = cellLonLat$x, lat = cellLonLat$y, matrix(0, nrow = length(cellIDs), ncol = 1 + 2 * length(treeKillYears)))
treeKillSPDF = cbind(treeKillSPDF, matrix(0, nrow = length(cellIDs), ncol = length(MpbSurvivalYears)))
names(treeKillSPDF) = 
  c(
    leadingColumnNames, 
    paste0("Ponderosa_Kill_Observed_", treeKillYears),paste0("Contorta_Kill_Observed_", treeKillYears), 
    paste0("Mpb_survival_winter_ending_", MpbSurvivalYears))

# Convert it to a spatial points dataframe
coordinates(treeKillSPDF) = cellXY@coords
proj4string(treeKillSPDF) = proj4string(ponderosaRasters[[1]])
summary(treeKillSPDF)

# Add the tree kill values
for (i in 1:length(ponderosaRasters)){
  pIndex = i + length(leadingColumnNames)
  cIndex = pIndex + length(treeKillYears)
  print(names(treeKillSPDF)[pIndex])
  names(treeKillSPDF)[cIndex]
  treeKillSPDF@data[, pIndex] = ponderosaRasters[[i]][cellIDs]
  treeKillSPDF@data[, cIndex] = contortaRasters[[i]][cellIDs]
}

treeKillSPDF_orig = treeKillSPDF

# Get the closest points with survival values
treeKillSPDF = spTransform(treeKillSPDF, proj4string(mpbSurvivalSpatialPoints))
ddd = knn(coordinates(mpbSurvivalSpatialPoints), coordinates(treeKillSPDF), k = 1)
treeKillSPDF@data[, survivalIndices] = mpbSurvivalSpatialPoints@data[ddd$nn.idx, ]
save(treeKillSPDF, file = paste0(dataSaveDir, "treeKillSPDF.Rd"))



plot(treeKillSPDF, pch = 16, cex = 0.1)

plot(subset(treeKillSPDF, is.na(Mpb_survival_winter_ending_1982)), add = T, cex = 0.1, col = 2)




summary(treeKillSPDF)
treeKillSPDF

load(paste0(dataSaveDir, "treeKillSPDF.Rd"))
load(paste0(dataSaveDir, "daymetList.Rd"))

save(treeKill_MpbSurvival_US_5, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US_5.Rd"))


treeKill_MpbSurvival_US = rbind(treeKill_MpbSurvival_US_1, treeKill_MpbSurvival_US_2, treeKill_MpbSurvival_US_3, treeKill_MpbSurvival_US_4)
names(treeKill_MpbSurvival_US) = c("CellID",
                                   paste0("Ponderosa_Kill_Observed_", treeKillYears),
                                   paste0("Contorta_Kill_Observed_", treeKillYears),
                                   paste0("Mpb_survival_winter_ending_", MpbSurvivalYears))
save(treeKill_MpbSurvival_US, file = paste0(dataSaveDir, "treeKill_MpbSurvival_US.Rd"))



# Extract data for study sites and set up colors/plotting names --------------------------------------------

proj = proj4string(treeKill_MpbSurvival_US)
studySites = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj)

treeKill_MpbSurvival_StudySites = 
  treeKill_MpbSurvival_US[
    !is.na(over(treeKill_MpbSurvival_US, as(studySites, "SpatialPolygons"))), ]
forestIndices = over(treeKill_MpbSurvival_StudySites, as(studySites, "SpatialPolygons"))
treeKill_MpbSurvival_StudySites = cbind(treeKill_MpbSurvival_StudySites, studySites[forestIndices, 3:4])

forcodes = levels(treeKill_MpbSurvival_StudySites$FORCODE)
fornames = treeKill_MpbSurvival_StudySites$FOREST[match(forcodes, treeKill_MpbSurvival_StudySites$FORCODE)]
colMap = data.frame(forcode = forcodes, col = 1:6, forname = fornames)

# Combine araphao, white river, and medicine bow into the same study site:
oldCodes = list(forcodes[1], forcodes[2:4], forcodes[5], forcodes[6])
newNames = as.character(c("Black Hills", "Colorado", "Colville", "Beaverhead"))
newCodes = 1:4
colMap2 = data.frame(forcode = newCodes, forname = newNames, col = 1:4)

StudySiteCode = vector(mode = "numeric", length = nrow(treeKill_MpbSurvival_StudySites))
StudySiteNames = vector(mode = "character", length = length(StudySiteCode))

for(i in 1:4){
  StudySiteCode[treeKill_MpbSurvival_StudySites$FORCODE %in% oldCodes[[i]] ] = colMap2$forcode[i]
  StudySiteNames[StudySiteCode == colMap2$forcode[i]] = as.character(colMap2$forname[i])}
StudySiteNames[match(colMap2$forcode, StudySiteCode)]
treeKill_MpbSurvival_StudySites@data = data.frame(treeKill_MpbSurvival_StudySites, StudySiteCode = StudySiteCode, StudySiteName = StudySiteNames)

save(colMap, colMap2, file = paste0(dataSaveDir, "colMaps.Rd"))
save(treeKill_MpbSurvival_StudySites, file = paste0(dataSaveDir, "treeKill_MpbSurvival_StudySites.Rd"))


# Setup Study Sites vector data -------------------------------------------------------
proj = proj4string(treeKill_MpbSurvival_US)
proj = proj4string(daymetList[[1]]@data)
studySites = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj)

StudySiteCode = vector(mode = "numeric", length = nrow(studySites))
StudySiteNames = vector(mode = "character", length = length(StudySiteCode))
for(i in 1:4){
  StudySiteCode[studySites$FORCODE %in% oldCodes[[i]] ] = colMap2$forcode[i]
  StudySiteNames[StudySiteCode == colMap2$forcode[i]] = as.character(colMap2$forname[i])}
StudySiteNames[match(colMap2$forcode, StudySiteCode)]
studySites@data = data.frame(studySites@data[, c("FOREST", "FORCODE")], StudySiteName = StudySiteNames, StudySiteCode = StudySiteCode)
head(studySites@data)
save(studySites, file = paste0(dataSaveDir, file = "studySites.Rd"))


# Setup US States vector data ---------------------------------------------

states = spTransform(readOGR(statesDir, "tl_2017_us_state"), proj)
head(states@data)
states@data = states@data[, c("STUSPS", 'NAME')]
save(states, file = paste0(dataSaveDir, "States.Rd"))

