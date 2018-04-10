
# Data sources ------------------------------------------------------------
# https://daymet.ornl.gov/



# Data needs --------------------------------------------------------------
#
#
# What data do we need for plots and analysis?
# 
# Basic spatial data:
#
#   Bounding boxes for the study sites.
#   Polygons of the study sites
#   Polygons of range limits for pines
#   Raster stacks of tree kill for each study site
#   Raster stacks of MPB survival for each study site
#
# Derived data
#
# Aggregate data:
#   mean yearly mpb survival for study sites
#   mean multi-year average survival for study sites


# Set up data paths -------------------------------------------------------
{
  require(data.table)
  require(rgdal)
  require(sp)
  require(rgeos)
  require(nabor)
  require(plyr)
  source("dataPrep/daymet_class.R")
  source("globals.R")
  source("helperFunctions.R")
}

# Study site polygons ---------------------------------------------
{ 
  usStates = spTransform(readOGR(statesDir, "tl_2017_us_state"), proj4stringMaster)
  canada = spTransform(readOGR(canadaDir, "lpr_000a16a_e"), proj4stringMaster)
  
  # We don't need all the columns in the data frames:
  usStates@data = usStates@data[, c("STUSPS", 'NAME')]
  canada@data = canada@data[, c("PREABBR", "PRENAME")]
  names(usStates) = names(canada) = c("abbr", "name")
  
  # Define the extents we want for the study sites
  studySiteExtents = list(
    Beaverhead = extent(x = c(-1064015, -820737), y = c(235392, 539489)),
    BlackHills = extent(x = c(-406060, -195957), y = c(72286, 287917)),
    Colorado = extent(x = c(-748860, -325889), y = c(-392153, 83343)),
    Colville = extent(x = c(-1371116, -1146951), y = c(683243, 868466)),
    WesternUS = extent(x = c(-2001120, 100000), y = c(-6e5, 951135)),
    WesternUS_unmasked = extent(x = c(-2001120, 100000), y = c(-6e5, 951135)))
  
  # Read in the national forest spatial data, and keep only the forests of interest:
  nationalForestsSPDF = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj4stringMaster)
  
  # Keep only the names and codes:
  nationalForestsSPDF@data = nationalForestsSPDF@data[, c("FOREST", "FORCODE")]
  
  # Combine araphao, white river, and medicine bow into the same study site,
  # and simplify the other study forest names:
  forestNames = data.frame(
    oldName = c("Arapaho and Roosevelt National Forests", 
                "Medicine Bow National Forest", 
                "White River National Forest",
                "Beaverhead-Deerlodge National Forest", 
                "Black Hills National Forest", 
                "Colville National Forest"),
    newName = c("Colorado", "Colorado", "Colorado", "Beaverhead", 
                "Black Hills", "Colville"), stringsAsFactors = F)
  
  # Keep only the forests of interest and map names to new names:  
  studyForestsSPDF = subset(nationalForestsSPDF, FOREST %in% forestNames$oldName)
  studyForestsSPDF$FOREST = mapvalues(studyForestsSPDF$FOREST, from = forestNames$oldName, to = forestNames$newName)
  
  # For the western US study sites we need the pine ranges
  pineRanges = list(
    ponderosa = spTransform(readOGR(dsn = paste0(treeMapDir, "/Pinus_ponderosa/data/commondata/data0"), "pinupond"), proj4stringMaster),
    contorta = spTransform(readOGR(dsn = paste0(treeMapDir, "/Pinus_contorta/data/commondata/data0"), "pinucont"), proj4stringMaster)
  )
  pineRanges$union = gUnion(pineRanges$ponderosa, pineRanges$contorta)
  pineRanges$unionBuffered = gBuffer(pineRanges$union, width = rangeBufferWidth)
  
  studySitePolygons = list(
    Beaverhead = subset(studyForestsSPDF, FOREST == "Beaverhead"),
    Black_Hills = subset(studyForestsSPDF, FOREST == "Black Hills"),
    Colorado = subset(studyForestsSPDF, FOREST == "Colorado"),
    Colville = subset(studyForestsSPDF, FOREST == "Colville"),
    WesternUS =   gIntersection(
      pineRanges$unionBuffered, 
      gUnaryUnion(crop(usStates, studySiteExtents$WesternUS_unmasked))),
    WesternUS_unmasked = 
      gUnaryUnion(crop(usStates, studySiteExtents$WesternUS_unmasked)))
  
  # Verify that the polygons are correct graphically:
  plot(crop(usStates, studySitePolygons$WesternUS_unmasked), border = "gray")
  plot(studySitePolygons$WesternUS_unmasked, add = T)
  plot(studySitePolygons$WesternUS, add = T, border = 2)  
  lapply(studySitePolygons[1:4], function(x) plot(x, add = T, border = 4))
  
  # Verify that the projections are correct:
  proj4string(studyForestsSPDF) == proj4stringMaster
  proj4string(usStates) == proj4stringMaster
  lapply(pineRanges, function(x) proj4string(x) == proj4stringMaster)
  lapply(studySitePolygons, function(x) proj4string(x) == proj4stringMaster)
  
  save(pineRanges, studySitePolygons, file = paste0(dataSaveDir, "studySitesSpatial.Rd"))
  
  # Environment cleanup:
  rm(nationalForestsSPDF, studyForestsSPDF, forestNames)
}

# Tree kill rasters -------------------------------------------------------
{
  USTreesPerCellDirs = list.dirs(paste0(DatabasinDataPath, "Western_Conterminous_US_Killed_Trees_Per_Grid_Cell_Mountain_Pine_Beetle"), recursive = F)
  i = 1; (contortaRasterFiles = list.files(USTreesPerCellDirs[i], "w001001.adf", recursive = T, full.names = T))
  i = 2; (ponderosaRasterFiles = list.files(USTreesPerCellDirs[i], "w001001.adf", recursive = T, full.names = T))
  
  treeKillRasterStack = stack()
  for(i in 1:length(contortaRasterFiles)){
    print(i)
    
    conR = raster(contortaRasterFiles[i])
    ponR = raster(ponderosaRasterFiles[i])
    
    # Set the na values to zero
    conR[conR[] == -9999] = 0
    ponR[ponR[] == -9999] = 0
    
    # We only want the sum (for now)
    sumR = projectRaster(conR + ponR, crs = CRS(proj4stringMaster))
    
    treeKillRasterStack = addLayer(treeKillRasterStack, sumR)
    rm(conR, ponR, sumR)
  }
  names(treeKillRasterStack) = paste0("pineKill_", killYears)
  
  # Now we need stacks cropped to the study sites:
  treeKillRastersCropped = list()
  for(name in names(studySitePolygons)){
    print(name)
    treeKillRastersCropped[[name]] = crop(treeKillRasterStack, studySitePolygons[[name]])
  }  
  
  rm(name)
  save(treeKillRasterStack, treeKillRastersCropped, file = paste0(dataSaveDir, "treeKillSpatial.Rd"))
}



# MPB Survival rasters ----------------------------------------------------
{
  
  # Get a lat-lon bounding box for the kill data
  extentMaster = as(extent(spTransform(studySitePolygons$WesternUS_unmasked, proj4stringLonLat)), "SpatialPolygons")
  proj4string(extentMaster) = proj4stringLonLat
  
  daymetFiles = paste0(daymetFilesDir, list.files(daymetFilesDir))
  
  # Create a giant spatial points data frame with all the daymet cells:
  
  # A list with pre-defined length populates much faster than recursive calls to list()
  daymetSPDFs = vector(mode = "list", length = length(daymetFiles))
  nrowsDF = 0
  counterDFs = 1
  
  for(i in 1:length(daymetFiles)) { 
    dm = makeDaymet(daymetFiles[i]) 
    # if any part of the daymet tile falls within the boundary:
    if(daymetIn(extentMaster, dm)){
      print(paste0(i, " ", dm@tileNum))
      dmSPDF = rasterToPoints(dm@data, spatial = T)
      stopifnot(proj4string(dmSPDF) == proj4stringMaster)
      names(dmSPDF@data)
      dmSPDF = dmSPDF[complete.cases(dmSPDF@data), ]
      daymetSPDFs[[counterDFs]] = dmSPDF[!is.na(over(dmSPDF, studySitePolygons$WesternUS_unmasked)), ]
      nrowsDF = nrowsDF + nrow(daymetSPDFs[[counterDFs]]@data)
      counterDFs = counterDFs + 1
    }
  }
  
  # Pre-making a data frame is much faster than repeated calls to rbind()
  mpbSurvivalSpatialPointsDF = data.frame(matrix(0, nrow = nrowsDF, ncol = ncol(dmSPDF) + 2))
  nrowsDF; counterDFs
  i = 1
  count = 1
  for(i in 1:(counterDFs - 1)){
    nrowsDF1 = nrow(daymetSPDFs[[i]]@data)
    if(nrowsDF1 > 0){
      from = count;
      to = from + nrowsDF1 - 1
      mpbSurvivalSpatialPointsDF[from:to, 1:2] = daymetSPDFs[[i]]@coords  
      mpbSurvivalSpatialPointsDF[from:to, 3:(2 + ncol(daymetSPDFs[[i]]@data))] = daymetSPDFs[[i]]@data
      count = count + nrow(daymetSPDFs[[i]]@data)
      print(i)
    }
  }
  
  names(mpbSurvivalSpatialPointsDF) = c("x", "y", names(daymetSPDFs[[1]]@data))
  str(daymetSPDFs[[i]]@data, 0)
  str(mpbSurvivalSpatialPointsDF, 2)
  tail(mpbSurvivalSpatialPointsDF)
  head(mpbSurvivalSpatialPointsDF)
  coordinates(mpbSurvivalSpatialPointsDF) <- ~ x + y
  
  # That was a lot of work, so we'll save the intermediate result:
  save(mpbSurvivalSpatialPointsDF, file = paste0(dataSaveDir, "mpbSurvivalSpatialPointsDF.Rd"))
  
  # load(paste0(dataSaveDir, "treeKillSpatial.Rd"))
  # load(paste0(dataSaveDir, "mpbSurvivalspatialpointsdf.Rd"))
  # load(paste0(dataSaveDir, "treeKillSpatial.Rd"))
  # load(paste0(dataSaveDir, "studySitesSpatial.Rd"))
  
  # Make raster stacks with the same spatial extent and resolution as the tree kill stacks:
  mpbSurvivalRastersCropped = vector(mode = "list", length = length(treeKillRastersCropped))
  names(mpbSurvivalRastersCropped) = names(treeKillRastersCropped)
  
  for(name in names(treeKillRastersCropped)){
    print(name)
    st = stack()
    sss = nabor::knn(coordinates(mpbSurvivalSpatialPointsDF), 
                     coordinates(treeKillRastersCropped[[name]]), k = 1) 
    templateR = subset(treeKillRastersCropped[[name]], 1)
    length(sss$nn.dists)
    sum(sss$nn.dists < 1000)
    outsideMask = which(sss$nn.dists > 1000)
    
    summary(sss$nn.dists)
    
    for(i in 1:(ncol(mpbSurvivalSpatialPointsDF@data) - 1)){
      print(i)
      layer = templateR
      layer[] = mpbSurvivalSpatialPointsDF@data[sss$nn.idx, i]
      layer[outsideMask] = NA  
      st = addLayer(st, layer)
    }
    names(st) = paste0("MpbSurvival_", names(mpbSurvivalSpatialPointsDF@data)[-length(names(mpbSurvivalSpatialPointsDF@data))])
    mpbSurvivalRastersCropped[[name]] = st
  }
  
  # Create a masked raster of the western US (this takes a LONG time!!):
  mpbSurvivalRastersCropped$WesternUS = mask(mpbSurvivalRastersCropped$WesternUS, studySitePolygons$WesternUS)
  
  save(mpbSurvivalRastersCropped, file = paste0(dataSaveDir, "mpbSurvivalRastersCropped.Rd"))
  rm(sss, st, templateR, name, rrr)
}



# Aggregate data ----------------------------------------------------------
{
  
  # Yearly average survival in study sites
  {
    mpbSurvivalYearlyMeans = data.frame(matrix(0, nrow = length(survivalYears), ncol = length(mpbSurvivalRastersCropped) + 1))
    names(mpbSurvivalYearlyMeans) = c("year", names(mpbSurvivalRastersCropped))
    mpbSurvivalYearlyMeans$year = survivalYears
    for(name in names(mpbSurvivalRastersCropped)){
      means = c()
      for(i in 1:nlayers(mpbSurvivalRastersCropped[[name]])){
        print(i)
        means = c(means, mean(subset(mpbSurvivalRastersCropped[[name]], i)[], na.rm = T))
      }
      mpbSurvivalYearlyMeans[[name]] = means
    }
  } 
  
  # Yearly average tree kill in study sites
  {
    treeKillYearlyMeans = data.frame(year = killYears, matrix(0, nrow = length(killYears), ncol = length(mpbSurvivalRastersCropped)))
    names(treeKillYearlyMeans)[-1] = names(mpbSurvivalRastersCropped)
    head(treeKillYearlyMeans)
    treeKillYearlyMeans_nonzeroCells = treeKillYearlyMeans
    for(name in names(mpbSurvivalRastersCropped)){
      means = c()
      means_nonzero = c()
      for(i in 1:length(killYears)){
        print(i)
        rrr = subset(treeKillRastersCropped[[name]], i)
        mm = mean(rrr[], na.rm = T); if(is.na(mm)) mm = 0
        mm0 = mean(rrr[rrr[] > 0], na.rm = T); if(is.na(mm0)) mm0 = 0
        means = c(means, mm)
        means_nonzero = c(means_nonzero, mm0)
      }
      treeKillYearlyMeans[[name]] = means
      treeKillYearlyMeans_nonzeroCells[[name]] = means_nonzero
    }
  }
  
    
  save(treeKillYearlyMeans, treeKillYearlyMeans_nonzeroCells, mpbSurvivalYearlyMeans, file = paste0(dataSaveDir, "yearlyAggregateData.Rd"))
  
  plot(calc(subset(treeKillRastersCropped[[name]], 1:4), mean))
  plot(calc(subset(mpbSurvivalRastersCropped[[name]], 1:5), mean))
  
  treeKillYearlyMeans
  treeKillYearlyMeans_nonzeroCells
  
  treekillras
  
}

# 
# 
# # mpbSurvivalSpatialPoints, mpbSurvivalSpatialPointsUnmasked   Daymet: create a SpatialPointsDataFrame of the rasters --------------------------
# { 
#   mpbSurvivalSpatialPointsUnmasked = rasterToPoints(daymetList[[1]]@data, spatial = T)
#   
#   # Keep only points within the ranges of the pines (this is very slow):
#   r1 = mask(daymetList[[1]]@data, ranges$unionBuffered)
#   mpbSurvivalSpatialPoints = rasterToPoints(r1, spatial = T)
#   
#   for (i in 2:length(daymetList)){
#     print(i)
#     # mpbSurvivalSpatialPointsUnmasked = rbind(mpbSurvivalSpatialPointsUnmasked, rasterToPoints(daymetList[[i]]@data, spatial = T))
#     r1 = mask(daymetList[[i]]@data, ranges$unionBuffered)
#     mpbSurvivalSpatialPoints = rbind(mpbSurvivalSpatialPoints, rasterToPoints(r1, spatial = T))
#   }
#   
#   mpbSurvivalSpatialPoints = spTransform(mpbSurvivalSpatialPoints, proj4stringMaster)
#   mpbSurvivalSpatialPointsUnmasked = spTransform(mpbSurvivalSpatialPointsUnmasked, proj4stringMaster)
#   
#   save(mpbSurvivalSpatialPoints, file = paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))
#   save(mpbSurvivalSpatialPointsUnmasked, file = paste0(dataSaveDir, "mpbSurvivalSpatialPointsUnmasked.Rd"))
#   
#   rm(r1)
#   
#   
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ranges  Pine Species Range Maps ----------------------------------------------------
# {
#   ranges = list(
#     ponderosa = spTransform(readOGR(dsn = paste0(treeMapDir, "/Pinus_ponderosa/data/commondata/data0"), "pinupond"), proj4stringMaster),
#     contorta = spTransform(readOGR(dsn = paste0(treeMapDir, "/Pinus_contorta/data/commondata/data0"), "pinucont"), proj4stringMaster)
#   )
#   
#   ranges$union = gUnion(ranges$ponderosa, ranges$contorta)
#   ranges$unionBuffered = gBuffer(ranges$union, width = rangeBufferWidth)
#   
#   save(ranges, file = paste0(dataSaveDir, "ranges.Rd"))
#   
# }
# 
# #  daymetList   Daymet: Read all the daymet rasters ---------------------------------------------
# {
#   files = paste0(daymetFilesDir, list.files(daymetFilesDir))
#   daymetList = vector("list", length(files))
#   
#   for(i in 1:length(files)) { 
#     print(i); 
#     daymetList[[i]] = makeDaymet(files[i]) }
#   save(daymetList, file = paste0(dataSaveDir, "daymetList.Rd"))
#   
#   canada = spTransform(readOGR(canadaDir, "lpr_000a16a_e"), daymetList[[1]]@crs)
#   us = spTransform(readOGR(statesDir, "tl_2017_us_state"), daymetList[[1]]@crs)
#   plot(us, xlim = c(-2400000, 1000000), ylim = c(-1000000, 2000000), border = 0); plot(canada, add = T)
#   lapply(daymetList, function(x) plot(extent(x@mask), add = T))
#   
#   rm(files)
#   
#   # Check that the lat/lon ranges make sense 
#   extents = lapply(daymetList, function(dm) extent(expand.grid(y = dm@latRange, x = dm@lonRange)))
#   plot(0, xlim = c(-130, -100), ylim = c(25, 60), type = "n") 
#   lapply(extents, function(x) plot(x, add = T))
#   rm(extents, us, canada)
# }
# 
# 
# # mpbSurvivalSpatialPoints, mpbSurvivalSpatialPointsUnmasked   Daymet: create a SpatialPointsDataFrame of the rasters --------------------------
# { 
#   mpbSurvivalSpatialPointsUnmasked = rasterToPoints(daymetList[[1]]@data, spatial = T)
#   
#   # Keep only points within the ranges of the pines (this is very slow):
#   r1 = mask(daymetList[[1]]@data, ranges$unionBuffered)
#   mpbSurvivalSpatialPoints = rasterToPoints(r1, spatial = T)
#   
#   for (i in 2:length(daymetList)){
#     print(i)
#     # mpbSurvivalSpatialPointsUnmasked = rbind(mpbSurvivalSpatialPointsUnmasked, rasterToPoints(daymetList[[i]]@data, spatial = T))
#     r1 = mask(daymetList[[i]]@data, ranges$unionBuffered)
#     mpbSurvivalSpatialPoints = rbind(mpbSurvivalSpatialPoints, rasterToPoints(r1, spatial = T))
#   }
#   
#   mpbSurvivalSpatialPoints = spTransform(mpbSurvivalSpatialPoints, proj4stringMaster)
#   mpbSurvivalSpatialPointsUnmasked = spTransform(mpbSurvivalSpatialPointsUnmasked, proj4stringMaster)
#   
#   save(mpbSurvivalSpatialPoints, file = paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))
#   save(mpbSurvivalSpatialPointsUnmasked, file = paste0(dataSaveDir, "mpbSurvivalSpatialPointsUnmasked.Rd"))
#   
#   rm(r1)
#   
# }
# 
# # ponderosaRasters, contortaRasters  Databasin--------------------------- 
# {
#   USTreesPerCellDirs = list.dirs(paste0(DatabasinDataPath, "Western_Conterminous_US_Killed_Trees_Per_Grid_Cell_Mountain_Pine_Beetle"), recursive = F)
#   i = 1; (contortaRasterFiles = list.files(USTreesPerCellDirs[i], "w001001.adf", recursive = T, full.names = T))
#   i = 2; (ponderosaRasterFiles = list.files(USTreesPerCellDirs[i], "w001001.adf", recursive = T, full.names = T))
#   contortaRasters = lapply(contortaRasterFiles, raster)
#   ponderosaRasters = lapply(ponderosaRasterFiles, raster)
#   # Set the -9999 values to zero
#   for(i in 1:length(contortaRasters)){
#     print(i)
#     contortaRasters[[i]][contortaRasters[[i]] == -9999] = 0
#     ponderosaRasters[[i]][ponderosaRasters[[i]] == -9999] = 0
#   }
#   
#   save(contortaRasters, file = paste0(dataSaveDir, "contortaRasters.Rd"))
#   save(ponderosaRasters, file = paste0(dataSaveDir, "ponderosaRasters.Rd"))
#   rm(i, USTreesPerCellDirs, ponderosaRasterFiles, contortaRasterFiles)
# }
# 
# # mpbSurvivalTreeKill_WesternUs  Databasin: get all raster cells with any tree kill activity duri --------
# # load(paste0(dataSaveDir, "daymetList.Rd"))
# {
#   cellIDList = vector(mode = "list", length = length(contortaRasters))
#   for(i in 1:length(contortaRasters)){
#     print(i)
#     cellIDList[[i]] =   
#       c(which(ponderosaRasters[[i]][] > 0), which(contortaRasters[[i]][] > 0))
#   }
#   
#   # raster cell IDs corresponding to pixels that had beetle kill in at least one of the study years
#   cellIDs = unique(unlist(cellIDList))
#   
#   # Coordinates of the cells
#   cellXY = data.frame(xyFromCell(ponderosaRasters[[1]], cellIDs))
#   head(cellXY)
#   
#   coordinates(cellXY) = ~ x + y
#   proj4string(cellXY) = proj4string(ponderosaRasters[[1]])
#   
#   leadingColumnNames = c("cellID")
#   
#   # Collect the tree killed data into a data frame
#   mpbSurvivalTreeKill_WesternUs = data.frame(cellID = cellIDs, matrix(0, nrow = length(cellIDs), ncol = 2 * length(killYears)))
#   mpbSurvivalTreeKill_WesternUs = cbind(mpbSurvivalTreeKill_WesternUs, matrix(0, nrow = length(cellIDs), ncol = length(survivalYears)))
#   names(mpbSurvivalTreeKill_WesternUs) = 
#     c(
#       leadingColumnNames, 
#       paste0("Ponderosa_Kill_Observed_", killYears),paste0("Contorta_Kill_Observed_", killYears), 
#       paste0("Mpb_survival_winter_ending_", survivalYears))
#   
#   # Convert it to a spatial points dataframe
#   coordinates(mpbSurvivalTreeKill_WesternUs) = cellXY@coords
#   proj4string(mpbSurvivalTreeKill_WesternUs) = proj4string(ponderosaRasters[[1]])
#   summary(mpbSurvivalTreeKill_WesternUs)
#   
#   # Add the tree kill values
#   for (i in 1:length(ponderosaRasters)){
#     pIndex = i + length(leadingColumnNames)
#     cIndex = pIndex + length(killYears)
#     print(names(mpbSurvivalTreeKill_WesternUs)[pIndex])
#     names(mpbSurvivalTreeKill_WesternUs)[cIndex]
#     mpbSurvivalTreeKill_WesternUs@data[, pIndex] = ponderosaRasters[[i]][cellIDs]
#     mpbSurvivalTreeKill_WesternUs@data[, cIndex] = contortaRasters[[i]][cellIDs]
#   }
#   
#   # Get the closest points with survival values
#   survivalIndices = (ncol(mpbSurvivalTreeKill_WesternUs) - (length(survivalYears) - 1)):ncol(mpbSurvivalTreeKill_WesternUs)
#   
#   # load(paste0(dataSaveDir, "mpbSurvivalSpatialPointsMasked.Rd"))
#   mpbSurvivalTreeKill_WesternUs = spTransform(mpbSurvivalTreeKill_WesternUs, proj4stringMaster)
#   ddd = knn(coordinates(mpbSurvivalSpatialPointsUnmasked), coordinates(mpbSurvivalTreeKill_WesternUs), k = 1)
#   mpbSurvivalTreeKill_WesternUs@data[, survivalIndices] = mpbSurvivalSpatialPointsUnmasked@data[ddd$nn.idx, ]
#   save(mpbSurvivalTreeKill_WesternUs, file = paste0(dataSaveDir, "mpbSurvivalTreeKill_WesternUs.Rd"))
#   head(mpbSurvivalTreeKill_WesternUs)  
#   plot(mpbSurvivalTreeKill_WesternUs, pch = 16, cex = 0.1)
#   rm(ddd, survivalIndices, cIndex, pIndex, cellXY, leadingColumnNames, cellIDs, cellIDList)
# }
# 
# # studySites, colMap Setup Study Sites vector data -------------------------------------------------------
# {
#   studySites = spTransform(readOGR(studySiteDir, "mpb_study_sites"), proj4stringMaster)
#   # Keep just the for code and name'
#   studySites@data = studySites@data[, c("FOREST", "FORCODE")]
#   
#   levels(studySites$FOREST)
#   
#   # Combine araphao, white river, and medicine bow into the same study site:
#   oldNames = list(
#     "Arapaho and Roosevelt National Forests", 
#     "Beaverhead-Deerlodge National Forest", 
#     "Black Hills National Forest", 
#     "Colville National Forest",
#     "Medicine Bow National Forest", 
#     "White River National Forest")
#   newNames = list(
#     "Colorado",
#     "Colorado",
#     "Colorado",
#     "Beaverhead", 
#     "Black Hills", 
#     "Colville")
#   newCodes = list(3, 3, 3, 2, 1, 4)
#   
#   studySiteCode = vector(mode = "numeric", length = nrow(studySites))
#   studySiteName = vector(mode = "character", length = length(studySiteCode))
#   
#   replacements = data.frame(oldNames = c("Black", "Beaverhead", "Arapaho", "Medicine", "White", "Colville"),
#                             newNames = c("Black Hills", "Beaverhead", "Colorado", "Colorado", "Colorado", "Colville"),
#                             newCodes = c(1, 2, 3, 3, 3, 4))
#   for(i in 1:length(oldNames)){
#     studySiteName[grepl(replacements$oldNames[i], studySites$FOREST)] = as.character(replacements$newNames[i])
#     studySiteCode[grepl(replacements$oldNames[i], studySites$FOREST)] = as.character(replacements$newCodes[i])
#     
#   }
#   
#   studySites@data = cbind(studySites@data, studySiteName, studySiteCode)
#   
#   colMap = unique(studySites@data[, 3:4])
#   colMap$col = c(3, 2, 1, 4)
#   plot(studySites, col = colMap$col[match(studySites$studySiteName, colMap$studySiteName)], border = NA)
#   
#   legend("bottomleft", legend = colMap$studySiteName, col = colMap$col, pch = 15, bg = "white")
#   
#   save(studySites, file = paste0(dataSaveDir, "studySites.Rd"))
#   save(colMap, file = paste0(dataSaveDir, "colMap.Rd"))
#   
#   rm(replacements, studySiteName, newCodes, newNames, oldNames, studySiteCode)
# }
# 
# # mpbSurvivalTreeKill_StudySites Extract data for study sites and set up colors/plotting names --------------------------------------------
# {
#   mpbSurvivalTreeKill_StudySites = mpbSurvivalTreeKill_WesternUs[
#     !is.na(over(mpbSurvivalTreeKill_WesternUs, as(studySites, "SpatialPolygons"))), ]
#   
#   forestIndices = over(mpbSurvivalTreeKill_StudySites, as(studySites, "SpatialPolygons"))
#   mpbSurvivalTreeKill_StudySites = cbind(mpbSurvivalTreeKill_StudySites, studySites[forestIndices, c("FOREST", "FORCODE", "studySiteName", "studySiteCode")])
#   
#   save(mpbSurvivalTreeKill_StudySites, file = paste0(dataSaveDir, "mpbSurvivalTreeKill_StudySites.Rd"))
#   rm(forestIndices)
# }
# 
# 
# 
# # extentSites, shapeSites Bounding boxes for study sites ------------------------------------------
# {
#   extentSites = list(
#     Beaverhead = extent(x = c(-1064015, -820737), y = c(235392, 539489)),
#     BlackHills = extent(x = c(-406060, -195957), y = c(72286, 287917)),
#     Colorado = extent(x = c(-748860, -325889), y = c(-392153, 83343)),
#     Colville = extent(x = c(-1371116, -1146951), y = c(683243, 868466)),
#     WesternUS = extent(x = c(-2001120, 100000), y = c(-6e5, 951135)),
#     WesternUS_unmasked = extent(x = c(-2001120, 100000), y = c(-6e5, 951135)))
#   
#   
#   
#   
#   # for each study site, keep only the polygons within the extents.  
#   shapeSites = lapply(extentSites, function(x) crop(studySites, x))
#   names(shapeSites) = names(extentSites)
#   
#   
#   
#   
#   plot(shapeSites$WesternUS)
#   
#   
#   ss1 = gIntersection(shapeSites$we, states)
#   
#   
#   # 
#   shapeSites$WesternUS_unmasked = crop(states, extentSites$WesternUS)
#   
#   # Mask all of the areas int he Western US that aren't in the range of P. ponderosa or contorta:
#   shapeSites$WesternUS = gIntersection(shapeSites$WesternUS_unmasked, ranges$unionBuffered)
#   
#   plot(shapeSites$WesternUS, add = T)
#   
#   save(extentSites, file = paste0(dataSaveDir, "extentSites.Rd"))
#   save(shapeSites, file = paste0(dataSaveDir, "shapeSites.Rd"))
# }
# 
# # mpbSurvivalStacks  Raster stacks of survival for the study sites ---------------------------
# {
#   mpbSurvivalStacks = lapply(extentSites, function(x) getStackFromExt(x, mpbSurvivalSpatialPoints))
#   names(mpbSurvivalStacks) = names(extentSites)
#   mpbSurvivalStacks$WesternUS_unmasked = mpbSurvivalStacks$WesternUS
#   mpbSurvivalStacks$WesternUS = mask(mpbSurvivalStacks$WesternUS, shapeSites$WesternUS)
#   save(mpbSurvivalStacks, file = paste0(dataSaveDir, "mpbSurvivalStacks.Rd"))
# }
# 
# # stackSitesPonderosa, stackSitesContorta, stackSitesPonderosaMasked   RasterStacks for ponderosa and contorta ---------------------------------
# {
#   layerNames = paste0("tree_kill_", killYears)
#   for (i in 1:length(extentSites))
#     
#     stackSitesPonderosa = lapply(extentSites, function(ext) 
#       reprojectAndCropToStack(ponderosaRasters, ext, proj4stringMaster, layerNames))
#   stackSitesContorta = lapply(extentSites, function(ext) 
#     reprojectAndCropToStack(contortaRasters, ext, proj4stringMaster, layerNames))
#   stackSitesPonderosaMasked = stackSitesContortaMasked = vector(mode = "list", length = length(stackSitesContorta))
#   
#   for(i in 1:length(stackSitesPonderosa)){
#     print(i)
#     stackSitesPonderosaMasked[[i]] = mask(stackSitesPonderosa[[i]], shapeSites[[i]])  
#     stackSitesContortaMasked[[i]] = mask(stackSitesContorta[[i]], shapeSites[[i]])  
#   }
#   
#   names(stackSitesPonderosaMasked) = names(stackSitesContortaMasked) = names(stackSitesPonderosa) = names(stackSitesContorta) = names(shapeSites)
#   save(stackSitesPonderosa, stackSitesContorta, stackSitesPonderosaMasked, stackSitesContortaMasked, file = paste0(dataSaveDir, "stackSites.Rd"))
#   rm(layerNames, proj4ext)
# }
# 
# # siteMeanMpbSurvival   Mean survival each year in study sites ----------------------------------
# {
#   nnn = c("year", names(mpbSurvivalStacks))
#   siteMeanMpbSurvival = data.frame(matrix(0, ncol = length(nnn), nrow = length(survivalYears)))
#   names(siteMeanMpbSurvival) = nnn
#   siteMeanMpbSurvival$year = survivalYears
#   i = 1
#   for(i in 1:length(mpbSurvivalStacks)){
#     print(i); print(names(siteMeanMpbSurvival)[i])
#     
#     colIndex = which(names(siteMeanMpbSurvival) == names(siteMeanMpbSurvival)[-1][i])
#     print(names(siteMeanMpbSurvival)[colIndex])
#     means = c()
#     for(j in 1:length(survivalYears)){
#       dat = subset(mpbSurvivalStacks[[i]], j)[]
#       means = c(means, mean(dat, na.rm = T)  )
#     }
#     
#     siteMeanMpbSurvival[, colIndex] = means
#   }
#   head(siteMeanMpbSurvival)
#   save(siteMeanMpbSurvival, file = paste0(dataSaveDir, "siteMeanMpbSurvival.Rd"))
#   rm(nnn, i, colIndex, dat, means)
# }
# 
# # mpbSurvivalSitesMultiYearAvgAggregate  multi-year average survival in study sites ------------------------------
# { 
#   mpbSurvivalSitesMultiYearAvgAggregate = lapply(1:12, function(x) timeLagMeanSurvivals(siteMeanMpbSurvival, x))
#   save(mpbSurvivalSitesMultiYearAvgAggregate, file = paste0(dataSaveDir, "mpbSurvivalSitesMultiYearAvgAggregate.Rd"))
#   # tail(mpbSurvivalSitesMultiYearAvgAggregate[[1]])
#   # names(mpbSurvivalSitesMultiYearAvgAggregate[[12]])
# }
# 
# 
# 
# # siteMeanKills   Mean trees killed per cell in study sites -------------------------------
# {
#   nnn = c("site",  "year",  "meanPon",  "meanCon",  "meanPonNonzero",  "meanConNonzero",  "meanSum",  "meanSumNonzero")
#   
#   siteMeanKills = data.frame(matrix(0, nrow = length(killYears) * length(stackSitesContorta),  ncol = length(nnn)))
#   names(siteMeanKills) = nnn
#   head(siteMeanKills)
#   
#   index = 1
#   for(i in 1:length(stackSitesContorta)){
#     print(i)
#     for(j in 1:  nlayers(stackSitesContorta[[i]])){
#       con = subset(stackSitesContorta[[i]], j)[]
#       pon = subset(stackSitesPonderosa[[i]], j)[]
#       sum1 = con + pon
#       siteMeanKills[index, 1] = names(stackSitesContorta)[i]
#       siteMeanKills[index, -1] =
#         c(
#           killYears[j],
#           mean(pon, na.rm = T),
#           mean(con, na.rm = T),
#           mean(pon[pon > 0], na.rm = T),
#           mean(con[con > 0], na.rm = T),
#           mean(sum1, na.rm = T),
#           mean(sum1[sum1 > 0], na.rm = T))
#       index = index + 1
#     }
#   }
#   
#   # Set any of the NaN values to 0
#   apply(siteMeanKills[, -1], 2, function(x) x[is.nan(x)] = 0)
#   is.nan(siteMeanKills[, 5])
#   
#   for(i in 2:ncol(siteMeanKills))
#   {
#     ss = siteMeanKills[, i]
#     siteMeanKills[, i][is.nan(ss)] = 0
#   }
#   
#   save(siteMeanKills, file = paste0(dataSaveDir, "siteMeanKills.Rd"))
#   rm(con, pon, ss, sum1, index, nnn, i, j)
# }
# 
# 
# 
# 
# #  treeKill_multiYearMeanMpbSurvival   Master data frame of aggregate mean kill per year and multi-year mean survival --------
# {
#   # Merge all into one data frame
#   
#   df1 = mpbSurvivalSitesMultiYearAvgAggregate[[1]]
#   for(i in 1:12){
#     names(mpbSurvivalSitesMultiYearAvgAggregate[[i]]) = c("year", paste0("survival_", names(mpbSurvivalStacks), "_", i, "_year_mean"))
#     df1 = merge(df1, mpbSurvivalSitesMultiYearAvgAggregate[[i]])
#   }
#   df2 = merge(data.frame(year = killYears), df1)
#   df3 = subset(siteMeanKills, site == siteNames[i])[, c(2, 7:8)]
#   treeKill_multYearMeanMpbSurvival = merge(df3, df2, all = F)
#   treeKill_multYearMeanMpbSurvival = df2
#   for(i in 5:1){
#     df3 = subset(siteMeanKills, site == siteNames[i])[, c(2, 7:8)]
#     names(df3) = c("year", paste0(siteNames[i], "_", c("meanKill", "meanKillNonzero")))
#     treeKill_multYearMeanMpbSurvival = merge(df3, treeKill_multYearMeanMpbSurvival, all = F)
#   }
#   head(treeKill_multYearMeanMpbSurvival)
#   save(treeKill_multYearMeanMpbSurvival, file = paste(dataSaveDir, "treeKill_multYearMeanMpbSurvival.Rd"))
#   rm(df1, df2, df3, i)
# }
