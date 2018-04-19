
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
  require(ggplot2)
  require(sf)
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
    WesternUS =  SpatialPolygonsDataFrame(
      gIntersection(
      gUnaryUnion(crop(usStates, studySiteExtents$WesternUS_unmasked)),
      pineRanges$unionBuffered), 
      data = data.frame(id = 1)),
    WesternUS_unmasked = SpatialPolygonsDataFrame(
      gUnaryUnion(crop(usStates, studySiteExtents$WesternUS_unmasked)),
      data = data.frame(id = 1)))
  
  
  plot(studySitePolygons$WesternUS)
  str(studySitePolygons$WesternUS, 2)
  SpatialPolygonsDataFrame(studySitePolygons$WesternUS, data = data.frame(id = 1))
  class(studySitePolygons$WesternUS)
  
  
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
  
  # Build 'fortified' versions for plotting with ggplot
  usStatesFortified = makeFortified(usStates)
  canadaFortified = makeFortified(canada)
  studySiteFortified = lapply(studySitePolygons, makeFortified)  
  
  save(pineRanges, studySitePolygons, studySiteFortified, usStates, canada, usStatesFortified, canadaFortified, file = paste0(dataSaveDir, "studySitesSpatial.Rd"))
  
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
  
  name = names(treeKillRastersCropped)[[1]]
  
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
  
  names1 = names(st)

  for(name in names(treeKillRastersCropped)){
    print(name)
    print(names(mpbSurvivalRastersCropped[[name]]))
    names(mpbSurvivalRastersCropped[[name]]) =gsub("X", "", names1)
    print(names(mpbSurvivalRastersCropped[[name]]))
  }
  save(mpbSurvivalRastersCropped, file = paste0(dataSaveDir, "mpbSurvivalRastersCropped.Rd"))
  rm(sss, st, templateR, name, rrr)
}



# Masked rasters ----------------------------------------------------------
{
  
  nn = names(mpbSurvivalRastersCropped)[1:4]

  mpbSurvivalMasked = treeKillMasked = vector(mode = "list", length = 5)
  names(mpbSurvivalMasked) = names(treeKillMasked) = c(nn, "WesternUS")
  
  name = nn[1]
  
  for(name in nn){
    print(name)
    mpbSurvivalMasked[[name]] = mask(mpbSurvivalRastersCropped[[name]], studySitePolygons[[name]])
    treeKillMasked[[name]] = mask(treeKillRastersCropped[[name]], studySitePolygons[[name]])
  }  
  
  mpbSurvivalMasked$WesternUS = mpbSurvivalRastersCropped$WesternUS
  treeKillMasked$WesternUS = mask(treeKillRastersCropped$WesternUS, studySitePolygons$WesternUS)
  
  image(treeKillMasked$WesternUS)
  image(mpbSurvivalMasked$WesternUS)
  
  
  save(treeKillMasked, mpbSurvivalMasked, file = paste0(dataSaveDir, "maskedRasters.Rd"))
  
  
  rm(nn, name)
    
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
  
}
