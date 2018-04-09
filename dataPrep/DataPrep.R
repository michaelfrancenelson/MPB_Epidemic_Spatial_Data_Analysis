# https://daymet.ornl.gov/

# Set up data paths -------------------------------------------------------
{
  require(data.table)
  require(rgdal)
  require(sp)
  require(nabor)
  source("dataPrep/daymet_class.R")
  source("globals.R")
  source("helperFunctions.R")
}

# Daymet: Read all the daymet rasters ---------------------------------------------
{
  files = paste0(daymetFilesDir, list.files(daymetFilesDir))
  daymetList = vector("list", length(files))
  
  for(i in 1:length(files)) { 
    print(i); 
    daymetList[[i]] = makeDaymet(files[i]) }
  save(daymetList, file = paste0(dataSaveDir, "daymetList.Rd"))
  
  canada = spTransform(readOGR(canadaDir, "lpr_000a16a_e"), daymetList[[1]]@crs)
  us = spTransform(readOGR(statesDir, "tl_2017_us_state"), daymetList[[1]]@crs)
  plot(us, xlim = c(-2400000, 1000000), ylim = c(-1000000, 2000000), border = 0); plot(canada, add = T)
  lapply(daymetList, function(x) plot(extent(x@mask), add = T))
  
  rm(files)
  
  # Check that the lat/lon ranges make sense --------------------------------
  extents = lapply(daymetList, function(dm) extent(expand.grid(y = dm@latRange, x = dm@lonRange)))
  plot(0, xlim = c(-130, -100), ylim = c(25, 60), type = "n") 
  lapply(extents, function(x) plot(x, add = T))
  rm(extents)
}


# Daymet: create a SpatialPointsDataFrame of the rasters --------------------------
{ 
  mpbSurvivalSpatialPoints = rasterToPoints(daymetList[[1]]@data, spatial = T)
  for (i in 2:length(daymetList)){
    print(i)
    mpbSurvivalSpatialPoints = rbind(mpbSurvivalSpatialPoints, rasterToPoints(daymetList[[i]]@data, spatial = T))
  }
  save(mpbSurvivalSpatialPoints, file = paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))
}

# Databasin:  Read the lodgepole and ponderosa pine rasters --------------------------- 
{
  USTreesPerCellDirs = list.dirs(paste0(DatabasinDataPath, "Western_Conterminous_US_Killed_Trees_Per_Grid_Cell_Mountain_Pine_Beetle"), recursive = F)
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
  rm(i, USTreesPerCellDirs, ponderosaRasterFiles, contortaRasterFiles)
}

# Databasin: get all raster cells with any tree kill activity duri --------
# load(paste0(dataSaveDir, "daymetList.Rd"))
{
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
  
  leadingColumnNames = c("cellID")
  
  # Collect the tree killed data into a data frame
  mpbSurvivalTreeKill_WesternUs = data.frame(cellID = cellIDs, matrix(0, nrow = length(cellIDs), ncol = 2 * length(killYears)))
  mpbSurvivalTreeKill_WesternUs = cbind(mpbSurvivalTreeKill_WesternUs, matrix(0, nrow = length(cellIDs), ncol = length(survivalYears)))
  names(mpbSurvivalTreeKill_WesternUs) = 
    c(
      leadingColumnNames, 
      paste0("Ponderosa_Kill_Observed_", killYears),paste0("Contorta_Kill_Observed_", killYears), 
      paste0("Mpb_survival_winter_ending_", survivalYears))
  
  # Convert it to a spatial points dataframe
  coordinates(mpbSurvivalTreeKill_WesternUs) = cellXY@coords
  proj4string(mpbSurvivalTreeKill_WesternUs) = proj4string(ponderosaRasters[[1]])
  summary(mpbSurvivalTreeKill_WesternUs)
  
  # Add the tree kill values
  for (i in 1:length(ponderosaRasters)){
    pIndex = i + length(leadingColumnNames)
    cIndex = pIndex + length(killYears)
    print(names(mpbSurvivalTreeKill_WesternUs)[pIndex])
    names(mpbSurvivalTreeKill_WesternUs)[cIndex]
    mpbSurvivalTreeKill_WesternUs@data[, pIndex] = ponderosaRasters[[i]][cellIDs]
    mpbSurvivalTreeKill_WesternUs@data[, cIndex] = contortaRasters[[i]][cellIDs]
  }
  names(mpbSurvivalTreeKill_WesternUs)
  # Get the closest points with survival values
  survivalIndices = (ncol(mpbSurvivalTreeKill_WesternUs) - (length(survivalYears) - 1)):ncol(mpbSurvivalTreeKill_WesternUs)
  names(mpbSurvivalTreeKill_WesternUs)[survivalIndices]
  # load(paste0(dataSaveDir, "mpbSurvivalSpatialPoints.Rd"))
  mpbSurvivalTreeKill_WesternUs = spTransform(mpbSurvivalTreeKill_WesternUs, proj4string(mpbSurvivalSpatialPoints))
  ddd = knn(coordinates(mpbSurvivalSpatialPoints), coordinates(mpbSurvivalTreeKill_WesternUs), k = 1)
  mpbSurvivalTreeKill_WesternUs@data[, survivalIndices] = mpbSurvivalSpatialPoints@data[ddd$nn.idx, ]
  save(mpbSurvivalTreeKill_WesternUs, file = paste0(dataSaveDir, "mpbSurvivalTreeKill_WesternUs.Rd"))
  
  plot(mpbSurvivalTreeKill_WesternUs, pch = 16, cex = 0.1)
  rm(ddd, survivalIndices, cIndex, pIndex, cellXY, leadingColumnNames, cellLonLat, cellIDs, cellIDList)
}

# Setup Study Sites vector data -------------------------------------------------------
{
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
  
  rm(replacements, studySiteName, newCodes, newNames, oldNames, studySiteCode, studySiteName)
}

# Extract data for study sites and set up colors/plotting names --------------------------------------------
{
  mpbSurvivalTreeKill_StudySites = mpbSurvivalTreeKill_WesternUs[
    !is.na(over(mpbSurvivalTreeKill_WesternUs, as(studySites, "SpatialPolygons"))), ]
  
  forestIndices = over(mpbSurvivalTreeKill_StudySites, as(studySites, "SpatialPolygons"))
  mpbSurvivalTreeKill_StudySites = cbind(mpbSurvivalTreeKill_StudySites, studySites[forestIndices, c("FOREST", "FORCODE", "studySiteName", "studySiteCode")])
  
  save(mpbSurvivalTreeKill_StudySites, file = paste0(dataSaveDir, "mpbSurvivalTreeKill_StudySites.Rd"))
  rm(forestIndices)
}

# Setup US States vector data ---------------------------------------------
{ 
  states = spTransform(readOGR(statesDir, "tl_2017_us_state"), proj)
  head(states@data)
  states@data = states@data[, c("STUSPS", 'NAME')]
  save(states, file = paste0(dataSaveDir, "States.Rd"))
}

# Bounding boxes for study sites ------------------------------------------
{
  # plot(studySites, col = colMap$col[match(studySites$studySiteName, colMap$studySiteName)], border = NA)
  extentSites = list(
    Beaverhead = extent(x = c(-1064015, -820737), y = c(235392, 539489)),
    BlackHills = extent(x = c(-406060, -195957), y = c(72286, 287917)),
    Colorado = extent(x = c(-748860, -325889), y = c(-392153, 83343)),
    Colville = extent(x = c(-1371116, -1146951), y = c(683243, 868466)),
    WesternUS = extent(x = c(-2001120, 100000), y = c(-6e5, 951135)))
  
  # plot(extentSites$WesternUS); plot(states, add = T)
  # plot(states, xlim = c(extentSites$WesternUS@xmin, extentSites$WesternUS@xmax))
  
  shapeSites = lapply(extentSites, function(x) crop(studySites, x))
  names(shapeSites) = names(extentSites)
  shapeSites$WesternUS = crop(states, extentSites$WesternUS)
  save(extentSites, file = paste0(dataSaveDir, "extentSites.Rd"))
  save(shapeSites, file = paste0(dataSaveDir, "shapeSites.Rd"))
}

# Raster stacks of survival for the study sites ---------------------------
{
  mpbSurvivalStacks = lapply(extentSites, function(x) getStackFromExt(x, mpbSurvivalSpatialPoints))
  names(mpbSurvivalStacks) = names(extentSites)
  save(mpbSurvivalStacks, file = paste0(dataSaveDir, "mpbSurvivalStacks.Rd"))
}

# RasterStacks for ponderosa and contorta ---------------------------------
{
  layerNames = paste0("tree_kill_", 1997:2010)
  proj4ext = proj4string(shapeSites$WesternUS)
  
  stackSitesPonderosa = lapply(extentSites, function(ext) reprojectAndCropToStack(ponderosaRasters, ext, proj4ext, layerNames))
  stackSitesContorta = lapply(extentSites, function(ext) reprojectAndCropToStack(contortaRasters, ext, proj4ext, layerNames))
  stackSitesPonderosaMasked = stackSitesContortaMasked = vector(mode = "list", length = length(stackSitesContorta))
  
  for(i in 1:length(stackSitesPonderosa)){
    print(i)
    stackSitesPonderosaMasked[[i]] = mask(stackSitesPonderosa[[i]], shapeSites[[i]])  
    stackSitesContortaMasked[[i]] = mask(stackSitesContorta[[i]], shapeSites[[i]])  
  }
  
  names(stackSitesPonderosaMasked) = names(stackSitesContortaMasked) = names(stackSitesPonderosa) = names(stackSitesContorta) = names(shapeSites)
  save(stackSitesPonderosa, stackSitesContorta, stackSitesPonderosaMasked, stackSitesContortaMasked, file = paste0(dataSaveDir, "stackSites.Rd"))
  rm(layerNames, proj4ext)
}

# Mean survival each year in study sites ----------------------------------
{
  nnn = c("year", names(mpbSurvivalStacks))
  siteMeanMpbSurvival = data.frame(matrix(0, ncol = length(nnn), nrow = length(survivalYears)))
  names(siteMeanMpbSurvival) = nnn
  siteMeanMpbSurvival$year = survivalYears
  i = 1
  for(i in 1:length(stackSitesContorta)){
    print(i)
    colIndex = which(names(siteMeanMpbSurvival) == names(stackSitesContorta)[i])
    means = c()
    for(j in 1:length(survivalYears)){
      dat = subset(mpbSurvivalStacks[[i]], j)[]
      means = c(means, mean(dat, na.rm = T)  )
    }
    siteMeanMpbSurvival[, colIndex] = means
  }
  save(siteMeanMpbSurvival, file = paste0(dataSaveDir, "siteMeanMpbSurvival.Rd"))
  rm(nnn, i, colIndex, dat, means)
}

# multi-year average survival in study sites ------------------------------
{ 
  mpbSurvivalSitesMultiYearAvgAggregate = lapply(1:12, function(x) timeLagMeanSurvivals(siteMeanMpbSurvival, x))
  save(mpbSurvivalSitesMultiYearAvgAggregate, file = paste0(dataSaveDir, "mpbSurvivalSitesMultiYearAvgAggregate.Rd"))
}



# Mean trees killed per cell in study sites -------------------------------
{
  nnn = c("site",  "year",  "meanPon",  "meanCon",  "meanPonNonzero",  "meanConNonzero",  "meanSum",  "meanSumNonzero")
  
  siteMeanKills = data.frame(matrix(0, nrow = length(killYears) * length(stackSitesContorta),  ncol = length(nnn)))
  names(siteMeanKills) = nnn
  head(siteMeanKills)
  
  index = 1
  for(i in 1:length(stackSitesContorta)){
    print(i)
    for(j in 1:  nlayers(stackSitesContorta[[i]])){
      con = subset(stackSitesContorta[[i]], j)[]
      pon = subset(stackSitesPonderosa[[i]], j)[]
      sum1 = con + pon
      siteMeanKills[index, 1] = names(stackSitesContorta)[i]
      siteMeanKills[index, -1] =
        c(
          killYears[j],
          mean(pon, na.rm = T),
          mean(con, na.rm = T),
          mean(pon[pon > 0], na.rm = T),
          mean(con[con > 0], na.rm = T),
          mean(sum1, na.rm = T),
          mean(sum1[sum1 > 0], na.rm = T))
      index = index + 1
    }
  }
  
  # Set any of the NaN values to 0
  apply(siteMeanKills[, -1], 2, function(x) x[is.nan(x)] = 0)
  is.nan(siteMeanKills[, 5])
  
  for(i in 2:ncol(siteMeanKills))
  {
    ss = siteMeanKills[, i]
    siteMeanKills[, i][is.nan(ss)] = 0
  }
  
  save(siteMeanKills, file = paste0(dataSaveDir, "siteMeanKills.Rd"))
  rm(con, pon, ss, sum1, index, nnn, i, j)
}




# Master data frame of aggregate mean kill per year and multi-year mean survival --------
{
  # Merge all into one data frame
  df1 = mpbSurvivalSitesMultiYearAvgAggregate[[1]]
  for(i in 1:12){
    names(mpbSurvivalSitesMultiYearAvgAggregate[[i]]) = c("year", paste0("survival_", siteNames, "_", i, "_year_mean"))
    df1 = merge(df1, mpbSurvivalSitesMultiYearAvgAggregate[[i]])
  }
  df2 = merge(data.frame(year = killYears), df1)
  df3 = subset(siteMeanKills, site == siteNames[i])[, c(2, 7:8)]
  treeKill_multYearMeanMpbSurvival = merge(df3, df2, all = F)
  treeKill_multYearMeanMpbSurvival = df2
  for(i in 5:1){
    df3 = subset(siteMeanKills, site == siteNames[i])[, c(2, 7:8)]
    names(df3) = c("year", paste0(siteNames[i], "_", c("meanKill", "meanKillNonzero")))
    treeKill_multYearMeanMpbSurvival = merge(df3, treeKill_multYearMeanMpbSurvival, all = F)
  }
  head(treeKill_multYearMeanMpbSurvival)
  save(treeKill_multYearMeanMpbSurvival, file = paste(dataSaveDir, "treeKill_multYearMeanMpbSurvival.Rd"))
  rm(df1, df2, df3, i)
}
