



makeFortified = function(shape){
  # make a defensive copy:
  shape1 = shape
  if(is(shape1, "DataFrame")){
    
  }
  data.frame(shape1)
  slot(shape1, "data")
  is.data.frame(shape1)
  
  # create an id column for the separate polygons
  shape1@data$id = rownames(shape1@data)
  # melt the data
  s1 = fortify(shape1, region = "id")
  return(join(s1, shape1@data, by = "id"))
}

sourceSection = function(filename, sectionNames)
{
  lines1 = readLines(filename)
  for(sName in sectionNames){
    startIndex = which(grepl(paste0("# ", sName), lines1, ignore.case = T))
    endIndex = startIndex + 1
    while(!grepl("-------------------------", lines1[endIndex])) endIndex = endIndex + 1
    eval(parse(text = lines1[startIndex:endIndex]), envir = .GlobalEnv)
  }
}

killMeanSurvivalSubset = function(df1, site, nYears){
  dfNames =  names(df1)
  colIndices = c(1, 
                 which(grepl(paste0(site, "_", nYears, "_year"), dfNames)),
                 which(grepl(paste0(site, "_meanKill"), dfNames)))
  df1 = df1[, colIndices]
  names(df1) = c("year", "survival", "kill", "kill_nonzero")
  return(df1)
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


lengthXY = function(ext, unit = 1000){
  lx = abs(ext@xmax - ext@xmin)
  ly = abs(ext@ymax - ext@ymin)
  return(round(data.frame(nRows = ly, nColumns = lx) / unit))
}

timeLagMeanSurvivals = function(df,  nYears){
  dfOut = df[0, ]
  rowIndex = 1
  for(endYear in (df$year[1] + nYears):tail(df$year, 1)){
    startYear = endYear - nYears + 1
    startIndex = which(grepl(startYear, df$year))
    endIndex = startIndex + nYears - 1
    dfOut[rowIndex, -1] = apply(df[startIndex:endIndex, ], 2, mean)[-1]
    dfOut[rowIndex, ]$year = endYear
    rowIndex = rowIndex + 1
  }
  return(dfOut)
}




buildGrid = function(spdf,  nCols, nRows){
  bb = bbox(spdf)
  cellSize = (bb[, 2] - bb[, 1]) / c(nCols - 1, nRows - 1)
  return(SpatialGrid(GridTopology(c(bb[, 1]), cellSize, c(nCols, nRows))))
}


# rasterList = ponderosaRasters
# ext = extentSites$Beaverhead
# proj4ext = proj4stringMaster
# layerNames

reprojectAndCropToStack = function(rasterList, ext, proj4ext, layerNames = NA){
  
  # create a bounding polygon for the exteeeeetent of the 
  bPolygon = as(ext, 'SpatialPolygons');
  proj4string(bPolygon) = proj4ext
  
  # The bounding polygon reprojected to match the rasters:
  bPolygon1 = spTransform(bPolygon, proj4string(rasterList[[1]]))
  
  # Create an empty stack to be populated in the loop.  
  st = raster::stack()
  
  # for(r in rasterList[-1]){
  for(i in 1:length(rasterList)){
    print(i);
    # Crop the rasters to the extent of the polygon
    rr1 = crop(rasterList[[i]], extent(bPolygon1))
    # Reproject the raster to match the master projection:
    rr = projectRaster(rr1, crs = proj4ext)
    # Add the layer to the stack:
    st = addLayer(st, rr)
  }
  
  if(!is.na(layerNames[1]))
    names(st) = layerNames
  
  rm(rr, rr1, i, bPolygon, bPolygon1)  
  return(st)
}
