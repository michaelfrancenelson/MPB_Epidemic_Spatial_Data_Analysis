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
    dfOut[rowIndex, -1] = apply(df[startIndex:(startIndex + nYears), ], 2, mean)[-1]
    dfOut[rowIndex, ]$year = endYear
    rowIndex = rowIndex + 1
  }
  return(dfOut)
}

reprojectAndCropToStack = function(rasterList, ext, proj4ext, layerNames = NA){
  
  bPolygon = as(ext, 'SpatialPolygons');
  proj4string(bPolygon) = proj4ext
  bPolygon1 = spTransform(bPolygon, proj4string(rasterList[[1]]))
  
  rr1 = crop(rasterList[[1]], extent(bPolygon1))
  rr = crop(projectRaster(rr1, crs = crs(bPolygon)), ext)
  st = stack(rr)
  
  i = 2
  for(r in rasterList[-1]){
    print(i); i = i + 1
    rr1 = crop(r, extent(bPolygon1))
    
    rr = crop(projectRaster(rr1, crs = crs(bPolygon)), ext)
    st = addLayer(st, rr)
  }
  
  if(!is.na(layerNames))
    names(st) = layerNames
  return(st)
}
