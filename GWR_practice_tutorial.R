require(GWmodel)
require(ggplot2)
require(plyr)
require(maptools)
require(rgdal)





source("helperFunctions.R")


shape = shapeSite
shapeSite@data$id = rownames(shapeSite@data)

s1 = fortify(shapeSite, region = "id")

s1df = join(s1, shapeSite@data, by = "id")
class(s1df)
ggplot(shape1) + aes(long, lat, group = group) + geom_polygon() + 
  geom_path(data = s1df, aes(long, lat), color = 2)


makeFortified = function(shape){
  # make a defensive copy:
  shape1 = shape
  # create an id column for the separate polygons
  shape1@data$id = rownames(shape1@data)
  # melt the data
  s1 = fortify(shape1, region = "id")
  return(join(s1, shape1@data, by = "id"))
}


shape1 = makeFortified(studySitePolygons[[4]])



data(LondonHP)


source("globals.R")
# Create a spatial grid 
nCols = 50
nRows = 60
# How many points to use for the GWR?
nPts = 2500

site = names(studySitePolygons)[3]
grd = spsample(studySitePolygons[[site]], nPts, type = "regular")


nYears = 8
survivalYear = 2005
killYear = 2006



shapeSite = studySitePolygons[[site]]


killSPDF = rasterToPoints(mask(treeKillRastersCropped[[site]], shapeSite), spatial = T)
survivalSPDF = rasterToPoints(mask(mpbSurvivalRastersCropped[[site]], shapeSite), spatial = T)
indices = sample(1:nrow(killSPDF@data), min(nPts, nrow(killSPDF)))
df1 = killSPDF[indices, ]
df1@data = cbind(df1@data, survivalSPDF@data[indices, ])
head(df1@data)
names1 = names(df1)

survivalColumns = which(grepl(survivalYear - nYears, names1) & grepl("Mpb", names1)):which(grepl(survivalYear, names1) & grepl("Mpb", names1))
df1$ex1 = apply(df1@data[, survivalColumns], 1, mean)



r1 = paste0("pineKill_", killYear)
ex2 = paste0("pineKill_", killYear - 1)
f1 = as.formula(paste(r1, "~", "ex1"))
f2 = as.formula(paste(r1, "~", "ex1 + ", ex2))

ggplot(df1@data, aes_string(x = "ex1", y = r1)) + geom_point()

distMat = gw.dist(dp.locat = coordinates(df1), rp.locat = coordinates(grd))


# bw1 = bw.gwr(f1, data = df1, adaptive = F)


bw2 = bw.gwr(f1, data = df1, adaptive = T)

gwr.res = gwr.basic(f1, data = df1, regression.points = grdPts, dMat = distMat, kernel = "gaussian", bw = bw2, adaptive = T, cv = T, F123.test = T)
gwr.res2 = gwr.basic(f2, data = df1, regression.points = grdPts, dMat = distMat, kernel = "gaussian", bw = bw2, adaptive = T, cv = T, F123.test = T)




str(gwr.res, 1)
head(gwr.res$SDF)
plot(studySitePolygons[[site]], border = "lightgray")
contour(gwr.res$SDF, "ex1", lwd = 1, add = T)

ggplot(gwr.res$SDF@data, aes(x = coordinates(gwr.res$SDF)[, 1], y = coordinates(gwr.res$SDF)[, 2])) +
  geom_point(aes(color = ex1), shape = 15) +
  geom_point(aes(x = long, y = lat), data = coordinates(shapeSite))

image(rasterize(gwr.res$SDF, "ex1"))
plot(studySitePolygons[[site]], add = T)

image(gwr.res2$SDF, ex2)
plot(studySitePolygons[[site]], add = T)


gwr.t.adjust(gwr.res)
summary(gwr.res$SDF)
summary(gwr.res2$SDF)

head()
gwr.res$GW.arguments
names(gwr.res)
gwr.res$Ftests

head(killSPDF)


rasterToPoints(killst)

?gwr.generalised







grd = buildGrid(studySitePolygons[[site]], nCols, nRows)






plot(grd)
plot(studySitePolygons[[site]], add = T)


bb = bbox(studySitePolygons[[site]])
(bb[, 2] - bb[, 1]) / 50
grd = points2grid(SpatialPoints(makegrid(studySitePolygons[[site]], 3500), CRS(proj4stringMaster)))
studySitePolygons[[site]]

ggplot(fortify(studySitePolygons[[site]], region = "id")) + aes(x = long, y = lat) + geom_polygon()


studySitePolygons[[site]]@data$id = rownames(studySitePolygons[[site]]@data)
df1 = join(fortify(studySitePolygons[[site]], region = "id"), studySitePolygons[[site]]@data)

studySitePolygons[[site]]
extent(studySitePolygons[[site]])




ggplot(df1, aes(x = long, y = lat, group = group)) + geom_polygon() + coord_equal()





head(studySitePolygons[[site]]@data)


fortify.SpatialPolygonsDataFrame()

fortify(utah, region="LEVEL3")


site = 1
killYear = 2005
survivalYear = 2003
nPts = 1000




kill = subset(stackSitesContorta[[site]] + stackSitesPonderosa[[site]], paste0("tree_kill_", killYear))
kill = subset(stackSitesContortaMasked[[site]] + stackSitesPonderosaMasked[[site]], paste0("tree_kill_", killYear))
survival = subset(mpbSurvivalStacks[[site]], paste0("X", survivalYear))

killSPDF = rasterToPoints(kill, spatial = T)
survivalSPDF = rasterToPoints(survival, spatial = T)

