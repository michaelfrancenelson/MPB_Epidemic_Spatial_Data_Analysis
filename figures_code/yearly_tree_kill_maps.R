plotSeriesName = "yearly_tree_kill_maps"
plotSeriesDir = paste0(figureDir, plotSeriesName)
if(!dir.exists(plotSeriesDir)) dir.create(plotSeriesDir)

plotSeriesDescription = "
# Maps of the number of newly observed red-stage trees in 1-km square raster cells in the study sites
"
writeLines(plotSeriesDescription, paste0(plotSeriesDir, "/00_plot_descriptions.txt"))




# ggplot2 experimentation -------------------------------------------------

site = "WesternUS_unmasked"
layer = 2



############## this works
colr = colorRampPalette(rev(terrain.colors(10)))

levelplot(
  subset(mpbSurvivalRastersCropped[[site]], layer),
  margin=FALSE,                       # suppress marginal graphics
  colorkey=list(
    space='right' #,                   # plot legend at bottom
    # labels=list(at=, font=4)      # legend ticks and labels 
  ),    
  par.settings=list(
    axis.line=list(col='transparent') # suppress axes and legend outline
  ),
  scales=list(draw=FALSE),            # suppress axis labels
  col.regions=colr,                   # colour ramp
  at=seq(0, 1, len=101)) +           # colour ramp breaks
  layer(sp.polygons(crop(studySitePolygons[[site]], mpbSurvivalRastersCropped[[site]]))) +
  layer(sp.polygons(usStates, lwd = 0.1))



######################### experimental stuff below

gplot(subset(mpbSurvivalRastersCropped$Beaverhead, 8)) +
  geom_tile(aes(fill = factor(value)))



plotTitleNames = names(mpbSurvivalRastersCropped)
plotNames = plotTitleNames
plotTitleNames[5] = "Western US"; plotTitleNames[2] = "Black Hills"
plotTitleNames[6] = "Western US - Unmasked"
plotTitleNames; plotNames

site = 5
layer = 10
survivalYears[layer]


ddd = data.frame(rasterToPoints(rr))
head(ddd)

ss@data$id = rownames(ss@data)
ss.points = fortify(ss, region = "id")


# Plot the rasters:
rPt = ggplot(ddd, aes(x, y, fill = value)) + 
  geom_raster() +
  scale_fill_gradientn("Trees killed per cell", colors = terrain.colors(5)) +
  coord_fixed() 
  # theme_void()
  

ggplot(ddd, aes(x, y, fill = value)) + 
  geom_raster() +
  scale_fill_gradientn("Trees killed per cell", colors = terrain.colors(5)) +
  coord_fixed() +
  geom_line(data = newDat, aes(x = x, y = y1))

ggplot() + theme_void() +
  geom_point(aes(x = x, y = y1), data = newDat) +
  geom_line(aes(x = x, y = y1), data = newDat2)

ggplot() + geom_raster(aes(x, y, fill = value), data = ddd) +
  scale_fill_gradientn("Trees killed per cell", colors = terrain.colors(5))  +
  geom_line(aes(x = x, y = y1), data = newDat2)

ggplot() + 
  geom_polygon(aes(x = long, y = lat), data = ss.points)
  
  
  geom_path(aes(x = long, y = lat), data = ss.points)


rPt + geom_point(data = newDat, aes(x = x, y = y1)))
geom_path(data = ss.points, aes(x = long, y = lat))

newDat = data.frame(x = c(-1500000, -500000), y1 = c(-5e5, 5e5))
newDat2 = data.frame(x = c(-100000, -50000) , y1 = c(-4e5, 5e4))



       head(ss.points)
       geom_raster()

map = rr
#convert the raster to points for plotting
map.p <- rasterToPoints(map)

#Make the points a dataframe for ggplot
df <- data.frame(map.p)
#Make appropriate column headings
colnames(df) = c("Longitude", "Latitude", "MAP")



eee = as(extent(ss), "SpatialPolygons")

names(rr) = "value"

names(rr)
gplot(rr) +
  geom_tile(aes(fill = factor(value)))

#Now make the map
ggplot(data=df) +
  geom_raster(aes(fill=MAP)) 


  scale_fill_gradientn("Trees killed per cell", colors = terrain.colors(5)) +
  geom_path(data = ss.points)
  theme_void() + 


ddd = data.frame(rasterToPoints(rr))
head(ddd)

ggplot(data = ddd, aes(y = y, x = x)) + 
  geom_raster(aes(fill = raster(rr)))


  scale_fill_gradient(low = 1, high = 2)
dev.off()

rr = subset(treeKillRastersCropped[[site]], layer)
rr[rr[] == 0] = NA
ss = crop(usStates, treeKillRastersCropped[[site]])

data(mpg)
p <- ggplot(mpg, aes(displ, hwy))

pdf(file = "rplot.pdf")
plot(rr, box = F, axes = F)



colr = colorRampPalette(terrain.colors(300))

lp1 = 
levelplot(
  rr, margin = F,
  par.settings=list(
    axis.line=list(col='transparent') # suppress axes and legend outline
  ),
  scales = list(draw = F),
  col.regions = colr) 


lp1 + pp + lp1
pp

 


pp


class(pp)


plot(, lwd = 2, add = T, border = 2)
dev.off()


plot(studySitePolygons[[site]], lwd = 0.1, add = T)



for(site in 1:length(stackSitesContorta)){
  print(names(stackSitesContorta)[site])
  siteName = names(stackSitesContorta)[site]
  filename = paste0(plotSeriesDir, "/", plotSeriesName, "_", siteName, ".pdf")
  pdf(file = filename)
  zlim = c(0, 5000)
  # zlim = c(0, max(10, rangeD[2]))
  for(layer in 1:length(treeKillYears)){
    dat = subset(stackSitesContorta[[site]], layer) + subset(stackSitesPonderosa[[site]], layer)
    rangeD = range(dat[], na.rm = T)
    plot(dat, axes = F, box = F, zlim = zlim)
    plot(shapeSites[[site]], add = T, lwd = 0.4)
    title(siteName)
    mtext(paste0("red stage observed ", treeKillYears[layer]), side = 3)
  }
  dev.off()
}


