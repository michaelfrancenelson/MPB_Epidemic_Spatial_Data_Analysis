plotSeriesName = "yearly_tree_kill_maps"
plotSeriesDir = paste0(figureDir, plotSeriesName)
if(!dir.exists(plotSeriesDir)) dir.create(plotSeriesDir)

plotSeriesDescription = "
# Maps of the number of newly observed red-stage trees in 1-km square raster cells in the study sites
"
writeLines(plotSeriesDescription, paste0(plotSeriesDir, "/00_plot_descriptions.txt"))


treeKillYears = 1997:2010
layer = 10
site = 1

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


