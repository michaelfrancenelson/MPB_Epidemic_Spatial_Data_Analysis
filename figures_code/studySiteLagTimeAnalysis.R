source("LoadData.R")
source("plottingFunctions.R")

# moving average of mpb survival ------------------------------------------

nrows = 400; ncols = 400

colMap
forNames = colMap$studySiteName
forExtents = c(extentColorado, extentBeaverhead, extentBlackHills, extentColville)

i = 2

endYears = 1995:2016
endYear = 2010


layout(mat = matrix(1:2, nrow = 1))
plot(raster(matrix(1:1000 / 1000, ncol = 1)), col = heat.colors(1000))

filename = paste0(figureDir, forNames[i], "/MpbSurvivalWinter_", endYear - 1, "_", endYear, ".pdf")
mainTitle = paste0(forNames[i], " winter ", endYear - 1, "-", endYear)
pdf(filename)
par(mar = c(0,0,2,0))
rrr = plotMeanSurvival(mpbSurvivalSpatialPoints, ext = forExtents[[i]], endYear = 2016, nYears = 1, alpha = 1, nrows = nrows, ncols = ncols, nColors = 800, axes = F, box = F, cex = 0.5)
plot(studySites, lwd = 0.1, add = T)
mtext(mainTitle, side = 3)
dev.off()

pdf(paste0(figureDir, "test2.pdf"))
plot(rrr, col = terrain.colors(800))
dev.off()


