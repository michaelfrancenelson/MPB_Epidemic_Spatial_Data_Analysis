# Plot color-coded study sites --------------------------------------------
source("globals.R")
load(paste0(dataSaveDir, "studySitesSpatial.Rd"))


pdf(file = paste0(figureDir, "basic_study_site_map.pdf"))
plot(crop(usStates, studySitePolygons$WesternUS_unmasked), lwd = 0.4, border = "gray")
for(i in 1:4){plot(studySitePolygons[[i]], add = T, col = i, border = NA)}
legend("bottomleft", legend = names(studySitePolygons[1:4]), col = 1:4, pch = 15, bg = "white")
dev.off()
