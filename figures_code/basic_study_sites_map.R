# Plot color-coded study sites --------------------------------------------
pdf(file = paste0(figureDir, "basic_study_site_map.pdf"))
plot(studySites, col = colMap$col[match(studySites$studySiteName, colMap$studySiteName)], border = NA)
plot(states, add = T, border = gray(0.5))
legend("bottomleft", legend = colMap$studySiteName, col = colMap$col, pch = 15, bg = "white")
dev.off()
