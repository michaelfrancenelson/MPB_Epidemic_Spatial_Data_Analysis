source("LoadData.R")
source("globals.R")
source("helperFunctions.R")
source("PlottingFunctions.R")
plotSeriesName = "MPB_survival_spatial_multi_year_average"
plotSeriesDir = paste0(figureDir, plotSeriesName)
if(!dir.exists(plotSeriesDir)) dir.create(plotSeriesDir)

plotSeriesDescription = "# Maps of the modeled MPB survival, averaged over different lengths (5, 8, and 12 years). #"# winter corresponding to the survival values.
writeLines(plotSeriesDescription, paste0(plotSeriesDir, "/00_plot_descriptions.txt"))

# Single year winter survival plots for each study site -------------------
plot(extentSites$Colorado, axes = F, ann = F); 
plot(mpbSurvivalStacks$Colorado[[1]], axes = F, box = F); plot(shapeSites$Colorado, lwd = 0.4, add = T)

i = 5; layer = 2
names(mpbSurvivalStacks)
names(shapeSites)
for(i in 1:length(mpbSurvivalStacks)){
  siteName = names(mpbSurvivalStacks)f[i]
  filename = paste0("Survival_SingleYear_", siteName, ".pdf")
  
  pdf(file = paste0(plotSeriesDir, "/", filename))
  for(layer in 1:length(survivalYears)){ 
    print(layer)
    plot(mpbSurvivalStacks[[i]][[layer]], axes = F, box = F, zlim = c(0, 1)); plot(shapeSites[[i]], add = T, lwd = 0.4)
    mainTitle = paste0(siteName, " winter ", survivalYears[layer] - 1, "/", survivalYears[layer])
    mtext(mainTitle, side = 3)
  }
  dev.off()
}


# Multi-year average survival for study sites -----------------------------

nYears = 5
i = 2
layer = nYears

for(nYears in c(5, 8, 12)){
  for(i in 1:5){
    
    siteName = names(mpbSurvivalStacks)[i]
    filename = paste0("Survival_Average_", nYears, "_years_", siteName, ".pdf")
    
    pdf(file = paste0(plotSeriesDir, "/", filename))
    for(layer in (nYears + 1):length(survivalYears)){ 
      print(layer)
      plot(mean(subset(mpbSurvivalStacks[[i]], (layer - nYears + 1):layer)), axes = F, box = F, zlim = c(0, 1)); plot(shapeSites[[i]], add = T, lwd = 0.4)
      mainTitle = paste0(siteName, " mean survival\nfall ", survivalYears[layer - nYears], " - spring ",survivalYears[layer])
      mtext(mainTitle, side = 3)
    }
    dev.off()
  }
}
