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


plotTitleNames = names(mpbSurvivalRastersCropped)
plotNames = plotTitleNames
plotTitleNames[5] = "Western US"; plotTitleNames[2] = "Black Hills"
plotTitleNames[6] = "Western US - Unmasked"
plotTitleNames; plotNames

site = 2
layer = 10
survivalYears[layer]


for(site in 1:length(plotNames)){
  print(plotTitleNames[site])  
  filename = paste0("MPB_Survival_SingleYear_", plotNames[site], ".pdf")
  pdf(file = paste0(plotSeriesDir, "/", filename))
  for(layer in 1:length(survivalYears)){
    print(survivalYears[layer])  
    mainTitle = paste0(plotTitleNames[site], " winter ", survivalYears[layer] - 1, "/", survivalYears[layer])
    
    plot(subset(mpbSurvivalRastersCropped[[site]], layer), axes = F, ann = F, box = F, zlim = c(0, 1))
    plot(studySitePolygons[[site]], add = T, lwd = 0.2, border = gray(0.1))
    title(mainTitle)    
  }
  dev.off()
}




lags = c(5, 8, 12)

lag = 5
layer = 5

for(lag in lags){
  yearStartIndices = lag:length(survivalYears)
  for(site in 1:length(plotNames)){
    print(plotTitleNames[site])  
    filename = paste0("MPB_Survival_", lag, "_year_mean_", plotNames[site], ".pdf")
    pdf(file = paste0(plotSeriesDir, "/", filename))
    for(layer in yearStartIndices){
      print(survivalYears[layer])  
      mainTitle = paste0(plotTitleNames[site], " winters ", survivalYears[layer - lag + 1] - 1, " to ", survivalYears[layer])
      plot(calc(subset(mpbSurvivalRastersCropped[[site]], (layer - lag + 1):layer), mean), axes = F, ann = F, box = F, zlim = c(0, 1))
      plot(studySitePolygons[[site]], add = T, lwd = 0.2, border = gray(0.1))
      title(mainTitle)
    }
    dev.off()
  }
}


i = 5; layer = 2
names(mpbSurvivalStacks)
names(shapeSites)
for(i in 1:length(mpbSurvivalStacks)){
  siteName = names(mpbSurvivalStacks)f[i]
  
  for(layer in 1:length(survivalYears)){ 
    print(layer)
    plot(mpbSurvivalStacks[[i]][[layer]], axes = F, box = F, zlim = c(0, 1)); plot(shapeSites[[i]], add = T, lwd = 0.4)
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
