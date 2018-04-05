source("LoadData.R")
source("PlottingFunctions.R")

# Single year winter survival plots for each study site -------------------
plot(extentSites$Colorado, axes = F, ann = F); 
plot(mpbSurvivalStacks$Colorado[[1]], axes = F, box = F); plot(shapeSites$Colorado, lwd = 0.4, add = T)

survivalYears = 1981:2017

i = 5
names(shapeSites)
for(i in 1:4){
  siteName = names(extentSites)[i]
  filename = paste0("Survival_SingleYear_", siteName, ".pdf")
  
  pdf(file = paste0(figureDir, filename))
  for(layer in 1:length(survivalYears)){ 
    print(layer)
    plot(mpbSurvivalStacks[[i]][[layer]], axes = F, box = F, zlim = c(0, 1)); plot(shapeSites[[i]], add = T, lwd = 0.4)
    mainTitle = paste0(siteName, " winter ", survivalYears[layer] - 1, "/", survivalYears[layer])
    mtext(mainTitle, side = 3)
  }
  dev.off()
}


# Multi-year average survival for study sites -----------------------------

survivalYears = 1981:2017
nYears = 5
i = 2

layer = nYears

for(nYears in c(5, 8, 12)){
  for(i in 1:5){
    
    siteName = names(extentSites)[i]
    filename = paste0("Survival_Average_", nYears, "_years_", siteName, ".pdf")
    
    
    pdf(file = paste0(figureDir, filename))
    for(layer in (nYears + 1):length(survivalYears)){ 
      print(layer)
      plot(mean(subset(mpbSurvivalStacks[[i]], (layer - nYears + 1):layer)), axes = F, box = F, zlim = c(0, 1)); plot(shapeSites[[i]], add = T, lwd = 0.4)
      mainTitle = paste0(siteName, " mean survival\nfall ", survivalYears[layer - nYears], " - spring ",survivalYears[layer])
      mtext(mainTitle, side = 3)
    }
    dev.off()
  }
}



 