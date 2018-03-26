source("LoadData.R")
source("PlottingFunctions.R")

ls()


# Create bounding boxes around the study sites

plot(studySites)

locator(2)
# $x
# [1] -1354382 -1145460
# $y
# [1] 889246.4 689820.8

ColvilleExtent = extent(x = c(-1354382,  -1145460), y = c(689820, 889246))
locator(2)

BlackHillsExtent = extent(x = c(-395239, -210058), y = c(271976, 963054))

