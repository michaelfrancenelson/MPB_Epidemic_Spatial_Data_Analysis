require(spgwr)
require(maptools)
require(ggplot2)

# tree kill and single year survival --------------------------------------

# predictor: mpb modeled survival in winter preceding the tree kill 
# (dates for tree data are when red stage observed)

str(stackSitesContorta, 1)
str(mpbSurvivalStacks, 1)

siteNames = names(stackSitesContorta)


site = 1
killYear = 2005
survivalYear = 2003
nPts = 1000

kill = subset(stackSitesContorta[[site]] + stackSitesPonderosa[[site]], paste0("tree_kill_", killYear))
kill = subset(stackSitesContortaMasked[[site]] + stackSitesPonderosaMasked[[site]], paste0("tree_kill_", killYear))
survival = subset(mpbSurvivalStacks[[site]], paste0("X", survivalYear))

killSPDF = rasterToPoints(kill, spatial = T)
survivalSPDF = rasterToPoints(survival, spatial = T)

head(killSPDF@data)
head(survivalSPDF@data)

proj4string(killSPDF) == proj4string(survivalSPDF)

dim(killSPDF)
dim(survivalSPDF)

indices = nabor::knn(survivalSPDF@coords, killSPDF@coords, k = 1)
names(killSPDF) = "kill"
killSPDF$survival = survivalSPDF@data[indices$nn.idx, ]
head(killSPDF@data)

ddd = killSPDF[sample(1:nrow(killSPDF), nPts), ]
GWRbandwidth = gwr.sel(kill ~ survival, data = ddd, adapt = T)

gwr.model = gwr(kill ~ survival, data = ddd, adapt = GWRbandwidth, se.fit = T, hatmatrix = T)
gwr.model

results<- as.data.frame(gwr.model$SDF)
ddd$coef = results$pred
ddd$se = results$pred.se
ddd$sigTest = abs(ddd$kill) - 2 * ddd$se

head(ddd@data)

min(ddd$coef, na.rm = T)
summary(ddd@data)
gwr.point1<-ggplot(ddd@data, aes(x=coordinates(ddd)[, 1],y=coordinates(ddd)[, 2]))+
  geom_point(aes(colour = ddd$coef))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50",
                         guide = "colourbar", guide_legend(title="Coefs"))
gwr.point1_sigTest<-ggplot(ddd@data, aes(x=coor
                                         dinates(ddd)[, 1],y=coordinates(ddd)[, 2]))+
  geom_point(aes(colour = ddd$sigTest))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         midpoint = 0, space = "rgb", na.value = "grey50",
                         guide = "colourbar", guide_legend(title="Sig. test"))
gwr.point1 + geom_path(data = shapeSites[[site]], aes(long, lat, group = id))
gwr.point1_sigTest

summary(ddd@data)


coordinates(ddd)
ddd$x = 

gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()



plot(killSPDF)

gwr.point <- ggplot(shapeSites[[site]])
gwr.point

plot(shapeSites[[site]])


print(GWRbandwidth)
class(GWRbandwidth)

