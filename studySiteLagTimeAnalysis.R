require(car)
require(MASS)
source("helperFunctions.R")
sourceSection(filename = "LoadData.R", sectionNames = c("Data paths", "Load small data"))


# Regressions multi-year survival tree kill aggregate ---------------------
ls()
siteNames = c("Beaverhead", "BlackHills", "Colorado", "Colville", "WesternUS" )
site = 5
nYears = 12



df1 = killMeanSurvivalSubset(treeKill_multYearMeanMpbSurvival, siteNames[site], nYears)
lm1 = lm(kill ~ I(survival), data = df1)
lm1a = lm(log(kill) ~ I(survival), data = df1)
lm2 = lm(kill ~ I(survival) + I(survival^2), data = df1)
lm3 = lm(kill_nonzero ~ I(survival), data = df1)
lm3a = lm(log(kill_nonzero) ~ I(survival), data = df1)
lm4 = lm(kill_nonzero ~ I(survival) + I(survival^2), data = df1)


an1 = anova(lm1)
an1

sum(df1$kill^2)
sum(lm1$residuals^2)



sum((df1$kill - mean(df1$kill))^2)
202.71 + 159.85

str(lm1, 0)

summary(lm1)
summary(lm1a)

summary(lm2)
summary(lm3)
summary(lm3a)
summary(lm4)


outlierTest(lm1)
outlierTest(lm2)

qqPlot(lm1)
qqPlot(lm1a)
qqPlot(lm2)
qqPlot(lm3)
qqPlot(lm3a)
qqPlot(lm4)

ncvTest(lm1)
ncvTest(lm1a)
ncvTest(lm2)
ncvTest(lm3)
ncvTest(lm3a)
ncvTest(lm4)

avPlots(lm1)
avPlots(lm1a)
avPlots(lm2)
avPlots(lm3)
avPlots(lm3a)
avPlots(lm4)

crPlots(lm1)
crPlots(lm1a)
crPlots(lm2)
crPlots(lm3)
crPlots(lm3a)
crPlots(lm4)

durbinWatsonTest(lm1)
durbinWatsonTest(lm1a)
durbinWatsonTest(lm2)

anova(lm1, lm2)
anova(lm3, lm4)


residualPlot(lm1)
residualPlot(lm1a)
residualPlot(lm2)
residualPlot(lm3)
residualPlot(lm3a)
residualPlot(lm4)

plot(lm1, which = 1)

dev.off()
x = seq(0.5, 0.7, length.out = 100)

plot(kill ~ survival, data = df1)
points(x, predict(lm2, newdata = data.frame(survival = x)), type = "l")


plot(log(kill) ~ survival, data = df1)
points(x, predict(lm1a, newdata = data.frame(survival = x)), type = "l")

abline(lm1)

summary(lm1)
summary(lm2)

par(mfrow = c(2, 2)); plot(lm2, which = 1:4)
par(mfrow = c(2, 2)); plot(lm1, which = 1:4)


treeKill_multYearMeanMpbSurvival


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


