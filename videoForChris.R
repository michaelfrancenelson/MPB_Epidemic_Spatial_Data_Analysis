
require(animation)
dat = fread("/Eclipse/NeonWorkspace2/ForestGapModels/output/test.log", header = T)
dat = read.csv("/Eclipse/NeonWorkspace2/ForestGapModels/output/test.log", header = T)
names(dat)



zip = function(pine, fir, line, length = 8){
  output = vector(mode = "numeric", length = length)
  x = unlist(pine[line, 3:10])
  y = unlist(fir[line, 3:10])
  for (i in 1:length(x)){
    output[(i - 1) * 2 + 1] = x[i]
    output[(i - 1) * 2 + 2] = y[i]
  }
  return(output)
}

pine = subset(dat, species == "PIPO")
fir = subset(dat, species == "PSME")

zip(pine, fir, 1)


i = 100

col.pine = rgb(0.05, 1, 0.3, alpha = 0.8)
col.fir = rgb(0.7, 0.2, 0.7, alpha = 0.6)
ylim = c(0, max(fir[, 3:10]))
max(pine[, 3:10])
asp = 1.4
height = 1400
width = asp * height
cex1 = 3.5
cex2 = 4.5
cex3 = 8
line1 = 5
nYears = nrow(pine)
ani.options(interval = 0.04, ani.width = width, ani.height = height, nmax = nYears)
nYears = 2000
saveVideo(
  expr = 
  {
    layout(matrix(1:2, ncol = 1))
    mar = par("mar")
    oma = par("oma")
    oma[3] = 7
    mar[2] = 9; mar[3] = 1.8
    par(mar = mar)
    par(oma = oma)
    for(i in 1:nYears) {
      barplot(unlist(pine[i, 3:10]), col = col.pine, xlab = "",
              names.arg = "", ylim = ylim, cex.axis = cex1, cex.names = cex2)
      mtext(side = 2, text = "PIPO basal area", cex = cex2, line = line1)
      barplot(unlist(fir[i, 3:10]), col = col.fir, ylim = ylim, 
              names.arg = "", ylab = "", cex.axis = cex1, cex.names = cex2)
      mtext(side = 2, text = "PSME basal area", cex = cex2, line = line1)
      mtext(side = 3, outer = T, text = paste0("year ", sprintf(fmt = "%4d", i)), cex = cex3)
      print(i)
    }
  },
  other.opts = "-c:v mpeg4 -preset slow -crf 15 -fs 10000000"
)

tail(fir)
tail(pine)

max(fir[, 3:10])

plot(c(pine[i, 3:10]))






barplot(height = )



x = 1:120

x = 55
y = 35



r1 = function(x1, y1, w = 16.5, h = 13.5){
  for(i in 1:length(x1)){
    x = x1[i]; y = y1[i]
    polygon(x = c(x - 0.5 * w, x - 0.5 * w, x + 0.5 * w, x + 0.5 * w, x - 0.5 * w),
            y = c(y - 0.5 * h, y + 0.5 * h, y + 0.5 * h, y - 0.5 * h, y - 0.5 * h))
  }
}
xlim = c(0, 144);  ylim = c(0, 96)
r1(pts[c(2, 4, 6, 8) - 0], rep(61, 4))

nPts = 9
pts = seq(1, 144, length.out = nPts)


sp0 = 144 - 16.5 * 4
sp0
plot(1, 1, xlim = xlim, ylim = ylim, type = "n", axes = F, ann = F); abline(h = c(0, 96)); abline(v = c(0, 144))
sp1 = sp0 / 5 + 16.5 / 2
sp2 = sp0 / 5 + 16.5
cs = cumsum(c(sp1, sp2, sp2, sp2))
r1(cs, rep(61, 4))
r1(sp * 1:4, rep(61, 4))



nFrames = 2
wFrame = 12.5
wWall = 49.5


spacing = function(nFrames, wWall, wFrame){
  # how much empty wall space with frames present?
  sp0 = wWall - wFrame * nFrames
  nSpaces = nFrames + 1
  spBetween = sp0 / nSpaces  

  spCenter = wFrame + spBetween  
  sp1 = spBetween + wFrame * 0.5
  sp2 = wFrame + spBetween
  pos = cumsum(c(sp1, rep(sp2, nFrames - 1)))
  
  return(list(pos = pos, spBetween = spBetween))  
}

sp = spacing(nFrames = 2, wWall = 49.5, wFrame = 12.5)

h1 = 70

plot(1, xlim = c(0, wWall), ylim = c(0, 96), asp = 1, type = "n", ann = F, axes = F)
abline(v = c(0, wWall)); abline(h = c(0, 96))
r1(sp$pos, rep(h1, length(sp$pos)), w = 12.5, h = 12.5)
r1(sp$pos, rep(h1 - (sp$pos[2] - sp$pos[1]), 2), w = 12.5, h = 12.5)

sp



