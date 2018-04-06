
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
height = 1800
width = asp * height
cex1 = 3.5
cex2 = 4.5
cex3 = 8
line1 = 5
ani.options(interval = 0.08, ani.width = width, ani.height = height, nmax = nYears, )
nYears = nrow(pine)
nYears = 100
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
  other.opts = "-c:v libx264 -preset slow -crf 10 -fs 10000000"
)

tail(fir)
tail(pine)

max(fir[, 3:10])

plot(c(pine[i, 3:10]))






barplot(height = )

