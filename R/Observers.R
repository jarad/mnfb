d.site <- read.csv("data/site.csv")

# Plots which observers were observing in which years
pdf("figs/Observers-obsyear.pdf", width=3.25, height=6)
image(x = sort(unique(d.site$year)), 
      y = sort(unique(d.site$obs)),
      z = (table(d.site$year, d.site$obs)>0)*99, xlab = "Year", ylab = "Observer Number", col = c("midnightblue", "white"))
dev.off()