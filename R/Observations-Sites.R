d.bird <- read.csv("data/bird.csv")
d.bird$siteyear <- paste(d.bird$site, d.bird$year)
d.site <- read.csv("data/site.csv")
d.site$siteyear <- paste(d.site$site, d.site$year)

bsy <- unique(d.bird$siteyear)
ssy <- unique(d.site$siteyear)

bsy[-which(bsy %in% ssy)]   # Observations without site data
# "1012 2010"
ssy[-which(ssy %in% bsy)]   # Sites without observation data
# "686 1992"  "289 2008"  "1655 2008"
