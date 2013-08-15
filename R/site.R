# If you are running this outside of the Makefile, you should be
# one directory below the R directory.


library(plyr)
library(ggplot2)

source("R/common/functions.R")

d = read.csv("data/location.csv")
d$forest = factor(d$forest)

# Only use Chequamegon, Chippewa, and Superior national forests
d = d[d$forest %in% c(9020, 9030, 9090),]

###########################
# Checking for duplicate or missing locations
dups = d[which( duplicated(d$X) & duplicated(d$Y) ),]
dups = dups[order(dups$forest, dups$site),]


library(xtable)
#tab = xtable(dups[,c("forest","site","X_COORD","Y_COORD")], 
#             caption="Number of sites with duplicated or missing coordinates",
#             label="tab:duplicated-sites")
tab = xtable(data.frame("Number"=nrow(dups)), 
            caption="Number of sites with duplicated or missing coordinates",
             label="tab:duplicated-sites")

print(tab, file=tab_dir("site-duplicated.tex"), include.rownames=FALSE)
rm(dups)

############################
# Number of sites per forest
d$forest <- refactor_forests(d$forest)
tab = xtable(as.data.frame(with(d, table(forest)), responseName="Number of Sites"),
             caption="Number of sites in each national forest",
             label="tab:number-of-sites-in-forest")
print(tab, file=tab_dir("site-number-per-forest.tex"), include.rownames=FALSE)

############################
# Make a map of the sites
# This should be replaced with a real map using maptools or something similar
library(ggplot2)
pdf(fig_dir("site-map.pdf"), width=6, height=4)
qplot(x=X_COORD, y=Y_COORD, data=d, color=forest)
dev.off()


############################
# Sites 
d2 = read.csv("data/site.csv")
d2 = join(d2, d[,c("site","forest")], by = "site")
d2 = d2[-which(is.na(d2$forest)),] # Why are there NA forests?

f = ddply(d2, "site", summarise, number=length(year), forest=unique(forest))
#ggplot(na.omit(f), aes(x=number))+geom_histogram(binwidth=1)+facet_wrap(~forest)


# Image of which sites were surveyed in what years
siteyear = function(d) {
  sy = unique(d[,c("site","year")])
  sy$site = as.factor(sy$site)
  sy$year = as.factor(sy$year)

  nsites = nlevels(sy$site)
  nyears = nlevels(sy$year)

  surveyed = matrix(0, nsites, nyears)
  for (i in 1:nyears) {
    sv = as.numeric(sy$site[sy$year==levels(sy$year)[i]])
    surveyed[sv,i] = 1
  }
  colnames(surveyed) = levels(sy$year)
  rownames(surveyed) = levels(sy$site)
  
  return(surveyed)
}

sy = dlply(d2, "forest", siteyear)

for(i in 1:length(sy)){ attr(sy[[i]],"name") <- names(sy)[i] }


site_image = function(x) {
  n = rowSums(x)
  image(as.numeric(colnames(x)), 1:length(n), t(x[order(n),]), 
        ylab="Site", xlab="Year",
        main=attr(x, "name"), col=c("white","black"))
}

pdf("figs/site-siteyear.pdf", width=6, height=4)
par(mfrow=c(1,3), mar=c(4,4,2,0)+.1)
l_ply(sy, site_image)
dev.off()




quit(ifelse(interactive(), "ask", "no"))

