# If you are running this outside of the Makefile, you should be
# one directory below the R directory.


library(plyr)
library(ggplot2)

source("R/common/functions.R")

d.loc = read.csv("data/location.csv")
d.loc$forest = factor(d.loc$forest)

# Only use Chequamegon, Chippewa, and Superior national forests
d = d.loc[d.loc$forest %in% c(9020, 9030, 9090),]

###########################
# Checking for duplicate or missing locations

d$xy <- paste(d$X_COORD, d$Y_COORD)
xy <- length(d$xy[d$xy %in% d$xy[duplicated(d$xy)]])

# dups = d[which( duplicated(d$X) & duplicated(d$Y) ),]
# dups = dups[order(dups$forest, dups$site),]

library(xtable)
tab = xtable(data.frame("Number"=xy), 
            caption="Number of sites with duplicated or missing coordinates.  They turn out all to be missing.",
            label="tab:site-duplicated")

print(tab, file=tab_dir("site-duplicated.tex"), include.rownames=FALSE)
rm(dups)

############################
# Number of sites per forest
d$forest <- refactor_forests(d$forest)

stands.sites <- ddply(d, .(forest), summarize, stands = length(unique(standunique)), sites = length(unique(site)))
names(stands.sites) <- c("Forest", "Stands", "Sites")

tab = xtable(stands.sites,
             caption="Total number of stands and sites in each national forest (1991-2012)",
             label="tab:stands-sites-per-forest")
print(tab, file=tab_dir("stands-sites-per-forest.tex"), include.rownames=FALSE)

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
d2 = join(d2, d.loc[,c("site","forest")], by = "site")
d2 = d2[d2$forest %in% c(9020, 9030, 9090),]

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

############################
# Counts without Observation Conditions and 
# Site-Years with All-zero Counts

d.bird <- read.csv("data/bird.csv")
d.bird = join(d.bird, d.loc[,c("site","forest")], by = "site")
d.bird = d.bird[d.bird$forest %in% c(9020, 9030, 9090),]

nonzero.sites <- unique(paste(d.bird$site, d.bird$year))
sites.wo.cond <- unique(paste(d2$site, d2$year)) 

b <- c(nonzero.sites[!(nonzero.sites %in% sites.wo.cond)], sites.wo.cond[!(sites.wo.cond %in% nonzero.sites)])
miss.dat <- data.frame(do.call('rbind', strsplit(as.character(b),' ',fixed=TRUE)))
miss.dat[,3] <- c("Counts without observation conditions.", rep("Site-year without any birds counted.",3))
colnames(miss.dat) <- c("Site", "Year", "Issue")

tab = xtable(miss.dat,
             caption="Counts without observation conditions and zero-count site-years",
             label="tab:site-missing-data")
print(tab, file=tab_dir("site-missing-data.tex"), include.rownames=FALSE)


quit(ifelse(interactive(), "ask", "no"))

