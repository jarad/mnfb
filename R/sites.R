# If you are running this outside of the Makefile, you should be
# one directory below the R directory.

source("R/common/functions.R")

d = read.csv("data/mnfb.csv")

# Create a smaller data frame with only sites and their locations
sites = unique(d[,c(1,3:4,6)])
rm(d)
names(sites) = gsub(".","",names(sites), fixed=TRUE)
sites$forest = refactor_forests(sites$forest)

# Checking for duplicate locations
dups = which( duplicated(sites$X) & duplicated(sites$Y) )
sites[dups,]

library(xtable)
tab = xtable(sites[dups,], 
             caption="Sites with duplicated or missing locations",
             label="tab:duplicated-sites")
print(tab, file=tab_dir("duplicated-sites.tex"), include.rownames=FALSE)
rm(dups)


# Number of sites per forest
tab = xtable(as.data.frame(with(sites, table(forest)), responseName="number of sites"),
             caption="Number of sites in each national forest",
             label="tab:number-of-sites-in-forest")
print(tab, file=tab_dir("number-of-sites-in-forest.tex"), include.rownames=FALSE)


# Make a map of the sites
# This should be replaced with a real map using maptools or something similar
library(ggplot2)
pdf(fig_dir("site-map.pdf"), width=6, height=4)
qplot(x=X_COORD, y=Y_COORD, data=sites, color=forest)
dev.off()


q("no")

