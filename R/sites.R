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
print(tab, file="duplicated-sites.tex", include.rownames=FALSE)
rm(dups)

# Make a map of the sites
# This should be replaced with a real map using maptools or something similar
library(ggplot2)
qplot(x=X, y=Y, data=sites, color=forest)


q("no")

