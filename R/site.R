# If you are running this outside of the Makefile, you should be
# one directory below the R directory.

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
tab = xtable(as.data.frame(with(d, table(forest)), responseName="number of sites"),
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

library(plyr)
f = ddply(d2, "site", summarise, number=length(year))
f = join(f, d[,c("site","forest")], by = "site")
ggplot(na.omit(f), aes(x=number))+geom_histogram()+facet_wrap(~forest)





quit(ifelse(interactive(), "ask", "no"))

