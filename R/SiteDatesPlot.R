require(ggplot2)
require(lubridate)
require(plyr)
require(xtable)

source("R/common/functions.R")

############################
# Scatterplot: Julian dates for observations by year (faceted by forest)

# Read data
d.loc <- read.csv("data/location.csv")
d.site <- read.csv("data/site.csv")

# Calculate Julian Dates
POSIXdate <- as.POSIXct(as.character(d.site$date), format = "%m/%d/%Y %H:%M:%S")
jan1 <- as.Date(paste("1/1/", year(POSIXdate), sep=""), format = "%m/%d/%Y")
d.site$jd <- as.numeric(floor(difftime(POSIXdate, jan1, units = "days"))) + 1

# Join location and observation data
t <- join(d.site, d.loc, by = "site")
t <- t[t$forest %in% c(9020, 9030, 9090),]
t$forest <- refactor_forests(t$forest)

# Final plot
pdf(fig_dir("SiteDatesPlot-byForestbyYear.pdf"), width=6.5, height=4.5)
qplot(year, jd, geom="point", data = t[t$year > 1994.5,], binwidth=1, facets = ~forest, xlab = "Year", ylab = "Julian Date", size = I(2))
dev.off()


############################
# Table: Observation dates that are isolated and may be in error

# List of observations that are isolated in time from others in that forest-year:
isol.obs <- c("Chequamegon 1995 140",
              "Chequamegon 2010 189",
              "Chippewa 2004 171",
              "Chippewa 2010 157",
              "Chippewa 2010 174",
              "Chippewa 2010 192",
              "Chippewa 2012 167",
              "Superior 2010 180")

isol.tab <- t[paste(t$forest, t$year, t$jd) %in% isol.obs,][,c("forest", "site", "year", "jd")]
names(isol.tab) <- c("Forest", "Site", "Year", "Julian.Date")

# This is the only way I can figure to have no decimal places in the output... feel free to improve upon this
for(j in 2:4){
  isol.tab[,j] <- as.integer(isol.tab[,j])
}

tab = xtable(isol.tab[order(isol.tab$Forest, isol.tab$Year, isol.tab$Julian.Date),],
             caption = "Isolated Observations",
             label = "tab:isolated-observations")
print(tab, file=tab_dir("SiteDatesPlot-isolated-observations.tex"), include.rownames=FALSE)


############################
# Figures: Maps for each forest showing the order of site sampling by year

# ordval: A variable to store the order in which sites are sampled for a specific forest-year
# - by using rank(), we focus on the order and not the spacing of observations
# - by using scale(), we guarantee that all years plot the same range of colors
t$ordval <- NA
for(j in c("Chequamegon", "Chippewa", "Superior")){
  for (i in 1:22){
    t$ordval[t$year == 1994 + i & t$forest == j] <- scale(rank(t$jd[t$year == 1994 + i & t$forest == j]))
  }
}

# Faceted plot for Chequamegon
pdf(fig_dir("SiteDatesPlot-orderwithinCheq.pdf"), width=6.5, height=4.5)
qplot(X_COORD, Y_COORD, geom="point", data=t[t$year > 1994.5 & t$forest=="Chequamegon" & is.na(t$X_COORD)==FALSE,], color=ordval, facets = ~year, size=I(3), main="Order of Sampling at Chequamegon Forest", xlab="Longitude", ylab="Latitude") + scale_colour_gradient2(low="red3", mid="white", high="navy", name = "Sampling Order", breaks = c(-1.5, 1.5), labels = c("Earliest", "Latest")) + theme(panel.background = element_rect(fill='lightgoldenrod1'), axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()

# Faceted plot for Chippewa
pdf(fig_dir("SiteDatesPlot-orderwithinChip.pdf"), width=6.5, height=4.5)
qplot(X_COORD, Y_COORD, geom="point", data=t[t$year > 1994.5 & t$forest=="Chippewa" & is.na(t$X_COORD)==FALSE,], color=ordval, facets = ~year, size=I(3), main="Order of Sampling at Chippewa Forest", xlab="Longitude", ylab="Latitude") + scale_colour_gradient2(low="red3", mid="white", high="navy", name = "Sampling Order", breaks = c(-1.5, 1.5), labels = c("Earliest", "Latest")) + theme(panel.background = element_rect(fill='lightgoldenrod1'), axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()

# Faceted plot for Superior
pdf(fig_dir("SiteDatesPlot-orderwithinSup.pdf"), width=6.5, height=4.5)
qplot(X_COORD, Y_COORD, geom="point", data=t[t$year > 1994.5 & t$forest=="Superior" & is.na(t$X_COORD)==FALSE,], color=ordval, facets = ~year, size=I(3), main="Order of Sampling at Superior Forest", xlab="Longitude", ylab="Latitude") + scale_colour_gradient2(low="red3", mid="white", high="navy", name = "Sampling Order", breaks = c(-1.5, 1.5), labels = c("Earliest", "Latest")) + theme(panel.background = element_rect(fill='lightgoldenrod1'), axis.text.x = element_blank(), axis.text.y = element_blank())
dev.off()