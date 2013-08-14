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

# This is the only way I can figure to have no decimal places in the output... feel free to improve this
for(j in 2:4){
  isol.tab[,j] <- as.integer(isol.tab[,j])
}

tab = xtable(isol.tab[order(isol.tab$Forest, isol.tab$Year, isol.tab$Julian.Date),],
             caption = "Isolated Observations",
             label = "tab:isolated-observations")
print(tab, file=tab_dir("SiteDatesPlot-isolated-observations.tex"), include.rownames=FALSE)