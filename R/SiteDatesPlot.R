# Should we consider any of these observations to be outliers?

require(ggplot2)
require(lubridate)
require(plyr)

source("R/common/functions.R")

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

# Set colors
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# jd by forest by year
pdf(fig_dir("SiteDatesPlot-byForestbyYear.pdf"), width=6.5, height=4.5)
qplot(jd, geom="histogram", data = t[t$year > 1994.5 & t$year < 2010.5,], binwidth=1, fill=forest, facets = ~year, xlab = "Julian Date") + scale_fill_manual(values=cbbPalette)
dev.off()



