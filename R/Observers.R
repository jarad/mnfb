require(plyr)
require(ggplot2)
require(xtable)

source("R/common/functions.R")

############################
# Image plot of the years during which observers were in the field.  Sorted by observer number.

d.site <- read.csv("data/site.csv")
d.loc <- read.csv("data/location.csv")

pdf("figs/Observers-obsyear.pdf", width=3.25, height=6)
image(x = sort(unique(d.site$year)), 
      y = sort(unique(d.site$obs)),
      z = (table(d.site$year, d.site$obs)>0)*99, xlab = "Year", ylab = "Observer Number", col = c("gray87", "black"))
dev.off()


############################
# Scatterplots for two observers who might have data entry errors

d.site <- join(d.site, d.loc, by = "site")
d.site$instudy <- (d.site$forest %in% c(9020, 9030, 9090))

obyr <- ddply(d.site, .(obs, year), summarize, n.obs = length(obs))
obstat <- ddply(d.site, .(obs), summarize, fy = min(year), ly = max(year), ny = length(unique(year)), nobs = length(obs))
obtab <- merge(obyr, obstat, by="obs")

obsbynumyr <- ddply(obstat, .(ny), summarize, Number.of.Observers = length(ny), Total.Observations = sum(nobs), Percent.of.Total.Observations = round(100*sum(nobs)/dim(d.site)[1],1))
names(obsbynumyr)[1] <- "Number.of.Field.Seasons"
tab = xtable(obsbynumyr, caption = "Summaries by Number of Field Seasons Worked by an Observer",
             label = "tab:field-seasons")
print(tab, file=tab_dir("Observers-field-seasons.tex"), include.rownames=FALSE)

d.site$jd <- jdate(d.site)
qplot(year, jd, geom="point", data=d.site, facets = ~obs, color=instudy)
qplot(year, jd, geom="point", data=d.site[d.site$year > 1994.5,], facets = ~obs, color=instudy, ylab="Julian Date") + scale_colour_manual(values=c("gray60", "black"), name = "Forest of Interest?", labels=c("No", "Yes"))


# There are 18 NA's!  All in 2008
# Observers 10, 20, 75, and 88 work for just a few days.  Likewise, observers 1, 8, 10 have some very short seasons.  4 & 8 have huge gaps in time.  I can actually attach names to these people!
# Cross-tab the above against the site-date abnormalities


# In the following plot, TRUE <=> first year of observation; FALSE <=> second or later year
Obsmult <- unique(Obsmin$obs[Obsmin$firstyr == FALSE])
Obsmin2 <- subset(Obsmin, obs %in% Obsmult)
Obsminply2 <- ddply(Obsmin2, .(obs, juliandate, firstyr), summarize, avg=mean(N))
# This is a plot of just the multi-year observers
qplot(juliandate, avg, geom="point", data=Obsminply2, facets = ~obs, color = firstyr, size = I(2), main="Multi-Year Observers (TRUE = 1st Year Observing)")
