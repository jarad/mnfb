# Image of broad2 across years, all forests combined
# I confess that I don't find this image very informative... I need to think of some better formulations...

require(ggplot2)
require(plyr)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Function to retrieve the first & last broad2 type as well as the number of broad2 changes
# flc() is used by sort.broad()
flc <- function(data){
  last <- first <- changes <- changeyr <- numeric(dim(data)[1])
  for (i in 1:(dim(data)[1])) {
    y <- data[i,][!is.na(data[i,])]
    last[i] <- y[length(y)]
    first[i] <- y[2]
    changes[i] <- sum(diff(y[2:length(y)]) != 0)
    h <- match(TRUE, diff(y[2:length(y)]) != 0) + 2
    if(!is.na(h)) {
      changeyr[i] <- as.numeric(names(data)[match(y[h], data[i,])])
    } else {changeyr[i] <- NA}
  }
  return(list(last, first, changes, changeyr))
}

# Function to perform necessary data manipulations (not a very informative comment, is it?)
sort.broad <- function(data, fornum){
  require(reshape2)
  brd <- dcast(data[data$forest == fornum,], site ~ year, fun = sum, value.var="broad2")
  brd[brd == 0 | brd == 3] <- NA; brd$site[is.na(brd$site)] <- 3     # Treat 'not used' as NA
  brd[,c("first", "last", "changes", "changeyr")] <- flc(brd)
  brd <- subset(brd, !is.na(brd$first))         # Get rid of sites that never had a broad2 classification
  sort.brd <- brd[order(brd$first, brd$last),]  # Sort table by first/last broad2 type
  return(sort.brd)
}




# Read and compile data
d.site <- read.csv("data/site.csv")
d.fstype <- read.csv("data/forest_type.csv")
d.loc <- read.csv("data/location.csv")
d.log <- read.csv("data/logged_sites.csv")
d.log$year[d.log$year=="2001?"] <- 2001    # One odd data entry point
d.log$year <- factor(d.log$year)           # Number of logging events per year

t <- merge(d.site, d.fstype, by.x = "fstype", by.y = "fstype")
t <- merge(t, d.loc, by.x = "site", by.y = "site")[, c("fstype", "site", "year", "fstypename", "broad2", "forest", "standunique")]
t <- t[t$forest %in% c(9020, 9030, 9090),]


 
# The table below -- t.ply -- lists the number of different fstypes and broad2 types for each stand-year combination.  In the end, 313 of 9068 stand-years have multiple broad2's.  2027 have multiple fstypes (narrowly defined).  Though not presented here, it's kind of fun to note that the number of multi-habitat stands increases over time... which is what you'd predict as forest composition slowly changes.

t.ply <- ddply(t, .(standunique, year), summarize, num.fstype = length(unique(fstype)), num.broad2 = length(unique(broad2)))




# Treat the factors as numeric to facilitate the use of dcast
t$fstype <- as.numeric(as.character(t$fstype))
t$broad2 <- as.numeric(t$broad2)

# Example analysis of Broad2 using forest 9030:

t.9030 <- sort.broad(t, 9030)
image(as.matrix(t.9030[,2:(match("first", names(t.9030))-1)] ), col = cbbPalette) # X-axis: Sites;   Y-axis: Years

# Same plot with only sites that have changed:
image(as.matrix(t.9030[t.9030$changes > 0,2:(match("first", names(t.9030))-1)] ), col = cbbPalette) # X-axis: Sites;   Y-axis: Years

# Same plot with only sites that have changed twice:
# I find it interesting that all two-change sites return to home base.  But there's only four sites.
image(as.matrix(t.9030[t.9030$changes > 1,2:(match("first", names(t.9030))-1)] ), col = cbbPalette) # X-axis: Sites;   Y-axis: Years



brd.m <- melt(t.9030, id.vars = 1, measure.vars = 2:(match("changes", names(t.9030))-1))
qplot(variable, geom="bar", fill=factor(value), data=brd.m, main = "Sites by Broad2 Each Year") + scale_fill_manual(values=cbbPalette)
table(t.9030$first, t.9030$last)



# Logging
# Note: some events in 'logging' are actually blowdowns (or a beaver dam)


log.events <- dcast(d.log[d.log$forest == 9030,], Site ~ year, fun = length, value.var="logged")

t.9030.log <- t.9030
d.log.9030 <- d.log[d.log$forest == 9030,]
for (i in 1:dim(d.log.9030)[1]){
  t.9030.log[t.9030.log$site == d.log.9030$Site[i], as.character(d.log.9030$year[i])] <- -1
}

# This image shows that logging events do not change a site's broad2
image(as.matrix(t.9030.log[,2:(match("first", names(t.9030.log))-1)]), col = cbbPalette) # X-axis: Sites;   Y-axis: Years



