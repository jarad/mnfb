require(ggplot2)
require(plyr)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###########################
# Functions

# Function to retrieve the first and last habitat types, as well as the number of habitat changes and the year of the first habitat change.
# flcc() requires a dataset that has been dcast.  It is called by sort.habt() and is not really meant to be run on its own.
flcc <- function(data){
  last <- first <- changes <- changeyr <- numeric(dim(data)[1])
  for (i in 1:(dim(data)[1])) {
    y <- data[i,][!is.na(data[i,])]
    last[i] <- y[length(y)]
    first[i] <- y[2]
    changes[i] <- sum(diff(y[2:length(y)]) != 0)
    h <- match(TRUE, diff(y[2:length(y)]) != 0) + 2
    if(!is.na(h)) {
      changeyr[i] <- as.numeric(names(data)[match(y[h], data[i,2:dim(data)[2]])]) + 1
      } else {changeyr[i] <- NA}
  }
  return(list(first, last, changes, changeyr))
}

# Function to convert a table of habitat types by site-year into a matrix of habitat type by site and year (plus extra columns output by the flcc() function)
sort.habt <- function(data, fornum, habitat){
  require(reshape2)
  hbt.lev <- levels(data[,match(habitat, names(data))])
  data[,match(habitat, names(data))] <- as.numeric(data[,match(habitat, names(data))])  # Convert habt type to numeric for casting purposes
  hindex <- match(habitat, c("broad2",  "fstypename", "fine2", "fstype"))
  h.notused <- list(c(0, 3), c(0, 22), c(0, 1, 10), c(0, 1, 2))   # Look up 'not used' or blank factor levels by habitat type
  hbt <- dcast(data[data$forest %in% fornum,], site ~ year, fun = sum, value.var = habitat)

  # Replace not-used and blanks with NA  (I haven't been able to figure out how to do this without a for loop when multiple factor levels must be replaced)
  for (k in 1:length(h.notused[[hindex]])) {
    hbt[,2:dim(hbt)[2]][hbt[,2:dim(hbt)[2]] == h.notused[[hindex]][k]] <- NA}

  hbt[,c("first", "last", "changes", "changeyr")] <- flcc(hbt)
  hbt <- subset(hbt, !is.na(hbt$first))         # Get rid of sites that never had a habitat classification
  sort.hbt <- hbt[order(hbt$first, hbt$last, hbt$changeyr),]  # Sort table by first/last/changeyr habitat type
  attr(sort.hbt, "hab.type") <- habitat
  attr(sort.hbt, "hab.levels") <- hbt.lev
  return(sort.hbt)
}


###########################
# Read and compile data

d.site <- read.csv("data/site.csv")
d.fstype <- read.csv("data/forest_type.csv")
d.loc <- read.csv("data/location.csv")
d.log <- read.csv("data/logged_sites.csv")
d.log$year[d.log$year=="2001?"] <- 2001    # One odd data entry point
d.log$year <- factor(d.log$year)           # Number of logging events per year

t <- merge(d.site, d.fstype, by.x = "fstype", by.y = "fstype")
t <- merge(t, d.loc, by.x = "site", by.y = "site")[, c("fstype", "site", "year", "fstypename", "fine2", "broad2", "forest", "standunique")]
t <- t[t$forest %in% c(9020, 9030, 9090),]


###########################
# Patterns in Habitat Type Change

# Consistency within a stand
# The table below -- t.ply -- lists the number of different habitat types for each stand-year combination.  

t.ply <- ddply(t, .(standunique, year, forest), summarize, num.fstypename = length(unique(fstypename)), num.fstype = length(unique(fstype)), num.fine2 = length(unique(fine2)), num.broad2 = length(unique(broad2)))

# In the end, out of 9068 stand-years, 
# 1389 have multiple fstypenames,
# 2027 have multiple fstypes (which is fstypename + stockdens)
# 1351 have multiple fine2's, and
# 1291 have multiple broad2's
# Though not presented here, it's kind of fun to note that the number of multi-habitat stands increases over time... which is what you'd predict as forest composition slowly changes.
c("stands" = dim(t.ply)[1],
  "multi-fstypename" = dim(t.ply[t.ply$num.fstypename > 1,])[1],
  "multi-fstype" = dim(t.ply[t.ply$num.fstype > 1,])[1],
  "multi-fine2" = dim(t.ply[t.ply$num.fine2 > 1,])[1],
  "multi-broad2" = dim(t.ply[t.ply$num.broad2 > 1,])[1])


# Frequency of habitat type changes by site
# NO HABITAT CHANGES SINCE 2007!!!!!!!!  (or before 1992)
# Note: technically, site 12 is an exception; it changes in 2008, but 'changeyr' is the *first* change, and site 12 changed once before in the '90s

t.b <- sort.habt(t, c(9020, 9030, 9090), "broad2")
summary(as.factor(t.b$changeyr))

# But fstype *has* changed as recently as 2012
t.fn <- sort.habt(t, c(9020, 9030, 9090), "fstypename")
summary(as.factor(t.fn$changeyr))

t.ft <- sort.habt(t, c(9020, 9030, 9090), "fstype")
summary(as.factor(t.ft$changeyr))

t.f2 <- sort.habt(t, c(9020, 9030, 9090), "fine2")
summary(as.factor(t.f2$changeyr))

summary(d.log$year)

# I find it interesting that there are sometimes more broad2 changes than fstypename changes
# Those changes in 2012 are all suspicious, because they all convert to fstypename=24 : "Open"
# The good news is: all of those sites were logged in 2012 (or 2011)...
# ... but this seems to be a change of methodology, because we don't see it in earlier years...???




###########################
# Creates an image plot of a sort.habt data object
# The one thing I really haven't figured yet is how to label the legend with the factor text rather than the factor number.

im <- function(data){
  require(fields)
  x <- 1:dim(data)[1]
  y <- as.numeric(names(data)[2:(match("first", names(data))-1)])
  z <- as.matrix(data[,2:(match("first", names(data))-1)])
  ncolor <- length(attr(data, "hab.levels"))
  image.plot(x, y, z, xlab = "Sites", ylab = "Year", nlevel=ncolor, breaks = 1:(ncolor+1), legend.lab = paste(attr(data, "hab.type"), "Levels"))
}

# Plots of habitat by forest using im()
par(mfrow=c(3,1))
im(sort.habt(t, c(9020), "broad2"))
im(sort.habt(t, c(9030), "broad2"))
im(sort.habt(t, c(9090), "broad2"))
im(sort.habt(t, c(9020), "fine2"))
im(sort.habt(t, c(9030), "fine2"))
im(sort.habt(t, c(9090), "fine2"))
im(sort.habt(t, c(9020), "fstypename"))
im(sort.habt(t, c(9030), "fstypename"))
im(sort.habt(t, c(9090), "fstypename"))

# Plots of habitat for sites that have changed by forest using im()
par(mfrow=c(3,1))
w = sort.habt(t, c(9020), "broad2"); im(w[w$changes > 0,])
w = sort.habt(t, c(9030), "broad2"); im(w[w$changes > 0,])
w = sort.habt(t, c(9090), "broad2"); im(w[w$changes > 0,])
w = sort.habt(t, c(9020), "fine2"); im(w[w$changes > 0,])
w = sort.habt(t, c(9030), "fine2"); im(w[w$changes > 0,])
w = sort.habt(t, c(9090), "fine2"); im(w[w$changes > 0,])
w = sort.habt(t, c(9020), "fstypename"); im(w[w$changes > 0,])
w = sort.habt(t, c(9030), "fstypename"); im(w[w$changes > 0,])
w = sort.habt(t, c(9090), "fstypename"); im(w[w$changes > 0,])

# Looking across the forests, there's been much less turnover of broad2 in 9020 (18) than in 9030 (48) or 9090 (80).




###########################
# Habitat composition by forest by year (all non-NA data)
# Note: haven't re-coded factor levels into values yet.

require(ggplot2)
require(plyr)

# Broad2
brd.a <- sort.habt(t, c(9020, 9030, 9090), "broad2")
brd.a <- merge(brd.a, d.loc[, c("site", "forest")], by = "site")
brd.m <- melt(brd.a, id.vars = c(1, dim(brd.a)[2]), measure.vars = 2:(match("first", names(brd.a))-1))

ggplot(brd.m[!is.na(brd.m$value),], aes(x=variable, stat="bin", fill=factor(value))) + geom_bar(position="fill") + scale_fill_manual(values=cbbPalette) + facet_grid( ~ forest)

table(brd.a$first, brd.a$last)

# Fine2
brd.a <- sort.habt(t, c(9020, 9030, 9090), "fine2")
brd.a <- merge(brd.a, d.loc[, c("site", "forest")], by = "site")
brd.m <- melt(brd.a, id.vars = c(1, dim(brd.a)[2]), measure.vars = 2:(match("first", names(brd.a))-1))

ggplot(brd.m[!is.na(brd.m$value),], aes(x=variable, stat="bin", fill=factor(value))) + geom_bar(position="fill") + facet_grid( ~ forest)

table(brd.a$first, brd.a$last)

# Fstypename
brd.a <- sort.habt(t, c(9020, 9030, 9090), "fstypename")
brd.a <- merge(brd.a, d.loc[, c("site", "forest")], by = "site")
brd.m <- melt(brd.a, id.vars = c(1, dim(brd.a)[2]), measure.vars = 2:(match("first", names(brd.a))-1))

ggplot(brd.m[!is.na(brd.m$value),], aes(x=variable, stat="bin", fill=factor(value))) + geom_bar(position="fill") + facet_grid( ~ forest)

table(brd.a$first, brd.a$last)


###########################
# Logging
# Note: some events in 'logging' are actually blowdowns (or a beaver dam)

log.events <- dcast(d.log[d.log$forest == 9030,], Site ~ year, fun = length, value.var="logged")

t.9030.log <- sort.habt(t, 9030, "fstypename")
d.log.9030 <- d.log[d.log$forest == 9030,]
for (i in 1:dim(d.log.9030)[1]){
  t.9030.log[t.9030.log$site == d.log.9030$Site[i], as.character(d.log.9030$year[i])] <- -1
}

# This image shows that logging events do not change a site's broad2
image(as.matrix(t.9030.log[,2:(match("first", names(t.9030.log))-1)]), col = cbbPalette) # X-axis: Sites;   Y-axis: Years