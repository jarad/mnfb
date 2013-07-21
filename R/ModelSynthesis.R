# Synthesis

require(lme4)
require(plyr)

source("R/common/functions.R")
source("R/common/loaddata.R")

##########################
# Load Data

birds <- loaddata("OVEN", 9020)

birds <- birds[,-which(names(birds)=="X_COORD")]   # Drops XCOORD
birds <- birds[,-which(names(birds)=="Y_COORD")]   # Drops YCOORD
birds <- na.omit(birds)

birds$key   <- paste(birds$site, birds$year, birds$abbrev)
birds$yearf <- as.factor(birds$year)
birds$obsyr <- factor(paste(birds$obs, birds$year))
birds$fsdr  <- factor(paste(birds$fstypename, birds$stockdens, birds$regen))
birds$bsdr  <- factor(paste(birds$broad2, birds$stockdens, birds$regen))
birds$finsdr <- factor(paste(birds$fine2, birds$stockdens, birds$regen))

birds$year <- scale(birds$year)
birds$temp <- scale(birds$temp)
birds$jd   <- scale(birds$jd)
birds$time <- scale(birds$time)
birds$siteorigyear <- scale(birds$siteorigyear)

birds$year2 <- (birds$year)^2
birds$temp2 <- (birds$temp)^2
birds$jd2   <- (birds$jd)^2
birds$time2 <- (birds$time)^2
birds$soy2 <- (birds$siteorigyear)^2

desig_factors(birds)

##########################
# Create a random sample of sites from birds

# Useful if data haven't already been filtered by species and/or forest
# birds <- subset(birds, abbrev=="OVEN")
# sites <- unique(birds$site[birds$forest==9020])
sites <- unique(birds$site)
sites.samp <- sample(sites, ceiling(length(sites)*0.25))   # Alternatively, we could subsample stands
birds <- subset(birds, site %in% sites.samp)

##########################
# Run Models & Purge NA's

source("R/common/models.R")

for (j in i:1){
  if (is.logical(z[[j]])) {
    z <- z[[-j]]
    nms <- nms[-i] }
}
i <- length(z)


##########################
# Compile AIC/BIC, Fixed Effect Data, Variance of Random Effects, and Estimate Random Effects

a <- get.effects(z)
fixedeff <- a$fixedeff
fixederr <- a$fixederr
fixedp <- a$fixedp
infocrit <- a$infocrit
randeffs <- a$randeffs
estyearf <- laply(z, randex, "yearf", birds); colnames(estyearf) <- sort(unique(birds$yearf))
estobs <- laply(z, randex, "obs", birds); colnames(estobs) <- sort(unique(birds$obs))
estobsyr <- laply(z, randex, "obsyr", birds); colnames(estobsyr) <- sort(unique(birds$obsyr))
estsite <- laply(z, randex, "site", birds); colnames(estsite) <- sort(unique(birds$site))
estkey <- laply(z, randex, "key", birds); colnames(estkey) <- sort(unique(birds$key))
estfstype <- laply(z, randex, "fstypename", birds); colnames(estfstype) <- sort(unique(birds$fstypename))
