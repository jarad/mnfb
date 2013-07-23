# Synthesis

require(lme4)
require(plyr)

source("R/common/functions.R")
source("R/common/loaddata.R")

##########################
# Load Data

for (species in c("BLBW", "BTNW", "OVEN")){
  for (fornum in c(9020, 9030, 9090)){

    birds <- loaddata(species, fornum)
    
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
    
    birds <- desig_factors(birds)
    
    ##########################
    # Create a random sample of sites from birds
    
    # Useful if data haven't already been filtered by species and/or forest
    # birds <- subset(birds, abbrev=="OVEN")
    # sites <- unique(birds$site[birds$forest==9020])
    sites <- unique(birds$standunique)
    sites.samp <- sample(sites, ceiling(length(sites)*0.25))   # Using stands instead of sites
    birds <- subset(birds, standunique %in% sites.samp)
    
    ##########################
    # Run Models & Purge NA's
    
    source("R/common/models.R")
    
    for (j in i:1){
      if (is.null(z[[j]])) {
        z <- z[-j]
        nms <- nms[-j] }
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
    estyearf <- laply(z, randex, "yearf", birds)
    estobs <- laply(z, randex, "obs", birds)
    estobsyr <- laply(z, randex, "obsyr", birds)
    eststand <- laply(z, randex, "standunique", birds)
    estsite <- laply(z, randex, "site", birds)
    estkey <- laply(z, randex, "key", birds)
    estfstype <- laply(z, randex, "fstypename", birds)
    estbroad <- laply(z, randex, "broad2", birds)
    estfine <- laply(z, randex, "fine2", birds)
    
    if (i>1) {
      colnames(estyearf) <- sort(unique(birds$yearf))
      colnames(estobs) <- sort(unique(birds$obs))
      colnames(estobsyr) <- sort(unique(birds$obsyr))
      colnames(eststand) <- sort(unique(birds$standunique))
      colnames(estsite) <- sort(unique(birds$site))
      colnames(estkey) <- sort(unique(birds$key))
      colnames(estfstype) <- sort(unique(birds$fstypename))
      colnames(estbroad) <- sort(unique(birds$broad2))
      colnames(estfine) <- sort(unique(birds$fine2))
    }
    
    ##########################
    # Compile AIC/BIC, Fixed Effect Data, Variance of Random Effects, and Estimate Random Effects
    write = TRUE
    if(write) {
      write.csv(fixedeff, file = paste("modout/fixedeff", species, fornum, ".csv", sep = "", na="."))
      write.csv(fixederr, file = paste("modout/fixederr", species, fornum, ".csv", sep = "", na="."))
      write.csv(fixedp, file = paste("modout/fixedp", species, fornum, ".csv", sep = "", na="."))
      write.csv(infocrit, file = paste("modout/infocrit", species, fornum, ".csv", sep = "", na="."))
      write.csv(randeffs, file = paste("modout/randeffs", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estyearf, file = paste("modout/estyearf", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estobs, file = paste("modout/estobs", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estobsyr, file = paste("modout/estobsyr", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(eststand, file = paste("modout/eststand", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estsite, file = paste("modout/estsite", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estkey, file = paste("modout/estkey", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estfstype, file = paste("modout/estfstype", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estbroad, file = paste("modout/estbroad", species, fornum, ".csv", sep = "", na="."))
      #   write.csv(estfine, file = paste("modout/estfine", species, fornum, ".csv", sep = "", na="."))
      print(paste(dim(birds)[1], "obs"))
    }    
  }
}