library(lme4)
library(plyr)

source("R/common/loaddata.R")
source("R/common/functions.R")

modelErrors <<- data.frame(species=character(), fornum=numeric(), model=character(), type=character(), message=character())

###########################################################
# Set up dataframe to look at species and forests
###########################################################
species_forest = expand.grid(fornum=c(9020, 9030, 9090),
                             abbrev=read.csv("R/common/trendspecies.csv")$abbrev)

# temporarily reduce the number of analyses
species_forest = species_forest[species_forest$fornum %in% c(9020) & species_forest$abbrev %in% c("BLBW", "BTNW", "OVEN", "CONW"),]



###########################################################
# Function to run analysis for a single species and forest
###########################################################

keynames <- read.csv("R/common/keynames.csv")$x

run_model = function(d,...) {
  e = loaddata(d$abbrev, d$fornum)
  
  myTryCatch(d$abbrev, d$fornum, "main.model",
             glmer(N ~ year + I(year^2) + 
                     temp + I(temp^2) +
                     jd   + I(jd^2  ) + 
                     time + I(time^2) + 
                     siteorigyear + 
                     noise +
                     (1|wind) +
                     (1|broad2) + (1|fine2) + (1|fstypename) +
                     (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + 
                     (1|site) + (1|key) + (1|sky),
                   data = e[e$key %in% keynames,],
                   family=poisson,...)
  )
}


############################################
# Run the model for all species and forests
############################################
parallel <- require(doMC, quietly=TRUE)
if(parallel){
    registerDoMC()
}

out = dlply(species_forest,
            .(fornum,abbrev),
            run_model,
            .parallel=parallel)

save.image("R/model.RData")


