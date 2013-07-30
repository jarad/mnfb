library(lme4)
library(plyr)

source("R/common/loaddata.R")


###########################################################
# Set up dataframe to look at species and forests
###########################################################
species_forest = expand.grid(fornum=c(9020,9030,9090),
                             abbrev=read.csv("R/common/trendspecies.csv")$abbrev)

# temporarily reduce the number of analyses
species_forest = species_forest[species_forest$abbrev %in% c("OVEN","BTNW"),]



###########################################################
# Function to run analysis for a single species and forest
###########################################################
run_model = function(d,...) {
  glmer(N ~ year + I(year^2) + 
            temp + I(temp^2) +
            jd + I(jd^2) + 
            time + I(time^2) + 
            siteorigyear + 
            (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + 
            (1|site) + (1|key) + (1|sky),
            data = loaddata(d$abbrev, d$fornum),
            family=poisson,...)
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


