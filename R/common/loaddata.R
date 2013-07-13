#' A function to extract survey information for a set of species and forests. 
#'
#' @param species a string vector indicating which species are requested.
#' @param forest.study a numeric vector indicating which forests are required 
#' @return a data.frame with each survey as a row 
loaddata <- function(species, forest.study){

  require(plyr)
  require(stringr)
  require(lubridate)
  
  source("R/common/functions.R")
  
  # For the following, the user needs to input the species (spp) and the forest (forest.study)... which suggests defining a function...
  
  # Name the species of interest and look up its NRRI code
  nrri_bird_code <- read.csv("data/nrri_bird_code.csv")
  nrri <- nrri_bird_code$nrricode[nrri_bird_code$abbrev == species]
  
  # Collect general site and stand data for forest of interest
  d.loc <- read.csv("data/location.csv")
  d.loc <- d.loc[d.loc$forest == forest.study, c("forest", "standunique", "site", "X_COORD", "Y_COORD")]
  
  # Generate sites and time-of-observation data for sites within the forest of interest
  d.site <- read.csv("data/site.csv")
  d.site <- d.site[d.site$site %in% d.loc$site,]
  d.site <- d.site[,c("site", "year", "date", "time", "obs", "temp", "wind", "sky", "noise", "fstype")]
  
  # Convert daate and time to numeric "jd" and "time"
  POSIXdate <- as.POSIXct(as.character(d.site$date), format = "%m/%d/%Y %H:%M:%S")
  jan1 <- as.Date(paste("1/1/", year(POSIXdate), sep=""), format = "%m/%d/%Y")
  d.site$jd <- as.numeric(floor(difftime(POSIXdate, jan1, units = "days"))) + 1
  
  POSIXtime <- as.POSIXct(as.character(d.site$time), format = "%m/%d/%Y %H:%M:%S")
  d.site$time <- hour(POSIXtime) + minute(POSIXtime)/60
  
  # Bird counts by site & year for species of interest
  d.bird <- read.csv("data/bird.csv")
  d.bird <- d.bird[d.bird$nrricode == nrri & d.bird$site %in% d.loc$site,]
  d.bird <- ddply(d.bird, .(site, year), summarize,
                  X1=length(minutes3==1), 
                  X2=length(minutes3==2), 
                  X3=length(minutes3==3), 
                  N=length(minutes3),
                  X1_100=length(minutes3==1 & outside==0), 
                  X2_100=length(minutes3==2 & outside==0), 
                  X3_100=length(minutes3==3 & outside==0), 
                  N_100=length(minutes3 & outside==0))
  
  # Habitat Variables
  # I would appreciate if you look this to consider alternative variable choices, given discussings with Jerry
  d.fstype <- read.csv("data/forest_type.csv")
  d.fstype <- d.fstype[,c("fstype", "fstypename", "fine2", "broad2", "age")]
  # Define a couple of variables (note: the code for stockdense is the final digit of fstype)
  d.fstype$regen <- ifelse(d.fstype$age == "regeneration","yes","no")
  d.fstype$stockdens <- str_sub(d.fstype$fstype, -1, -1)
  
  # Get siteorigyear
  d.soy <- read.csv("data/current_site_ages.csv")
  colnames(d.soy)[colnames(d.soy)=="X04_RevisedYearOrig"] <- "siteorigyear"
  
    
  t <- join(d.site, d.loc, by = "site")
  t2 <- merge(t, d.bird, by.x = c("site", "year"), by.y = c("site", "year"), all=TRUE)
  t3 <- merge(t2, d.fstype, by.x = "fstype", by.y = "fstype", all.x = TRUE, all.y = FALSE)
  t4 <- merge(t3, d.soy[,c("Site", "siteorigyear")], by.x = "site", by.y = "Site", all.x = TRUE, all.y = FALSE)
  
  
  # Note: there is one ovenbird bird observation for site:1012 year:2010 with no corresponding d.site entry.
  # I think we should consider what to do with observations that have no data-collection entry in site.csv.
  # The points would not be used in models on account of NAs.  Is it useful to identify all these points?  I noticed in code-testing that "UFLY" has 39 of them.
  
  # Convert NAs in the counts to zeros -- uses that fact that if is.na(t3$X1)==TRUE, then so are all the other counts
  t4[,c("X1", "X2", "X3", "N", "X1_100", "X2_100", "X3_100", "N_100")][is.na(t3$X1),] <- 0
  
  
  # I have not yet created the following variables:
  #      key, stock, dense, year2, temp2, jd2, time2, obsyr, fsdr, bsdr, finsdr
  # stock & dense probably should be left as the combined variable "stockdens"
  # the squared terms can be created when creating appropriate models
  # the same goes for key, obsyr, fsdr, bsdr, finsdr
  # note that fsdr is already exactly present within the variable fstype
  # I'm not sure what to do with "regen" as a variable.  What I have done above (i.e. to assign values from the "age" variable) is not what I did before (which was to split "regen" off from the broad2 classification).  Also, what I have done above is redundant with the stockdens variable... but I'm leaving it as is, because I'm not sure what (if anything) is the better strategy.
  # If you survey the forest_type data table, you'll see that regen is applied *differently* in each of the following variables: age, age_class, age2, broad2, fine1, and fine2.  It makes the "regen" concept potentially meaningless.
  
  # Also, "regen" may become less important as we re-craft siteorigyear.  "siteorigyear" could almost be retooled to become "years since last logging", since we have the logging data; then, siteorigyear can change from year to year within the same site.  This could be particularly useful for sites that have been logged during the course of the study.  However, there's a problem.  The current_site_ages.csv only goes up through 2004, and while the logged_sites.csv does go all the way up to the present, not all pre-2005 logging events in logged_sites are reflected in the columns of current_site_ages... which leaves me unsure of which logging event should or should not reset the siteorigyear counter.
  
  # I don't remember... did we ever get an explanation as to the distinction between <blank> and "not used" in fine2/broad2?
  
  return(t4)
}
