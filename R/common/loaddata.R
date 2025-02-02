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

  # Read in relevant data sets
  d.bird <- read.csv("data/bird.csv")
  d.loc <- read.csv("data/location.csv")
  d.site <- read.csv("data/site.csv")
  d.fstype <- read.csv("data/forest_type.csv")
  d.soy <- read.csv("data/current_site_ages.csv")
  nrri_bird_code = read.csv("data/nrri_bird_code.csv")


  


  # Name the species of interest and look up its NRRI code
  nrri_code = nrri_bird_code$nrricode[nrri_bird_code$abbrev %in% species]
  d.bird = join(d.bird[d.bird$year > 1994.5,], nrri_bird_code[,c("nrricode","abbrev")], "nrricode")

  d.bird = d.bird[d.bird$abbrev %in% species,] 
 
 
  # Collect general site and stand data for forest of interest
  d.loc <- d.loc[d.loc$forest %in% forest.study, c("forest", "standunique", "site", "X_COORD", "Y_COORD")]
  
  # Generate sites and time-of-observation data for sites within the forest of interest
  d.site <- d.site[d.site$site %in% d.loc$site & d.site$year > 1994.5,]
  d.site <- d.site[,c("site", "year", "date", "time", "obs", "temp", "wind", "sky", "noise", "fstype")]
  
  d.site$jd <- jdate(d.site)
  d.site$time <- timehr(d.site)
  
  # Bird counts by site & year for species of interest
  d.bird <- d.bird[d.bird$nrricode %in% nrri_code & d.bird$site %in% d.loc$site,]
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
  d.fstype <- d.fstype[,c("fstype", "fstypename", "fine2", "broad2", "age")]
  # Define a couple of variables (note: the code for stockdense is the final digit of fstype)
  d.fstype$regen <- ifelse(d.fstype$age == "regeneration","yes","no")
  d.fstype$stockdens <- str_sub(d.fstype$fstype, -1, -1)
  
  # Get siteorigyear
  colnames(d.soy)[colnames(d.soy)=="X04_RevisedYearOrig"] <- "siteorigyear"
  
    
  t <- join(d.site, d.loc, by = "site")
  t2 <- merge(t, d.bird, by.x = c("site", "year"), by.y = c("site", "year"), all=TRUE)
  t3 <- merge(t2, d.fstype, by.x = "fstype", by.y = "fstype", all.x = TRUE, all.y = FALSE)
  t4 <- merge(t3, d.soy[,c("Site", "siteorigyear")], by.x = "site", by.y = "Site", all.x = TRUE, all.y = FALSE)
  
  
  # Note: there is one ovenbird bird observation for site:1012 year:2010 with no corresponding d.site entry.
  # I think we should consider what to do with observations that have no data-collection entry in site.csv.
  # I made an error before.  1012/2010 is the only site/year without a d.site entry.  Conversely, there are three d.sites without any site/year observations: "686 1992"  "289 2008"  "1655 2008"
  
  # Convert NAs in the counts to zeros -- uses that fact that if is.na(t4$X1)==TRUE, then so are all the other counts
  t4[,c("X1", "X2", "X3", "N", "X1_100", "X2_100", "X3_100", "N_100")][is.na(t4$X1),] <- 0
  
  
  # I have not yet created the following variables:
  t4 <- t4[,-which(names(t4)=="X_COORD")]   # Drops XCOORD
  t4 <- t4[,-which(names(t4)=="Y_COORD")]   # Drops YCOORD
  t4 <- na.omit(t4)
    
  t4$key   <- paste(t4$site, t4$year, t4$abbrev)
  t4$yearf <- as.factor(t4$year)
  t4$obsyr <- factor(paste(t4$obs, t4$year))

  # Note for the following: right now, sd implies regen, so the regen's below are redundant
  t4$fsdr  <- factor(paste(t4$fstypename, t4$stockdens, t4$regen))
  t4$bsdr  <- factor(paste(t4$broad2, t4$stockdens, t4$regen))
  t4$finsdr <- factor(paste(t4$fine2, t4$stockdens, t4$regen))
    
  t4$year <- scale(t4$year)
  t4$temp <- scale(t4$temp)
  t4$jd   <- scale(t4$jd)
  t4$time <- scale(t4$time)
  t4$siteorigyear <- scale(t4$siteorigyear)
    
  #t4$year2 <- (t4$year)^2
  #t4$temp2 <- (t4$temp)^2
  #t4$jd2   <- (t4$jd)^2
  #t4$time2 <- (t4$time)^2
  #t4$soy2 <- (t4$siteorigyear)^2
    
  t4 <- desig_factors(t4)

  # I'm not sure what to do with "regen" as a variable.  What I have done above (i.e. to assign values from the "age" variable) is not what I did before (which was to split "regen" off from the broad2 classification).  Also, what I have done above is redundant with the stockdens variable... but I'm leaving it as is, because I'm not sure what (if anything) is the better strategy.
  # If you survey the forest_type data table, you'll see that regen is applied *differently* in each of the following variables: age, age_class, age2, broad2, fine1, and fine2.  It makes the "regen" concept potentially meaningless.
  
  # Also, "regen" may become less important as we re-craft siteorigyear.  "siteorigyear" could almost be retooled to become "years since last logging", since we have the logging data; then, siteorigyear can change from year to year within the same site.  This could be particularly useful for sites that have been logged during the course of the study.  However, there's a problem.  The current_site_ages.csv only goes up through 2004, and while the logged_sites.csv does go all the way up to the present, not all pre-2005 logging events in logged_sites are reflected in the columns of current_site_ages... which leaves me unsure of which logging event should or should not reset the siteorigyear counter.
  
  # I don't remember... did we ever get an explanation as to the distinction between <blank> and "not used" in fine2/broad2?
  
  return(t4)
}
