require(stringr)
require(ggplot2)
require(plyr)

source("R/common/functions.R")

# Read in data; note the NA values
birds <- read.csv("data/mnfb.csv", sep=",", na.strings=c(-99, "", "NA"))
# birds <- read.csv("data/mnfb.csv", sep=",")

names(birds) <- str_replace_all(names(birds), "[[:punct:]]", "")    # Clean up column names
desig_factors(birds)    # See functions.R

# Combine all data by site uniqueID regardless of distance
birds$key <- paste(birds$site, birds$year, birds$abbrev)
bunl <- subset(birds, distance=="unlimited")
b100 <- subset(birds, distance=="100m")[,c("key", "X1", "X2", "X3")]
names(b100) <- c("key", "X1_100", "X2_100", "X3_100")
birds <- merge(bunl, b100, by.x="key", by.y="key")

# Data Manipulations
birds$time <- birds$timehr+birds$timemin/60
birds$stock[birds$stockdens==0] <- "Nonstocked"
birds$stock[birds$stockdens %in% c(1:3)] <- "Sapling"
birds$stock[birds$stockdens %in% c(4:6)] <- "Poletimber"
birds$stock[birds$stockdens %in% c(7:9)] <- "Sawtimber"
birds$dense[birds$stockdens==0] <- "0-16%"
birds$dense[birds$stockdens %in% c(1,4,7)] <- "16-39%"
birds$dense[birds$stockdens %in% c(2,5,8)] <- "40-69%"
birds$dense[birds$stockdens %in% c(3,6,9)] <- "70+%"
birds$N <- with(birds, X1 + X2 + X3) 
birds$N_100 <- with(birds, X1_100 + X2_100 + X3_100)
birds$regen <- 0
birds$regen[grep("regen", birds$broad2)] <- 1

# Deletion of superfluous columns
birds <- subset(birds, select= -c(distcty, timehr, timemin, distance))
