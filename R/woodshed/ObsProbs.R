require(plyr)

# Load data
# site.csv is from the mnfb database
# birds.csv is the original subset of data we received back in June
# I'm using file.choose() below, because my birds.csv file is in an entirely different location than site.csv
birds <- read.csv(file.choose())  # Choose birds.csv from your file directory
d.site <- read.csv("data/site.csv")

# Create site-year keys in both data frames
birds$id <- paste(birds$site, birds$year)
d.site$id <- paste(d.site$site, d.site$year)

# Get rid of duplicates site-years in birds (one each for BLBW, BTNW, and OVEN); keep only the columns we need
birds.sub <- subset(birds, abbrev=="BTNW", select=c(id, obs))
d.site.sub <- subset(d.site, select=c(id, obs))

# A table of observer ID from both birds.csv and site.csv for each site-year
compare <- merge(birds.sub, d.site.sub, by="id", all.x=TRUE)

# Just the site-years where the two data sources list different observer numbers
misfits <- subset(compare, obs.x != obs.y)

# The ID numbers for these observer *appear* to have changed... but some (not all) will also show up on the next table
# The observer numbers across are from site.csv
# The observer numbers down are from birds.csv
table(misfits$obs.x, misfits$obs.y)



# Now, we're going to look at observers from birds.csv that show up with multiple observer numbers in sites.csv AND vice versa
# example: birds.csv observer #76 surveyed 409 site-years; those same site-years were surveyed by observers #71 and #76 according to site.csv
# The good news, as you'll note from the diagonal pattern, is that there appears to be something very systematic at play here.

# Number of site-years for each combination of birds.csv-observer and site.csv-observer
sumfit <- ddply(compare, .(obs.x, obs.y), summarize, count = length(id))

# Observer IDs that show up twice in the birds or site columns
twice.in.birds <- sumfit$obs.x[duplicated(sumfit$obs.x)]
twice.in.d.site <- sumfit$obs.y[duplicated(sumfit$obs.y)]

# A table of observers who correspond to multiple observers in the other dataset
doubles <- subset(compare, obs.x %in% twice.in.birds | obs.y %in% twice.in.d.site)
table(doubles$obs.x, doubles$obs.y)
