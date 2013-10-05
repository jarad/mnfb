
# working from the mnfb folder from git 

# 1) Yearly Average data creation 

#setwd( "/home/nachalca/mnfb/data") 
setwd('C:\\Users\\Toshiba\\Documents\\GitHub\\mnfb\\data')
bird <- read.csv('bird.csv', header=T)

# each row is a bird !!!
# minutes3: Time period during which the individual was observed divided into 3  time intervals.  
# This protocol was used from 1991-2007; see 'minutes' lookup table for code descriptions
#head(bird) 

# reduce birds species only the species in sp.info are important. 
species <- read.csv("C:\\Users\\Toshiba\\Documents\\GitHub\\mnfb\\R\\common\\trendspecies.csv")
sp.code <- read.csv('nrri_bird_code.csv')       
sp.info <-  merge(species, sp.code)
bird.red <- subset(bird, nrricode %in% sp.info$nrricode)       

# get the forest info for each site
forest <- read.csv('location.csv',header=T) 
#table(forest$forest) 
# there are 5 forest here, that is right, only 9030, 9090 and 9020 are important 

bird.red2 <- merge(bird.red, subset(forest, select=c('forest','site') ) ) 
bird.red <- subset(bird.red2, forest %in% c(9020,9030,9090) )

# Compute Yearly count on each period
library(plyr) 
bird.tot <-ddply(bird.red, .(year,forest,nrricode), 
                 summarise,count=length(minutes3),count1=sum(minutes3==1),count2=sum(minutes3==2),count3=sum(minutes3==3)
)       
#head(bird.tot)
#with(bird.tot, table(year,forest))       

with(bird.tot, length(unique(nrricode))) # 73 species

# Info for species with 0 counts is missing, for the 0 counts in all year
# we can add 1 bird to work in logs later. 
bird.tot$forest <- factor(bird.tot$forest)
gr  <- expand.grid(unique(bird.tot$year),levels(bird.tot$forest), unique(bird.tot$nrricode))
colnames(gr) <- colnames(bird.tot)[1:3]
aux <- merge(bird.tot,gr, all.y=T)
aux[is.na(aux$count), 4:7] <- 0
aux$count.add <- aux$count
cond <- aux$count==0
aux$count.add[cond] <- 1 
bird.tot <- aux



# site information: there is one recor per site*year, so here is the total times each site is sampled
site <- read.csv('site.csv', header=T)
site2<- merge(site, forest)
with(site2, length(unique(site)))

# get0 has the number of site sampled each year*forest, this is the denominator for the anual counts
# to get the anual average. 
get0 <- ddply(site2, .(year, forest),summarise, samples=length(obs) )
get0 <- get0[ get0$forest %in% c(9020, 9030, 9090), ]
get0$forest <- factor(get0$forest)

# compute average count on each site*year
bird.tot <- merge(bird.tot, get0)
bird.tot$ave <- with(bird.tot, count/samples)
bird.tot$forest <- as.numeric(as.character(bird.tot$forest))

# cut the counts previous to 1995 
bird.tot <- bird.tot[bird.tot$year > 1994, ]

# put the species and  forest names, 
# Chequamegon=9020,Chippewa=9030, Superior=9090
bird.yeartotal <- merge(bird.tot, sp.info[,1:2])

bird.yeartotal$forestN <- factor(bird.yeartotal$forest)
levels(bird.yeartotal$forestN) = c('Chequamegon','Chippewa','Superior') 
#with(bird.yeartotal, table(forestN,forest))


# saving data
write.csv(bird.yeartotal, file='bird_yeartotal.csv', row.names=FALSE)





