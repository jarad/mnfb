
# working from the mnfb folder from git 

# 1) Yearly Average data creation 

#setwd( "/home/nachalca/mnfb/data") 
setwd('C:\\Users\\Toshiba\\Documents\\GitHub\\mnfb\\data')
bird <- read.csv('bird.csv', header=T)

# each row is a bird !!!
# minutes3: Time period during which the individual was observed divided into 3  time intervals.  
# This protocol was used from 1991-2007; see 'minutes' lookup table for code descriptions
head(bird) 

# reduce birds species only the species in sp.info are important. 
species <- read.csv("C:\\Users\\Toshiba\\Documents\\mnfb\\R\\common\\trendspecies.csv")
sp.code <- read.csv('nrri_bird_code.csv')       
sp.info <-  merge(species, sp.code)
bird.red <- subset(bird, nrricode %in% sp.info$nrricode)       

# get the forest info for each site
forest <- read.csv('location.csv',header=T) 
table(forest$forest) 
# there are 5 forest here, that is right, only 9030, 9090 and 9020 are important 

bird.red2 <- merge(bird.red, subset(forest, select=c('forest','site') ) ) 
bird.red <- subset(bird.red2, forest %in% c(9020,9030,9090) )

# Compute Yearly count on each period
library(plyr) 
bird.tot <-ddply(bird.red, .(year,forest,nrricode), summarise, 
                        count1=sum(minutes3==1),count2=sum(minutes3==2),count3=sum(minutes3==3),
                        count=count1+count2+count3, prop3 = count3/count)       

bird.tot <- merge(bird.tot, sp.info[,1:2])
head(bird.tot)
#with(bird.tot, table(year,forest))       

# site information: there is one recor per site*year, so here is the total times each site is sampled
site <- read.csv('site.csv', header=T)
site2<- merge(site, forest)
with(site2, table(year,forest))
get0 <- ddply(site2, .(year, forest),summarise, samples=length(obs) )

# compute average count on each site*year
bird.tot <- merge(bird.tot, get0)
bird.tot$ave <- with(bird.tot, count/samples)
bird.tot$forest <- factor(bird.tot$forest)

# cut the counts previous to 1995 
bird.tot <- bird.tot[bird.tot$year > 1994, ]

head(bird.tot[order(bird.tot$count,decreasing=T),],15)

# some initial plots 
birdtot.3sp <- subset(bird.tot, abbrev %in% c('BLBW', 'BTNW', 'OVEN','NAWA') )
library(ggplot2)
qplot(data=birdtot.3sp, x=year,y=ave,geom=c('line','point'), color=abbrev, facets=abbrev~forest, main='Yearly Average Count per forest-species')
qplot(data=birdtot.3sp, x=year,y=prop3,geom=c('line','point'), color=abbrev, facets=abbrev~forest, main='Proportion of birds detected in the 3rd period')

#--------------

# 2) Simplest model: linear models without random terms one per specie*forest

mod.fix <- NULL
for (i in 1:3) { # workinng with 5 forest
  f <- levels(bird.tot$forest)[i]
  for (j in 1:length(levels(bird.tot$abbrev)) ) {
    s <- levels(bird.tot$abbrev)[j]
    aux <- lm(ave ~ year+I(year^2) ,data= with(bird.tot, bird.tot[forest==f & abbrev==s,]) )
    aux2 <- c(summary(aux)$coefficients[,1],summary(aux)$sigma,summary(aux)$coefficients[,4])
    aux3 <- data.frame(f,s,t(aux2))
    mod.fix <- rbind(mod.fix, aux3)  
  }
}
colnames(mod.fix) <- c('Forest', 'Specie','Intercept','Trend','Quad','Sigma','pval.int','paval.tr','pval.q' )

summary(mod.fix) 
# xtable(summary(mod.fix))
 
library(reshape2)
modfix.melt <- melt(data=mod.fix, id.vars=1:2, measure.vars=3:6, variable.name='Parameter', value.name='Estimate')
colnames(modfix.melt)[3:4] <- c('Parameter', 'Estimate')
qplot(data=modfix.melt, x=Estimate, geom='histogram')+facet_grid(facets=Forest~Parameter,scale='free')
# -----------------------------------------------------
# -----------------------------------------------------

#3) shiny application to plot average over years 
setwd('C:\\Users\\Toshiba\\Documents\\GitHub\\mnfb\\R')
library(shiny)
runApp("shiny1")






