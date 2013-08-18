############################
# Code that identifies observations of non-existent NRRI codes

# Observations
d.bird <- read.csv("data/bird.csv")
d.bird = join(d.bird, d.loc[,c("site","forest")], by = "site")
d.bird = d.bird[d.bird$forest %in% c(9020, 9030, 9090),]

# NRRI Codes
nrri <- read.csv("data/nrri_bird_code.csv")

# Non-existent codes (there are only two)
no.such.code <- unique(d.bird$nrricode[!(d.bird$nrricode %in% unique(nrri$nrricode))])

# A single example of NRRIcode = 764
d.bird[d.bird$nrricode == 764,]

# 563 examples of NRRIcode = 904
dim(d.bird[d.bird$nrricode == 904,])