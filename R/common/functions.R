
# Variables
forest3 = c(9020,9030,9090)


# Quit function
myquit = function() q(ifelse(interactive(),"ask","no"))

# Functions to return proper location for tables and figures
tab_dir = function(filename) {
  paste("tables/", filename, sep="")
}

fig_dir = function(filename) {
  paste("figs/", filename, sep="")
}

# Function to use forest name rather than code
# This should probably be updated to ensure the codes match the 
# forest names. 
refactor_forests = function(forest) {
  factor(forest, labels=c("Chequamegon","Chippewa","Superior"))
}

# Redesignates the following columns as factors
desig_factors = function(data) {
  facs <- c("forest","site","nrricode","obs","stockdens","wind","noise","sky","regen", "yearf", "fsdr", "finsdr", "bsdr")
  for (i in 1:length(facs)) {
    if (facs[i] %in% names(data)) {data[,facs[i]] <- as.factor(data[,facs[i]])}
  }
}


# Extracts information criteria, fixed effects, and random effects from a list of models
get.effects <- function(modellist) {
  z <- modellist
  i <- length(z)
  
  # Storage data frames and vectors
  infocrit <- fixedeff <- fixederr <- fixedp <- data.frame()
  yeareff <- obseff <- siteff <- keyeff <- fstypeff <- fineff <- broadeff <- numeric(length(z))
  
  for (j in 1:i){
    # Collect Information Criteria
    ic <- summary(z[[j]])@AICtab
    infocrit <- rbind(infocrit, ic)
    # Collect Estimated Fixed Effects
    fe <- data.frame(names(z[[j]]@fixef), unname(z[[j]]@fixef))
    fixedeff <- if (j==1) {fe} else {merge(fixedeff, fe, by.x="names.z..j...fixef.", by.y="names.z..j...fixef.", all = TRUE, suffixes=c(j, j+1), sort=TRUE)}
    # Collect Estimated Error for Fixed Effects
    fer <-  data.frame(rownames(summary(z[[j]])@coefs), summary(z[[j]])@coefs[,2])
    fixederr <- if (j==1) {fer} else {merge(fixederr, fer, by.x="rownames.summary.z..j....coefs.", by.y="rownames.summary.z..j....coefs.", all = TRUE, suffixes=c(j, j+1), sort=TRUE)}
    # Collect Estimate p-Values for Fixed Effects
    fep <-  data.frame(rownames(summary(z[[j]])@coefs), summary(z[[j]])@coefs[,4])
    fixedp <- if (j==1) {fep} else {merge(fixedp, fep, by.x="rownames.summary.z..j....coefs.", by.y="rownames.summary.z..j....coefs.", all = TRUE, suffixes=c(j, j+1), sort=TRUE)}
    
    # This is how you extract the variance for random effects
    # For now, the desired random effects are pre-specified
    fuzz <- summary(z[[j]])@REmat
    yeareff[j] <- as.numeric(fuzz[fuzz[,1]=="yearf",][3])
    obseff[j] <- as.numeric(fuzz[fuzz[,1]=="obsyr",][3])
    siteff[j] <- as.numeric(fuzz[fuzz[,1]=="site",][3])
    keyeff[j] <- as.numeric(fuzz[fuzz[,1]=="key",][3])
    fstypeff[j] <- as.numeric(fuzz[fuzz[,1]=="fstypename",][3])
    fineff[j] <- as.numeric(fuzz[fuzz[,1]=="fine2",][3])
    broadeff[j] <- as.numeric(fuzz[fuzz[,1]=="broad2",][3])
  }
  
  randeffs <- cbind(broadeff, fineff, fstypeff, yeareff, obseff, siteff, keyeff)
  rownames(infocrit) <- rownames(randeffs) <- nms
  colnames(fixedeff) <- colnames(fixederr) <- colnames(fixedp) <- c("Effect", nms)
  
  return(list(fixedeff = fixedeff,
              fixederr = fixederr,
              fixedp = fixedp,
              infocrit = infocrit,
              randeffs = randeffs))
}
