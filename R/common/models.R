require(lme4)



myTryCatch = function(species, fornum, nms, ...) {
  tryCatch(...,
    warning = function(w) {
      # warning-handler-code
      modelErrors <<- rbind(modelErrors, data.frame(species=species, fornum=fornum, model=nms, type="warning", message=as.character(w)))
      return(suppressWarnings(...))
    }, 
    error = function(e) {
      # error-handler-code
      modelErrors <<- rbind(modelErrors, data.frame(species=species, fornum=fornum, model=nms, type="error", message=as.character(e)))
      return(NULL) # Or do you want this to be NA?
    }, finally = {
      # cleanup-code
    })
}


# Establishing index and storage variables
z <- list(); nms <- c()
i=1

# Models to Run


nms[i] <- c("stand, site, key")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + (1|site) + (1|key), data=birds, family=poisson))

i = i+1
nms[i] <- c("site, key")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|site) + (1|key), data=birds, family=poisson))

i = i+1
nms[i] <- c("key")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|key), data=birds, family=poisson))

i = i+1
nms[i] <- c("stand, site")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + (1|site), data=birds, family=poisson))

i = i+1
nms[i] <- c("site")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|site), data=birds, family=poisson))

i = i+1
nms[i] <- c("stand")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique), data=birds, family=poisson))

i = i+1
nms[i] <- c("stand, key")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + (1|key), data=birds, family=poisson))

i = i+1
nms[i] <- c("none")
z[i] = myTryCatch(species, fornum, nms[i], glmer(N ~ fsdr + wind + noise + sky + year + I(year^2) + jd + I(jd^2) + time + I(time^2) + temp + I(temp^2) + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr), data=birds, family=poisson))

