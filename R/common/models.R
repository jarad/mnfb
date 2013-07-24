require(lme4)



myTryCatch = function(species, fornum, nms, ...) {
  tryCatch(...,
    warning = function(w) {
      # warning-handler-code
      modelErrors <<- rbind(modelErrors, data.frame(species=species, fornum=fornum, model=nms, type="warning", message=as.character(w)))
    }, 
    error = function(e) {
      # error-handler-code
      modelErrors <<- rbind(modelErrors, data.frame(species=species, fornum=fornum, model=nms, type="error", message=as.character(e)))
    }, finally = {
      # cleanup-code
    })
}


# Establishing index and storage variables
z <- list(); nms <- c()
i=1

# Models to Run


nms[i] <- c("stand, site, key")
myTryCatch(species, fornum, nms[i],
  z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + (1|site) + (1|key), data=birds, family=poisson))

i = i+1
nms[i] <- c("site, key")
myTryCatch(species, fornum, nms[i], 
  z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|site) + (1|key), data=birds, family=poisson))

i = i+1
nms[i] <- c("key")
z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|key), data=birds, family=poisson)

i = i+1
z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + (1|site), data=birds, family=poisson)
nms[i] <- c("stand, site")

i = i+1
z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|site), data=birds, family=poisson)
nms[i] <- c("site")

i = i+1
z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique), data=birds, family=poisson)
nms[i] <- c("stand")

i = i+1
z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr) + (1|standunique) + (1|key), data=birds, family=poisson)
nms[i] <- c("stand, key")

i = i+1
z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|yearf) + (1|obs) + (1|obsyr), data=birds, family=poisson)
nms[i] <- c("none")
