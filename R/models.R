require(lme4)

# Establishing index and storage variables
z <- list(); nms <- c()
i=1

# Models to Run

z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|obsyr) + (1|site), data=birds, family=poisson)
nms[i] <- c("fsdr / none")

i=i+1
z[i] <- glmer(N ~ finsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|obsyr) + (1|site), data=birds, family=poisson)
nms[i] <- c("finsdr / none")

i=i+1
z[i] <- glmer(N ~ finsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|obsyr) + (1|site) + (1|fstypename), data=birds, family=poisson)
nms[i] <- c("finsdr / fstype")

i=i+1
z[i] <- glmer(N ~ bsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|obsyr) + (1|site) + (1|fstypename) + (1|fine2), data=birds, family=poisson)
nms[i] <- c("bsdr / fstype, fine2")

i=i+1
z[i] <- glmer(N ~ stockdens + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|obsyr) +(1|site) + (1|fstypename) + (1|fine2) + (1|broad2), data=birds, family=poisson)
nms[i] <- c("sd / fstype, fine2, broad2")

i=i+1
z[i] <- glmer(N ~ wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|obsyr) + (1|site) + (1|fstypename) + (1|fine2) + (1|broad2), data=birds, family=poisson)
nms[i] <- c("none / fstype, fine2, broad2")


