require(lme4)

# Establishing index and storage variables
z <- list(); nms <- c()
i=1

# Models to Run

z[i] <- glmer(N ~ fsdr + wind + noise + sky + year + year2 + jd + jd2 + time + time2 + temp + temp2 + siteorigyear + (1|obsyr) + (1|standunique) + (1|site), data=birds, family=poisson)
nms[i] <- c("fsdr / none")




