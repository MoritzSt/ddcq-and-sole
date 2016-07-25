# This script calculates the Schoener index of spatial overlap between sole SSB and
# BT effort of BEL and NED from data priorly loaded through sourcing in an superordinate script.

  library(dplyr)

  
dat$rel.CPUE.minus.rel.f <- abs(dat$share_ssb - dat$relative_effort)  # calculate difference in shares 
dat <- dplyr::group_by(.data = dat, year, country)
dat <- dplyr::summarise(.data = dat, 
                        sum_minuses = sum(rel.CPUE.minus.rel.f))
dat$Schoener.I <- 1 - 1/2 * dat$sum_minuses
dat$sum_minuses <- NULL
dat <- dat[!is.na(dat$country),]