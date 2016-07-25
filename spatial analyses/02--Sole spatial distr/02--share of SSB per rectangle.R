# a script to calculate, from sole BTS data 2003 - 2013,
# the share of total SSB in each rectangle, annually

  # data should be loaded by superordinate executing script: dat
  dat_backup <- dat
  dat$statrec <- as.character(dat$statrec)
  
  library(dplyr)

# (1) convert sole length to weight
      # fishbase.org: cases in North Sea and Channel only. Accessed Jul 22 15:22:56 2016
      # http://fishbase.de/PopDyn/LWRelationshipList.php?ID=525&GenusName=Solea&SpeciesName=solea&fc=441, 
  fb_a <- 0.0047
  fb_b <- 3.22
  weight_at_length <- function(a, b, length) {   # W = a × Lb
    weight <- a * length^b
    return(weight)
  }
      # Test: Fishbase says Estimate weight for given length: 20 (cm) = 72.68(g)
              weight_at_length(a = fb_a, b = fb_b, length = 20)
              
  dat$weight_per_ind <- weight_at_length(a = fb_a, b = fb_b, length = dat$lngtclass / 10)  # in g [ / 10 = mm to cm]
  dat$weight_per_ind <- dat$weight_per_ind / 1000  # in kg
      # note that this leads to an underestimation of weight, as we use lower boundary of length class
  dat$cpue_kg_per_hour <- dat$weight_per_ind * dat$cpue_number_per_hour
  
  
# (2) aggregate over length classes ----
      # sole are defined SSB if above 300mm (Staebler et al. 2016 - Appendix A)
  dat <- dat[dat$lngtclass >= 300, ]
  
  
# (2) aggregate SSB per rectangle
  dat <- dplyr::group_by(.data = dat, year, statrec)
  dat <- dplyr::summarise(.data = dat,
                   local_ssb_kg = sum(cpue_kg_per_hour))
  
# (3) total SSB per year
  dat_total <- dplyr::group_by(.data = dat, year)
  dat_total <- dplyr::summarise(.data = dat_total,
                         total_annual_ssb = sum(local_ssb_kg))
  dat <- merge(dat, dat_total)
  rm(dat_total)
  dat$share_ssb <- dat$local_ssb_kg / dat$total_annual_ssb
  dat$local_ssb_kg <- NULL
  dat$total_annual_ssb <- NULL
  dat <- dat[!is.na(dat$year),]
  rm(dat_backup, fb_b, fb_a, weight_at_length)
  