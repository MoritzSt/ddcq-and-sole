# This script uses the Schoener index of partial spatial overlap of sole SSB and
# BT effort of BEL and NED as calculated in superordinate scripts
# and establishes the relationship between both.

  working_directory_here <- 'Z:\\02-TI\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole'  # at ZMT PC
  library(reshape2)
  
  
# (1) load sole SSB ----
  # Read B and F (total and per age class) from sole assessment (WGNSSK 2015).
  sole_total <- read.csv(file = paste0(working_directory_here, '\\input\\Sole in Subarea IV.csv'))
  names(sole_total) <- tolower(names(sole_total))
  sole_total$year <- as.integer(as.character(sole_total$year))
  sole_total <- sole_total[!sole_total$year == 2015,]
  sole_total <- sole_total[!is.na(sole_total$year),]
  
  sole_total <- dplyr::select(.data = sole_total, year, ssb)
  
  
# (2) merge sole SSB with Schoener overlap data ----
  dat <- merge(dat, sole_total, all.x = T, all.y = F)
  
  
# (3) calculate Spearman correlations between SSB and Schoener index ----
  
  cor_BEL2 <- cor.test(x = dat$ssb[dat$country == 'BEL'],
                       y = dat$Schoener.I[dat$country == 'BEL'],#
                       method = 'spearman' )
  cor_BEL1 <- cor(x = dat$ssb[dat$country == 'BEL'],
                      y = dat$Schoener.I[dat$country == 'BEL'],#
                      method = 'spearman' )
  
  cor_NED2 <- cor.test(x = dat$ssb[dat$country == 'NED'],
                       y = dat$Schoener.I[dat$country == 'NED'],#
                       method = 'spearman' )
  cor_NED1 <- cor(x = dat$ssb[dat$country == 'NED'],
                  y = dat$Schoener.I[dat$country == 'NED'],#
                  method = 'spearman' )
  
  
# (4) plot relationships between SSB and Schoener index ---- 
  
  x11()
  par(mfrow=c(2,1))

  plot(x = dat$ssb[dat$country == 'BEL'],
       y = dat$Schoener.I[dat$country == 'BEL'],
       col = 2, xlab = 'Sole SSB', ylab = 'Schoener Index',
       main = paste0('Belgium BT: cor ', round(cor_BEL1, digits = 2), '. p = ', round(cor_BEL2$p.value, digits = 3)),
       type = 'b')
  text(x = dat$ssb[dat$country == 'BEL'],
       y = dat$Schoener.I[dat$country == 'BEL'],
       labels = dat$year[dat$country == 'BEL'])
  
  
  plot(x = dat$ssb[dat$country == 'NED'],
       y = dat$Schoener.I[dat$country == 'NED'],
       col = 2, xlab = 'Sole SSB', ylab = 'Schoener Index',
       main = paste0('Dutch BT: cor ', round(cor_NED1, digits = 2), '. p = ', round(cor_NED2$p.value, digits = 3)),
       type = 'b')
  text(x = dat$ssb[dat$country == 'NED'],
       y = dat$Schoener.I[dat$country == 'NED'],
       labels = dat$year[dat$country == 'NED'])

  rm(sole_total, cor_NED2, cor_NED1, cor_BEL2, cor_BEL1, working_directory_here)
  