# a script to
# a) plot the spatial distribution of sole SSB --> #(3)
# b) plot the spatial distribution of NED and BEL BT effort

plotting_ssb = FALSE  # Do you want spatial plots to be produced?
plotting_effort = FALSE  # Do you want effort distribution maps to be created?
working_directory <- 'Z:\\02-TI\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\spatial analyses\\02--Sole spatial distr'
working_directory_0 <- working_directory


# (1) load cleansed sole BTS data 2003 - 2013 ----
  source(file = paste0(working_directory, '\\01--load and cleanse sole BTS data.R'))
  
  
# (2) calculate share of SSB per rectangle, annually
  working_directory <- 'Z:\\02-TI\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\spatial analyses\\02--Sole spatial distr'
  source(file = paste0(working_directory, '\\02--share of SSB per rectangle.R'))
  
  
# (3) plot the spatial distribution of sol SSB ----
  if(plotting_ssb == TRUE) {
  source(file = paste0(working_directory, '\\03--plot SSB distribution.R'))
  }
  ssb_dat <- dat
  rm(dat)

  
# (4) load BT effort data for NLD and BEL ----
  source(file = paste0('Z:\\02-TI\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\spatial analyses\\01--BEL vs NED f\\BEL-vs-NED-f-spatial.R'))
  dat <- dat[dat$country == 'NED' | dat$country == 'BEL', ]  # Dutch and Belgium data only
  effort <- dat[dat$vessel_length == 'O15M', ]  # big ships only
  effort$vessel_length <- NULL
  rm(dat)
  

# (5) calculate Schoener overlap index between SSB and effort per year (for NED and BEL) ----
  dat <- merge(ssb_dat, effort, all.x = T, all.y = T)
  rm(effort, ssb_dat)
  dat$share_ssb[is.na(dat$share_ssb)] <- 0  # If there was SSB, but no effort in an area, BT has probably not fished there.
  dat$relative_effort[is.na(dat$relative_effort)] <- 0  # If there was effort, but no stock, BT probably fished on sth. else.
  
        # TEST: Do all annual relative efforts add to one?
        test <- dplyr::group_by(.data = dat, country, year)
        test <- dplyr::summarise(.data = test,
                                 summed_annual_effort_shares = sum(relative_effort, na.rm = TRUE))
        rm(test)
        
  working_directory <- working_directory_0
  source(file = paste0(working_directory, '\\05--Schoener spatial overlap index.R'))
  
  # plot Schoener-Index over time for both countries
  x11()
  plot(y = dat$Schoener.I[dat$country == 'NED'], x = dat$year[dat$country == 'NED'],
       ylim = range(dat$Schoener.I), col = 4, type = 'l',
       xlab = 'Year', ylab = 'Schoener Index')
  points(y = dat$Schoener.I[dat$country == 'BEL'], x = dat$year[dat$country == 'BEL'],
         ylim = range(dat$Schoener.I), col = 2, type = 'l')
  

# (6) relate Schoener overlap to SSB
  source(file = paste0(working_directory, '\\06--Overlap vs SSB.R'))
  
  
  ### close graphic windows
  dev.off()
  dev.off()