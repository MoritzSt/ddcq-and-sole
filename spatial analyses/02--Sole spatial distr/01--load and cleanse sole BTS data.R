# a script to load BTS data on sole distribution

  working_directory <- 'Z:\\02-TI\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\spatial analyses\\02--Sole spatial distr'
  
  plotting_bts <- FALSE  # Do you want plots and analyses to be created?
  
  library(dplyr)
  library(ggplot2)

# (1) load and screen BTS data ----
  ori_dat <- read.csv(file = paste(working_directory, 'input', 'CPUE', 'CPUE per length per Hour and Swept Area_2016-07-22 14_00_58.csv', sep = '\\'))
  
  dat <- dplyr::select(.data = ori_dat, Quarter, Country, Gear, Year, StatRec, ICESArea, LngtClass, CPUE_number_per_hour)
  names(dat) <- tolower(names(dat))
  
  if(plotting_bts == TRUE) {
  x11()
  pairs(dat)
  dev.off()

    # --> Only Q3 has data

  # (1.2) Check gear
    dat_1 <- group_by(dat, gear, year)
    dat_1 <- summarise(dat_1,
              sum_cpue = sum(cpue_number_per_hour),
              var_cpue = var(cpue_number_per_hour)
              )
    x11()
    qplot(y = sum_cpue, x = gear, data = dat_1) + facet_wrap( ~ year, scales = 'free_y')
    max(dat_1$sum_cpue[dat_1$gear == 'BT7'])
    qplot(y = var_cpue, x = gear, data = dat_1) + facet_wrap( ~ year, scales = 'free_y')
    rm(dat_1)
    dev.off()
    # --> use gear == BT4 & gear == BT8, as BT7 catches are very low.
  
  
  # (1.3) Check country
    dat_1 <- group_by(dat, country, year)
    dat_1 <- summarise(dat_1,
                       sum_cpue = sum(cpue_number_per_hour),
                       var_cpue = var(cpue_number_per_hour)
    )
    x11()
    qplot(y = sum_cpue, x = country, data = dat_1) + facet_wrap( ~ year, scales = 'free_y')
    qplot(y = var_cpue, x = country, data = dat_1) + facet_wrap( ~ year, scales = 'free_y')
    max(dat_1$sum_cpue[dat_1$country == 'GFR'])
    rm(dat_1)
    dev.off()
    # --> remove country == 'GFR', as it barely contributes data.
    
  # (1.4) Remove potentially spurious data
  }
    dat <- dat[dat$quarter ==3 & dat$country != 'GFR' & dat$gear != 'BT7',]
    dat$quarter <- NULL
    rm(ori_dat, working_directory, plotting_bts)
  
  
