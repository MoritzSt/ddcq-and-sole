# with sole ssb data loaded in 01--load and cleanse sole BTS data.R
# and spatial distribution of SSB calcualted in 02--share of SSB per rectangle.R,
# plot the spatial distribution of sole SSB in the North Sea throughout the years.


  library(maps)
  library(mapdata)
  library(maptools)
  library(geo)
  
  
  # (1) add coordinates via ICES rectangles
  coordinates <- ir2d(ir = dat$statrec, useI = F)
  dat$lat  <- coordinates$lat
  dat$lon  <- coordinates$lon
  
  
  # (2) plot SSB distribution each year and save as png
  
  dat$year <- as.character(dat$year)
  yearlist <- unique(dat$year)
  
  for(i in seq_along(yearlist)) {
    png(filename = paste0(working_directory, '\\output', '\\sole-SSB-spatial-', yearlist[i], '.png'))
    plotdat <- dat[dat$year == yearlist[i],]
    plotdat$annual_share_ssb <- plotdat$share_ssb / max(plotdat$share_ssb)
    
    map(database = 'worldHires', xlim = c(-6,13), ylim = c(48,62), fill = T, col = grey(0.7))
    box(which = 'plot', lty = 'solid', col = 1)
    points(x = plotdat$lon, y = plotdat$lat, cex = 2, pch = 15, col = grey(level = 1 - plotdat$annual_share_ssb, alpha = 0.9))
    dev.off()
    rm(plotdat)
  }
  
  rm(coordinates, yearlist)
  dat$lon <- NULL
  dat$lat <- NULL
  message('Plotting SSB maps DONE.')
  