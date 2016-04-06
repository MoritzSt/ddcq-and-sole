# This EXECUTE script screens the sole fishery of the North Sea for density-dependent
# changes in FPUE (finshing mortality per unit effort, here called catchability,
# or q). If q or FPUE changes with fish abundance, we speak of density dependent
# changes in catchability (ddcq).

# It starts by establishing a time series of FPUE by dividing assessment F by
# efforts of various fleets from STECF. It seeks to establish if these time
# series are rather constant, if the rise with time (indicating technological
# creep) or are dependent on abundance (i.e. SSB or TSB, which indicates ddcq)


# (0) define baselines of analysis ----------------------------------------

  # Do you want to include effort data of dutch beam trawl prior 2003 from WGSAM05?
  prior2003 <- TRUE
  # working_directory <- 'D:\\workfolder\\TI-2016\\ddcq\\ddcq-and-sole-master\\ddcq-and-sole-master'
   working_directory <- 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole'

# (1) Is FPUE of sole constant or does it change with time and/or abundance?----

  # check catch composition of sole in STECF landings. Get data at
      # https://stecf.jrc.ec.europa.eu/ewg1413
      # use snose package to load and convert STECF file to long format
  library(reshape2)
  library(sNoSeR)
  file_location_stecf_landings <- paste(working_directory, 'input', 'STECF 2014', 'Landings_by_ICES_rectangle.csv', sep = '\\')
    # it was: 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\input data\\STECF 2014\\Landings_by_ICES_rectangle.csv'
  landings <- get_stecf_landings_per_rectangle(file = file_location_stecf_landings, nose_only = T, deep = F, fdf = F, format_long = T)
  landings_backup <- landings

    # calculate contribution of sole to total catch per fleet, rectangle and year
  library(dplyr)
  library(magrittr)

        #  ## For testing, specify input values for sNoSeR function aggr_share_of():
        #  data <- landings
        #  id.vars <- c('annex', 'reg_area_cod', 'reg_gear_cod', 'specon', 'vessel_length', 'rectangle', 'country', 'year')
        #  col <- 'species'
        #  case <- 'SOL'
        #  value <- 'landings'
  shares <- aggr_share_of(data = landings, id.vars = c('annex', 'reg_area_cod', 'reg_gear_cod', 'specon', 'vessel_length', 'rectangle', 'country', 'year'),
                value = 'value', col = 'species', case = 'SOL')
  if(max(shares$share, na.rm = TRUE) != 1)  {warnings('Shares calculation failed!')}

  # plot share of sole in total catch and seek a pattern
  library(ggplot2)
  qplot(data = shares[shares$share > 0.1,], share) + geom_histogram() + facet_wrap(~ reg_gear_cod, scales = 'free')
  qplot(data = shares[shares$reg_gear_cod == 'BT2',], share) + geom_histogram(binwidth=0.1) + facet_grid( vessel_length ~ country, scales = 'free')  # experiment with binwidth!
  # --> over 15m appears to be the interesting vessel length,
  #     BEL ENG GER NED (and, surprisingly, SCO) to be the interesting countries
  
  # [!!! Consider plotting share of sole as colour in a map.]
  # Check for BT2. One map per year.
  year_list <- unique(shares$year)[order(unique(shares$year),decreasing = FALSE)]
  subdat1 <- shares[ shares$reg_gear_cod == 'BT2' & shares$vessel_length == 'O15M',]
  country_list <- c('ENG', 'NED', 'GER', 'BEL', 'SCO')
  for(i in seq_along(year_list)) {
      for(j in seq_along(country_list)) {
        subdat2 <- subdat1[subdat1$year == year_list[[i]] & subdat1$country == country_list[[j]], ]
        if(dim(subdat2)[1] > length(unique(subdat1$rectangle))) {  # a warning if there are more cases than rectangles.
          warnings('There are more replicates that unique (rectangle) cases in the subset of data!')
        }
        plot_factor_in_map(data = subdat2, parameter = 'share', ices_rectangle = 'rectangle', visuals = 'colour')
      }
  }
  
   # Non-loop non-package version: Map of shares, O15M, all countries, all years
  
   
  

  # get effort
    # created as csv in file 'P:\\Offlineordner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\create-stecf-effort-csv.R'
  effort <- get_stecf_landings_per_rectangle(file = paste(working_directory, 'input', 'STECF 2014', 'Effort_by_rectangle.csv', sep = '\\'),
                                             nose_only = TRUE, deep = F, fdf = F, format_long = T)
 # It was 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\input data\\STECF 2014\\Effort_by_rectangle.csv', nose_only = TRUE, deep = F, fdf = F, format_long = T)
  effort <- rename(.data = effort, effort = value)
  effort$year <- as.integer(as.character(effort$year))
    # calculate effort per metier and year
  by_metier <- group_by(.data = effort, country, reg_gear_cod, vessel_length, year)
  by_metier <- summarise(.data = by_metier,
                          count = n(),
                          metier_effort = sum(effort, na.rm = TRUE))  # na.rm = T, as we consider NA in STECF effort data to translate to 'no effort in this rectangle'.
  
  # get effort prior 2003 from WGNSSK 2005, Table 7.3.5., North Sea sole: effort and CPUE series
  if(prior2003 == TRUE) {
    older_effort <- read.csv()
  }
  
  # get F per age class from assessment
  fish_mort_path <- paste(working_directory, 'input\\WGNSSK15-Table 10.3.1. North Sea sole. Harvest (F).csv', sep = '\\')
  fishing_mortality <- read.csv(file = fish_mort_path, sep = ';')
  rm(fish_mort_path)
    # It was: 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\WGNSSK15-Table 10.3.1. North Sea sole. Harvest (F).csv', sep = ';')
  names(fishing_mortality) <- c('year', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10')
  fishing_mortality <- melt(data = fishing_mortality, id.vars = 'year')
  fishing_mortality$age <- as.integer(as.character(fishing_mortality$variable))
  fishing_mortality$F_ <- as.numeric(as.character(fishing_mortality$value))
  fishing_mortality$value <- NULL
  fishing_mortality$variable <- NULL
  
  # combine effort and F information and calculate FPUE (or q)
  fishing_mortality_subset <- fishing_mortality[ fishing_mortality$year >= min(effort$year) ,]
  Fandf <- merge(fishing_mortality_subset, by_metier, all.x = TRUE, all.y = TRUE)
    # subset interesting metiers, as indicated by STECF landings analysis above: BT2
  Fandf <- Fandf[ Fandf$reg_gear_cod == 'BT2',]
  Fandf$fpue <- Fandf$F_ / Fandf$metier_effort  # calculate FPUE, i.e. q
  # cleanse data: I want effort to come from at least 5 rectangles per case.
  Fandf <- Fandf[ Fandf$count > 4, ]
  # also remove cases of zero effort
  Fandf <- Fandf[ Fandf$metier_effort > 0, ]
  Fandf <- Fandf[ !is.na(Fandf$year),]
  
  # plot and model FPUE over the years --> Is FPUE ±constant? Are there indications of techn creep?
  qplot(data = Fandf, x = year, y = fpue, colour = country) + geom_point() + facet_grid(age ~ vessel_length, scales = 'free_y')
    # make FPUE relative maximum FPUE and plot again
  FPUEmax <- group_by(.data = Fandf, age, country, vessel_length)
  FPUEmax <- summarise(.data = FPUEmax,
                       fpue_max = max(fpue, na.rm = TRUE))
  FPUEmax <- FPUEmax[ !is.na(FPUEmax$age),]
          # plot that maximum FPUE per age  --> btw.: the lower max FPUE, the more the effort is actually related to sole F, i presume.
          qplot(data = FPUEmax, y = fpue_max, x = age) + geom_point() + facet_wrap(vessel_length ~ country, scales = 'free_y')
  Fandf_relative <- merge(Fandf, FPUEmax, all.x = T, all.y = T)
  Fandf_relative <- mutate(.data = Fandf_relative, fpue_relative = fpue / fpue_max)
  qplot(data = Fandf_relative, x = year, y = fpue_relative, colour = country, cex = count) + geom_point() + facet_wrap(age ~ vessel_length, scales = 'free_y')
    
    # statistical modelling: FPUE ~ year
    correlations <- lapply(split(Fandf, list(Fandf$age, Fandf$country, Fandf$reg_gear_cod, Fandf$vessel_length)), function(X) if(dim(X)[1] >0) { cor.test(y = X$fpue, x = X$year) })
    # make correlations results a df
    correlations_df <- c()
    for(i in seq_along(correlations)) {
      if(!is.null(correlations[[i]])) {
      correlations_df <- rbind(correlations_df, c(
        names(correlations)[i],
        correlations[[i]]$estimate,
        correlations[[i]]$p.value
      ))
      } }
    correlations_df <- as.data.frame(correlations_df)
    correlations_df <- rename(.data = correlations_df, case = V1, p.value = V3)
    correlations_df$case <- as.character(correlations_df$case)
    correlations_df$cor <- as.numeric(as.character(correlations_df$cor))
    correlations_df$p.value <- as.numeric(as.character((correlations_df$p.value)))
    sig_correlations_df <- correlations_df[ correlations_df$p.value < 0.01,]
    time_correlations <- correlations_df
    rm(correlations_df, correlations, sig_correlations_df)
    # --> of the larger BT2, O15M, most BEL GER NED DEN  FPUE increases sig with time.
    
    
  # plot F against effort
  qplot(data = Fandf_relative, x = metier_effort, y = F_, colour = country) + geom_point() + facet_wrap(age ~ vessel_length, scales = 'free_y')
    # larger BT2 only
  qplot(data = Fandf_relative[ Fandf_relative$vessel_length == "O15M",], x = metier_effort, y = F_, colour = country) + geom_point() + facet_grid(age ~ country, scales = 'free')
  
  # [!!!] Add biomass info and plot 
  
  # Read B and F (total and per age class) from sole assessment (WGNSSK 2015).

  sole_total <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\03--Severe stats--JUL2015\\Input\\Stock Assessment Data\\Sole in Subarea IV.csv')
  names(sole_total) <- tolower(names(sole_total))
  sole_total$year <- as.integer(as.character(sole_total$year))
  sole_total <- sole_total[!sole_total$year == 2015,]
  sole_total <- sole_total[!is.na(sole_total$year),]
  
  sole_weight_at_age <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\WGNSSK15-Table 10.2.5. North Sea sole. Stock weight at age (kg).csv', sep = ';')
  names(sole_weight_at_age) <- c('year',paste( c(1:10)))
  sole_weight_at_age <- melt(data = sole_weight_at_age, id.vars = 'year')
  sole_weight_at_age <- rename(.data = sole_weight_at_age, age = variable, weight_at_age = value)
  sole_weight_at_age$age <- as.integer(as.character(sole_weight_at_age$age))
  
  sole_number_at_age <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\WGNSSK15-Table 10.4.1. North Sea sole. Stock numbers (thousands).csv', sep = ';')
  names(sole_number_at_age) <- c('year',paste( c(1:10)))
  sole_number_at_age <- melt(data = sole_number_at_age, id.vars = 'year')
  sole_number_at_age <- rename(.data = sole_number_at_age, age = variable, number_at_age = value)
  sole_number_at_age$age <- as.integer(as.character(sole_number_at_age$age))
  
  sole_b_at_age <- merge(sole_weight_at_age, sole_number_at_age, all.x = T, all.y = T)  
  sole_b_at_age <- mutate(.data = sole_b_at_age, b_at_age = weight_at_age * number_at_age)
  # plot
  qplot(data = sole_b_at_age, y = b_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  qplot(data = sole_b_at_age, y = weight_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  qplot(data = sole_b_at_age, y = number_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  # --> Numbers at age are very variable. Is that real? [!!!]
  
  
  # add biomass info to FPUE data
  sole_b_at_age <- sole_b_at_age[sole_b_at_age$year >= min(effort$year),]
  sole_b_at_age <- sole_b_at_age[sole_b_at_age$year <= max(effort$year),]
  sole_b_at_age$weight_at_age <- NULL
  sole_b_at_age$number_at_age <- NULL
  fpue_bio <- merge(Fandf, sole_b_at_age, all.x = T, all.y = T)
  fpue_bio_relative <- merge(Fandf_relative, sole_b_at_age, all.x = T, all.y = T)
  
  # plot FPUE ~ biomass
  qplot(data = fpue_bio_relative[fpue_bio_relative$vessel_length == 'O15M',],
        y = fpue_relative, x = b_at_age) + geom_point() + facet_wrap(age ~ country, scales = 'free')
  
  # statistical modelling: FPUE ~ biomass
  correlations <- lapply(split(fpue_bio, list(fpue_bio$age, fpue_bio$country, fpue_bio$reg_gear_cod, fpue_bio$vessel_length)), function(X) if(dim(X)[1] >0) { cor.test(y = X$fpue, x = X$b_at_age) })
  # make correlations results a df
  correlations_df <- c()
  for(i in seq_along(correlations)) {
    if(!is.null(correlations[[i]])) {
      correlations_df <- rbind(correlations_df, c(
        names(correlations)[i],
        correlations[[i]]$estimate,
        correlations[[i]]$p.value
      ))
    } }
  correlations_df <- as.data.frame(correlations_df)
  correlations_df <- rename(.data = correlations_df, case = V1, p.value = V3)
  correlations_df$case <- as.character(correlations_df$case)
  correlations_df$cor <- as.numeric(as.character(correlations_df$cor))
  correlations_df$p.value <- as.numeric(as.character((correlations_df$p.value)))
  sig_correlations_df <- correlations_df[ correlations_df$p.value < 0.01,]
  time_correlations <- correlations_df
  rm(correlations_df, correlations, sig_correlations_df)
  
  
  # [!!!]
  # So far: No significant relationship between FPUE and B in sole.
  # 1) Is that changed by being more specific with the rectangles I get effort from?
  # 2) Remove temporal autocorrelation, i.e. potential effect of TC.
  # 3) Does that change if I use B and FPUE of the whole stock? ddcq would then be
  #     because the age structure (and related properties) of the stock changes.
  #   3.b) If so, is maybe FPUE related to some measurable index of age structure?
  # 4) There is, however, a strong hint towards technological creep. Is that of use?
  # 5) Repeat the analysis for plaice and cod.