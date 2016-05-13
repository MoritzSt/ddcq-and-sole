# This EXECUTE script screens the sole fishery of the North Sea for density-dependent
# changes in FPUE (finshing mortality per unit effort, here called catchability,
# or q). If q or FPUE changes with fish abundance, we speak of density dependent
# changes in catchability (ddcq).

# It starts by establishing a time series of FPUE by dividing assessment F by
# efforts of various fleets from STECF. It seeks to establish if these time
# series are rather constant, if the rise with time (indicating technological
# creep) or are dependent on abundance (i.e. SSB or TSB, which indicates ddcq).
# ♥


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
  plotting_maps <- F
  if(plotting_maps == T) {
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
  }  # end of plotting_maps
  
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
  
  # get NLD BT effort prior 2003 from WGNSSK 2005, Table 7.3.5., North Sea sole: effort and CPUE series
  if(prior2003 == TRUE) {
    older_effort <- read.csv(stringsAsFactors = FALSE, sep = ';',
      file = paste(working_directory, 'input', 'WGNSSK05--NL BT effort.csv', sep = '\\'))
    names(older_effort) <- tolower(names(older_effort))
      names(older_effort) <- gsub(x = names(older_effort),
                          pattern = "\\.",
                          replacement = "_")
      older_effort <- older_effort[!is.na(older_effort$nl_bt_effort),]
  }
  
  # get BEL BT effort prior 2003 from WGNSSK 2005, Table 7.3.5., North Sea sole: effort and CPUE series
  if(prior2003 == TRUE) {
    older_effort_bel <- read.csv(stringsAsFactors = FALSE, sep = ';',
                             file = paste(working_directory, 'input', 'WGNSSK05--BEL BT effort.csv', sep = '\\'))
    names(older_effort_bel) <- tolower(names(older_effort_bel))
    names(older_effort_bel) <- gsub(x = names(older_effort_bel),
                                pattern = "\\.",
                                replacement = "_")
    older_effort_bel <- older_effort_bel[!is.na(older_effort_bel$bel_bt_effort),]
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
      # remove F where no effort data is available
      years_with_effort <- unique(effort$year)
      fishing_mortality_subset <- fishing_mortality[ is.element(fishing_mortality$year, years_with_effort) ,]
      rm(years_with_effort)
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
    # with a function to turn correlations stored in a list to a data frame
        cor_results_as_df <- function(correlations) { 
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
          correlations_df  # output: correlations_df
        }
    time_correlations <- cor_results_as_df(correlations = correlations)

    # --> of the larger BT2, O15M, most BEL GER NED DEN  FPUE increases sig with time.
    
    
  # plot F against effort
  qplot(data = Fandf_relative, x = metier_effort, y = F_, colour = country) + geom_point() + facet_wrap(age ~ vessel_length, scales = 'free_y')
    # larger BT2 only
  qplot(data = Fandf_relative[ Fandf_relative$vessel_length == "O15M",], x = metier_effort, y = F_, colour = country) + geom_point() + facet_grid(age ~ country, scales = 'free')

  
# (1.2) With additional NLD BT effort prior 2003 --------------------------
if(prior2003 == TRUE) {
  
  # remove F data for years without effort
  years_with_effort2 <- unique(c(unique(older_effort$year), unique(effort$year)))
  fishing_mortality_subset2 <- fishing_mortality[is.element(fishing_mortality$year, years_with_effort2),]
  rm(years_with_effort2)
  
  # use effort of NL, BT2, vessel > 15m from STECF and combine with WGSAM05 effort
  # Note: NLD effort til 2003 from WGSAM05 is in 1000000 HP day
  newer_effort <- by_metier[by_metier$country == 'NED' &
                              by_metier$vessel_length == 'O15M'&
                              by_metier$reg_gear_cod == 'BT2',
                            c(which(names(by_metier) == 'year'),
                              which(names(by_metier) == 'metier_effort'))]
  older_effort$effort_rel_2003 <- older_effort$nl_bt_effort / older_effort$nl_bt_effort[older_effort$year == 2003]
  older_effort$nl_bt_effort <- NULL
  newer_effort$effort_rel_2003 <- newer_effort$metier_effort / newer_effort$metier_effort[newer_effort$year == 2003]
  newer_effort$metier_effort <- NULL
  long_effort <- as.data.frame(merge(older_effort, newer_effort, all.x = T, all.y = T))
  long_ff <- merge(long_effort, fishing_mortality_subset2, all_x = T, all.y = T)
  
  # same for BEL, BT2, vessel > 15m combined with Belgium data from WGNSSK05 
  newer_effort_bel <- by_metier[by_metier$country == 'BEL' &
                                  by_metier$vessel_length == 'O15M'&
                                  by_metier$reg_gear_cod == 'BT2',
                                c(which(names(by_metier) == 'year'),
                                  which(names(by_metier) == 'metier_effort'))]
  older_effort_bel$effort_rel_2003 <- older_effort_bel$bel_bt_effort / older_effort_bel$bel_bt_effort[older_effort_bel$year == 2003]
  older_effort_bel$bel_bt_effort <- NULL
  newer_effort_bel$effort_rel_2003 <- newer_effort_bel$metier_effort / newer_effort_bel$metier_effort[newer_effort_bel$year == 2003]
  newer_effort_bel$metier_effort <- NULL
  long_effort_bel <- as.data.frame(merge(older_effort_bel, newer_effort_bel, all.x = T, all.y = T))

  
  # For NLD, calculate FPUE, i.e. q
  long_ff$fpue <- long_ff$F_ / long_ff$effort_rel_2003
  range(long_ff$fpue)
  qplot(data = long_ff, x = year, y = fpue) + geom_point() + facet_wrap( ~ age, scales = 'free_y')
  
  # statistical modelling with effort between 1978 and 2013: FPUE ~ year
    correlations <- lapply(split(long_ff, list(long_ff$age)), function(X) if(dim(X)[1] >0) { cor.test(y = X$fpue, x = X$year) })
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
    time_correlations_long <- correlations_df
    rm(correlations_df, correlations, sig_correlations_df)
  
}  # end of altered procedure for prior2003 == TRUE.
  

# # (1.3) Add biomass info from assessment --------------------------------
  
  # Read B and F (total and per age class) from sole assessment (WGNSSK 2015).
  sole_total <- read.csv(file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\Sole in Subarea IV.csv')
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
  rm(sole_number_at_age, sole_weight_at_age)
  sole_b_at_age <- mutate(.data = sole_b_at_age, b_at_age = weight_at_age * number_at_age)
  # plot
  qplot(data = sole_b_at_age, y = b_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  qplot(data = sole_b_at_age, y = weight_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  qplot(data = sole_b_at_age, y = number_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  # --> Numbers at age are very variable. Is that real? [!!!]
        
        # Add plaice ssb
        plaice_total <- read.csv2(stringsAsFactors = F, file = paste(working_directory, 'input', 'Plaice Subarea IV.csv', sep = '\\'))
        names(plaice_total) <- tolower(names(plaice_total))
        plaice_total$year <- as.integer(as.character(plaice_total$year))
        plaice_total <- plaice_total[!plaice_total$year == 2015,]
        plaice_total <- plaice_total[!is.na(plaice_total$year),]
        
  
  # add sole biomass info to FPUE data
  sole_b_at_age$weight_at_age <- NULL
  sole_b_at_age$number_at_age <- NULL
  sole_b_at_age_backup <- sole_b_at_age
  sole_b_at_age <- sole_b_at_age[sole_b_at_age$year >= min(effort$year),]
  sole_b_at_age <- sole_b_at_age[sole_b_at_age$year <= max(effort$year),]
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
  
  
# (1.4) FPUE vs biomass for longer NLD BT effort series -------------------
  long_fpue_bio <- merge(long_ff, sole_b_at_age_backup, all.x = T, all.y = F)
  # plot FPUE against biomass for all ages
  qplot(data = long_fpue_bio, x = b_at_age, y = fpue, main = 'FPUE vs B at age 1978-2013') + geom_point() + facet_wrap( ~ age, scales = 'free')
  qplot(data = long_fpue_bio[long_fpue_bio$year<2003,], x = b_at_age, y = fpue, main = 'FPUE vs B at age 1978-2003') + geom_point() + facet_wrap( ~ age, scales = 'free')
  # statistical modells
  correlations <- lapply(split(long_fpue_bio, list(long_fpue_bio$age)), function(X) if(dim(X)[1] >0) { cor.test(y = X$fpue, x = X$b_at_age) })
        # ...consider  >  models <- lapply(split(long_fpue_bio, list(long_fpue_bio$age)), function(X) if(dim(X)[1] >0) { lm(formula = X$fpue ~ X$b_at_age) })
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
  long_fpue_bio_correlations <- correlations_df
  rm(correlations_df, correlations, sig_correlations_df)
  
  
# (1.5)   Develop multiple regression model: FPUE ~ year + biomass_at_age --------
    pairs(long_fpue_bio)
  
  
  # ? Are explanatory variables confound, i.e. is B_at_age linearly correlated with time?
  correlations_expl_vars <- lapply(split(long_fpue_bio, list(long_fpue_bio$age)), function(X) if(dim(X)[1] >0) { cor.test(y = X$year, x = X$b_at_age) })
  correlations_expl_vars <- cor_results_as_df(correlations = correlations_expl_vars)
  # ! --> Yes, they are, for ages 8, 9, 10.
  qplot(data = long_fpue_bio, x = year, y = b_at_age) + geom_point() + facet_wrap( ~ age, scales = 'free_y')

  
  # ? In simple multiple linear regressions, check combined effect of time and B
  models1 <- lapply(split(long_fpue_bio, list(long_fpue_bio$age)), function(X) if(dim(X)[1] >0) { lm(formula = X$fpue ~ X$b_at_age + X$year) })
  # ! --> No single case where B effect is significant. Output see 'output' folder.
  
  # ? First exploratory use of glm
  models2 <- lapply(split(long_fpue_bio, list(long_fpue_bio$age)), function(X) if(dim(X)[1] >0) { glm(formula = X$fpue ~ X$b_at_age + X$year) })  
  
   # ... explore results by using 
    # > summary(models[[i]])
    # > plot(models[[i]])
    # ...where i is the age, e.g. > summary(models[[2]])  # for age 2.
  
  # ? First exploratory use of gam

  
  
  
# (1.6.1) Sole SSB Stats: Netherlands FPUE ~ biomass + year ----------------------------------
  
  dat <- merge(sole_total, long_effort, all.x = T, all.y = F)
  long_effort_bel <- rename(long_effort_bel, effort_bel_rel_2003 = effort_rel_2003)
  dat <- merge(dat, long_effort_bel, all.x = F, all.y = F)
  dat <- select(.data = dat, year, ssb, mean.f, effort_rel_2003, effort_bel_rel_2003)
  dat$fpue <- dat$mean.f / dat$effort_rel_2003
  dat$fpue_bel <- dat$mean.f / dat$effort_bel_rel_2003
  dat$ssb <- as.numeric(as.character(dat$ssb))
  dat$year <- as.numeric(as.character(dat$year))  # This might be a critical point.
  x11()
  pairs(dat)
  library(mgcv)
  qplot(data = dat, fpue) + geom_histogram()  # check distribution of response var
  dev.off()
    # --> looks like gaussian is OK.
  
  
  # Test for temporal autocorrelation in fpue
  x11()
  acf(dat)
  dev.off()
    # --> Autocorrelation up to a six years lag in fpue.
  

  # (1.6.1.A): Test family and shape with GAM -----------------
  model.gaussian <- gam(data = dat, formula = fpue ~ s(ssb, k = -1, fx = F) + year,
               family = gaussian(link = 'log') )
  
  model.gamma <- gam(data = dat, formula = fpue ~ s(ssb, k = -1, fx = F) + year,
                        family = Gamma(link = 'log') )
  
    # --> > family = Gamma  # is better than gaussian because Resids. vs.
    # linear pred. has u-shaped curve.
    # --> Both have bias in Resids. vs. linear pred. towards positive values,
    #     not equal on both sides of y = 0.
    # --> R2 adjusted, however, is higher for gaussian.
    AIC(model.gaussian, model.gamma)
    # --> Gamma is better AIC-wise.
    # --> Automatic k' of 9 seems OK for the model, k-index is 0.963

    # Are residuals OK?
    ad.test(resid(model.gaussian))  # --> Nor normally dist for gaussian
    cvm.test(resid(model.gaussian))
    ad.test(resid(model.gamma))
    cvm.test(resid(model.gamma))  # --> Normally distr for Gamma
    x11()
    acf(resid(model.gamma))
    dev.off()  # Residuals autocorrelated.
    
    # check shape of the relationship
    x11()
    plot(model.gamma)  # --> linear SSB-effect on lin-log scale
    summary(model.gamma)  # --> ...and edf is 1, so linear effect makes sense
    dev.off()
    first.model.gamma <- model.gamma
    
        
        # What if I smooth year as well and do not assume linear effect of year?
        model <- gam(data = dat, fpue ~ s(ssb) + s(year),
                     family = Gamma(link = 'log'))
        summary(model)
        x11()
        par(mfrow=c(2,2))
        plot(model)
        dev.off()
          # The reasoning why this is not leading me futher needs to be clearer.
    
        
    # (1.6.1.B): Does consideration of plaice catches and F help? ---------
    plaice2 <- select(.data = plaice_total, year, ssb, mean.f)
    plaice2$ssb <- as.numeric(as.character(plaice2$ssb))
    plaice2$mean.f <- as.numeric(as.character(plaice2$mean.f))
    plaice2 <- rename(.data = plaice2, ssb_ple = ssb, mean.f_ple = mean.f)
    dat_with_ple <- merge(dat, plaice2, all.x = T, all.y = F)
    rm(plaice2)
    dat_with_ple_all_years <- dat_with_ple
    write.csv(x = dat_with_ple, file = paste0(working_directory, '\\output\\dat_with_ple.csv'))
    dat_with_ple <- dat_with_ple[!is.na(dat_with_ple$fpue),]
    cor(dat_with_ple$fpue, dat_with_ple$mean.f_ple)
    x11()
    plot(dat_with_ple$fpue ~ dat_with_ple$mean.f_ple)
    dev.off()
    
    model.gamma <- gam(data = dat_with_ple,
                       formula = fpue ~ s(ssb, k = -1, fx = F) + year + s(mean.f_ple),
                       family = Gamma(link = 'log') )
    summary(model.gamma)
    x11()
    par(mfrow=c(1,2))
    plot(model.gamma)
    par(mfrow=c(2,2))
    gam.check(model.gamma)
    ad.test(resid(model.gamma))
    cvm.test(resid(model.gamma))  # --> OK, resids normally distributed
    par(mfrow=c(1,1))
    acf(resid(model.gamma))  # --> resids look better that first.model.gamma
    AIC(model.gamma, first.model.gamma)  # --> AIC-wise, considering F.ple helps a lot!
    anova(model.gamma, first.model.gamma)  # --> cannot interpret that [!!!]
    dev.off()
    # --> Statistically (but check conceptually [!!!]),
    #     consideration of plaice F is a good idea.
    # --> Effect of both sole ssb and plaice F could be considered linear. No log.
    
    # Test log = 'identity' also
    model.ident <- gam(data = dat_with_ple,
                       formula = fpue ~ s(ssb, k = -1, fx = F) + year + s(mean.f_ple),
                       family = Gamma(link = 'identity') )   
    AIC(model.gamma, model.ident)  # --> almost identical AIC
    x11()
    par(mfrow=c(1,2))
    plot(model.ident)
    par(mfrow=c(2,2))
    gam.check(model.ident)
    ad.test(resid(model.ident))
    cvm.test(resid(model.ident))  
    par(mfrow=c(1,1))
    summary(model.ident)
    acf(resid(model.ident))  # --> everything looks very much the same like log link.
    # --> linearity of ssb and almost linearity of F_ple on FPUE is consistent
    
    # Test gaussian also
        model.gaussian <- gam(data = dat_with_ple,
                       formula = fpue ~ s(ssb, k = -1, fx = F) + year + s(mean.f_ple),
                       family = gaussian(link = 'identity') )   
    AIC(model.gamma, model.gaussian)  # --> gaussian worse
    x11()
    par(mfrow=c(1,2))
    plot(model.gaussian)
    par(mfrow=c(2,2))
    gam.check(model.gaussian)
    ad.test(resid(model.gaussian))
    cvm.test(resid(model.gaussian))  # --> residual distribution normal  
    par(mfrow=c(1,1))
    acf(resid(model.gaussian))  # --> multicollinearity almost gone!
    summary(model.gaussian)  # --> significances and curve shapes consistent.
    dev.off()
    # --> again: linearity of ssb and almost linearity of F_ple on FPUE is consistent
    

    # (1.6.1.B.+) correct autocor from gam(FPUE ~ SSB + year + F_ple) ----
    
      # ...starting with gaussian, as that had lowest autocorr before
      # first the baseline, i.e. normal gam as gamm
      model.baseline<- gamm(data = dat_with_ple,
                       formula = fpue ~ s(ssb) + year + s(mean.f_ple),
                       family = gaussian(link = 'identity'))

              model <- gamm(data = dat_with_ple,
                       formula = fpue ~ s(ssb) + s(mean.f_ple),
                       family = gaussian(link = 'identity'),
                       correlation=corAR1(form=~year))
    model.gaussian.autocor <- model
    AIC(model$lme, model.baseline$lme)  # --> autoregressive term helps
    x11()
    ad.test(resid(model$lme, type = 'normalized'))
    cvm.test(resid(model$lme, type = 'normalized'))  # --> Resid.s non-normally distributed
    acf(resid(model$lme, type = 'normalized'))  # --> No autocorrealtion in resids
    par(mfrow=c(2,1))
    plot(model$gam)
    summary(model$gam)
    summary(model$lme)  # --> minimallest R2, no sig terms [!!!]
    
    ### --->  Maybe the gaussian model (because very low autocorrelation)
    ###       without autoregressive term is the best it gets.
   
    
# 1.6.1.C  try Fple/Fsol as plaice effect in GAM ----
    model <- gam(data = dat_with_ple,
                          formula = fpue ~ s(ssb) + year + s(I(mean.f_ple / mean.f)),
                          family = gaussian(link = 'identity') )
    AIC(model, model.gaussian)  # --> AIC much better
    x11()
    par(mfrow=c(1,2))
    plot(model)
    par(mfrow=c(2,2))
    gam.check(model)
    ad.test(resid(model))
    cvm.test(resid(model))  # --> residual distribution not normal  
    par(mfrow=c(1,1))
    acf(resid(model))  # --> multicollinearity
    summary(model)  # --> significances and curve shapes consistent.
    dev.off()
    
    
# (1.6.1.D) use glm ------------
    model <- glm(data = dat, fpue ~ log(ssb) + year, family = gaussian(link = 'identity'))
    summary(model)
    plot(model)
    ad.test(resid(model))
    cvm.test(resid(model))
    # --> Resis not normally distributed
    
    model <- glm(data = dat, fpue ~ log(ssb) + year, family = Gamma(link = 'identity'))
    summary(model)
    plot(model)
    ad.test(resid(model))
    cvm.test(resid(model))  # --> Resids normally distributed
    acf(resid(model))  # --> Resids autocorrelated
    model_no_plaice <- model  # safe model for comparison
    
    
    # include plaice F in glm ----
    model <- glm(data = dat_with_ple,
                 fpue ~ log(ssb) + year + log(mean.f_ple),
                 family = Gamma(link = 'identity'))
    summary(model)
    x11()
    par(mfrow=c(2,2))
    plot(model)
    ad.test(resid(model))
    cvm.test(resid(model))  # --> Resids normally distributed
    par(mfrow=c(2,1))
    acf(resid(model))
    acf(resid(model_no_plaice)) # --> Still autocor resid.s, but few and less
    AIC(model, model_no_plaice)  # --> AIC with plaice better than without
    
    
    # correct glm for autocorrelation ----
    library(nlme)
      # without correction for autocor, just rebuilding GLM
    Mglsbase<-gls(model=fpue~log(ssb)+year, data=dat[dat$year>1977,], method="REML")
    plot(dat$year[dat$year>1977], resid(Mglsbase, type="normalized"))
    acf(resid(Mglsbase, type="normalized"))
      # with correction for autocor
    Mgls<-gls(model=fpue~log(ssb), data=dat[dat$year>1977,], correlation=corAR1(form=~year), method="REML")
    plot(Mgls)
    plot(dat$year[dat$year>1977], resid(Mgls, type="normalized"))
    
    summary(Mgls)
    ad.test(resid(Mgls, type="normalized"))
    cvm.test(resid(Mgls, type="normalized"))  # --> Resids not normally distr.
    acf(resid(Mgls, type="normalized"))    
      # with plaice F
    model<-gls(model=fpue~log(ssb) + log(mean.f_ple),
               data=dat_with_ple,
               correlation=corAR1(form=~year), method="REML")
    acf(resid(model, type="normalized"))  # --> Resids still autocorrealted
    ad.test(resid(model, type="normalized"))  # --> Resids not normally distributed
    summary(model)
    dev.off()
    

# (1.6.2) Sole SSB Stats: Belgium FPUE ~ biomass + year --------------------------

    # Test for temporal autocorrelation in fpue
    x11()
    acf(dat$fpue_bel)
    dev.off()
    # --> Autocorrelation up to a four years lag in fpue.
    
    # Test family
    model.gaussian <- gam(data = dat, formula = fpue_bel ~ s(ssb, k = -1, fx = F) + year,
                          family = gaussian(link = 'log') )
    
    model.gamma <- gam(data = dat, formula = fpue_bel ~ s(ssb, k = -1, fx = F) + year,
                       family = Gamma(link = 'log') )
    AIC(model.gaussian, model.gamma)
    # --> resids vs linear pred more evenly distributed for Gamma.
    # --> histogram of resids less biased with Gamma.
    # --> lower AIC with Gamma
    # --> k' OK for Gamma modell, with k-index > 1
    summary(model.gamma)
    # --> Both terms sig.!
    

# (3.1) Sole vs NED: mechanistic model ---------------------------------------------------------------------
  # Inspect if the mechanistic model (formula) to account for both
  # technological creep (TC) and density-dependent changes in catchability
  # (ddqc) is a linear equation:
  
  # The model when no TC or ddq is asumed is 
  # Ftp = ft * q  ,
  # where Ftp is partial F caused by the respective fleet inspected at time t, ft is relative effort,
  # and q is catchability (assumed constant over time). This assumes a linear relationship between 
  # effort and catch (and F alike) as derived from Schaefer 1957 and used in most empirical stock assessments.
  
  # Inclusion of TC in this model is
  # Ftp = (1 + creep * t)  * ft * q,  where creep is annual average rate of increase in fishing power through
  # technological creep and t the time past start year of analysis in years.
  
  # The model to account for ddq is 
  # Ft = ft * QRo / [1 + (QRo - 1) * Bt / Bo] ,
  # where QR0  =  qmax / q0   , and ft is relative fishing effort, where f0 = Fp0, i.e. efforts are sclead so base q0 = 1,
  # where Fp0 is partial F caused by the fleet at t=0).
  # See also EwE help file, 'density-dependent changes in catchability.
  
  # Combining TC and ddq in a model results in
  # Ft = (1 + creep * t) * ft * QR0 / [1 + (QR0 -1) * Bt/B0]
  
  
  # fit maximum model: including TC and ddcq:
  # Ft = (1 + creep * t) * ft * QR0 / [1 + (QR0 -1) * Bt/B0]
  # where Ft is Catch/B at point t in time, and QR0 is qmax/q1991, and q is catchability.


  # Create relative fishing efforts ft, where fo = Fo (efforts scaled so base qo = 1).
    
  # for NLD, create scaled effort f = F0 (efforts scaled so base q0=1; by scaling effort of fleet relative F at t0, i.e. 1991)
  dat$f_scaled <- dat$mean.f[dat$year == 1991] * (dat$effort_rel_2003 / dat$effort_rel_2003[dat$year == 1991])  # scale effort to F1991
  
  # with TC
  nls.model1 <- nls(mean.f ~ (1 + creep * (year - min(dat$year[!is.na(dat$f_scaled)]))) * 
                    f_scaled * qr0 / (1 + (qr0 - 1) * ssb / dat$ssb[dat$year == 1991]),
                    data = dat, start = c(qr0 = 2, creep = 0.1))
  
    summary(nls.model1)
    # Omid:
    # check normality:
    library(nortest)
    ad.test(resid(nls.model1))
    cvm.test(resid(nls.model1))
    # --> You could use this model type, but delete insig. term: creep.
  
  # use log transformation
    # Rabea: Do not log-transform response variable. Rather use link (if using GLM or GAM  etc.)
  nls.model2 <- nls(log(mean.f) ~ log((1 + creep * (year - min(dat$year[!is.na(dat$f_scaled)]))) * f_scaled * qr0 / (1 + (qr0 - 1) * ssb / dat$ssb[dat$year == 1991])),
                    data = dat, start = c(qr0 = 2, creep = 2),
                    trace = TRUE)
  # without TC: Ft = ft QRo / [1 + (QRo - 1) Bt / Bo]
  # Ft = ft QRo / [1 + (QRo - 1) Bt / Bo] ;
  nls.model3 <- nls(mean.f ~ f_scaled * qr0 / (1 + (qr0 - 1) * ssb / dat$ssb[dat$year == 1991]),
                   data = dat, start = c(qr0 = 2))
  
  plot(nls.model3)
  summary(nls.model3)
  ad.test(resid(nls.model3))
  cvm.test(resid(nls.model3))
  # --> Residuals are ok. Normally distributed and (relatively) homoscedastic.
  par(mfrow=c(2,1))
  plot(resid(nls.model3) ~ dat$year[dat$year >1977], type = 'h')
  plot(dat$mean.f[dat$year >1977] ~ dat$year[dat$year >1977])
  points(predict(nls.model3) ~ dat$year[dat$year >1977], col = 'red')
  
  # Is there temporal autocorrelation in the residuals?
    acf(resid(nls.model3))  # but this is maybe not the best test.
  dev.off()  
  
  # Test for autocorrealtion of redsiduals based on linear regression.
  # You can regress the consecutive residuals against each other and test for
  # a significant slope. If there is auto-correlation, then there should be a
  # linear relationship between consecutive residuals. 
  # This is called a Breusch-Godfrey test for autocorrelation.
  # https://stats.stackexchange.com/questions/14914/how-to-test-the-autocorrelation-of-the-residuals
  res <- resid(nls.model3)
  n <- length(res)
  acf_model <- lm(res[-n] ~ res[-1])
  summary(acf_model)
  # -> Highly significant, so there might be autocorrealtion in residuals.
  
  # Also try Durbin-Watson test, implemented in the lmtest and the car package:
  library(lmtest)
  dwtest(nls.model3)
  library(car)
  durbinWatsonTest(nls.model3)
  # ...both don't work, DW-test only works for linear models.
  testmodel <- lm(data = dat, fpue ~ ssb + year)
  dwtest(testmodel)
  
  # --> Apart from maybe residuals' autocorrelation, model is ok.
  
  
  # 3.1.2: Adress autocorrelated residuals in nls.
  plot(resid(nls.model3) ~ dat$year[dat$year>1977])
  
  
  # plot obs vs pred
  qr0 <- 1.9167
  creep <- 0
  dat$pred_f <- c((1 + creep * (dat$year - min(dat$year[!is.na(dat$f_scaled)])) ) * dat$f_scaled * qr0 /  (1 + (qr0 - 1) * 
                          dat$ssb / dat$ssb[dat$year == 1991]))
  plot(dat$pred_f ~ dat$mean.f,
       main = paste0('R² = ', round(digits = 3, cor.test(dat$pred_f, dat$mean.f, method = 'pearson')$estimate ^2))  )
  abline(a = 0, b = 1, col = 'red')
  # which model is better?
  AIC(nls.model1, nls.model3)
  dev.off()
  
  
# (3.2) add F_plaice to mechanistic model SOL vs NLD ----
    # for NLD and with PLE, create scaled effort
  dat_with_ple$f_scaled <- dat_with_ple$mean.f[dat_with_ple$year == 1991] *
    (dat_with_ple$effort_rel_2003 / dat_with_ple$effort_rel_2003[dat_with_ple$year == 1991])  # scale effort to F1991
  
    # Use a linear relationship of sole F with plaice F
  model <- nls(mean.f ~ (1 + creep * (year - min(dat_with_ple$year[!is.na(dat_with_ple$f_scaled)]))) * 
                    f_scaled * qr0 / (1 + (qr0 - 1) * ssb / dat_with_ple$ssb[dat$year == 1991]) +
                    ple_effect * (mean.f_ple / f_scaled),
                    data = dat_with_ple, start = c(qr0 = 2, creep = 0.1, ple_effect = 0))
  
  summary(model)  # --> FPUE_sol increases with FPUE_ple... not expected!
                  # ... but maybe the reason is that, whatever increases FPUE of
                  #     the one would also act on the other.
                  x11()
                  plot(y = dat_with_ple$fpue, x = dat_with_ple$mean.f_ple / dat_with_ple$f_scaled)
                  cor.test(y = dat_with_ple$fpue, x = dat_with_ple$mean.f_ple / dat_with_ple$f_scaled)
                  # ... not really that simple...
                  dev.off()
                  
  acf(resid(model))  # --> residuals autocorrelated...
  plot(resid(model), type = 'h')
  ad.test(resid(model))  # --> ...and not normally distributed.
  x11()
  par(mfrow=c(2,1))
  plot(resid(model) ~ dat_with_ple$year, type = 'h')
  plot(dat_with_ple$mean.f ~ dat_with_ple$year)
  points(predict(model) ~ dat_with_ple$year, col = 'red')
  message(paste0('R2: ',   # calculate R2
          round(digits = 3, x = cor.test(predict(model), dat_with_ple$mean.f,
                         method = 'pearson')$estimate ^2)))
 dev.off()
  
# (3.3) Try using F_ple / F_sol instead FPUE_sol to determine plaice effect ----
  x11()
  plot(x = dat_with_ple$year, y = dat_with_ple$mean.f_ple / dat_with_ple$mean.f)
 
   model <- nls(mean.f ~ (1 + creep * (year - min(dat_with_ple$year[!is.na(dat_with_ple$f_scaled)]))) * f_scaled *
                qr0 / (1 + (qr0 - 1) * ssb / dat_with_ple$ssb[dat$year == 1991]) +
                ple_effect * (mean.f_ple / mean.f),
               data = dat_with_ple, start = c(qr0 = 2, creep = 0.1, ple_effect = 0))
   
   plot(model)  # there is still some trend in the residuals
   par(mfrow=c(2,1))
   plot(resid(model) ~ dat_with_ple$year, type = 'h')
   plot(dat_with_ple$mean.f ~ dat_with_ple$year)
   points(predict(model) ~ dat_with_ple$year, col = 'red')
   message(paste0('R2: ',   # calculate R2
                  round(digits = 3, x = cor.test(predict(model), dat_with_ple$mean.f,
                                                 method = 'pearson')$estimate ^2)))
   
   ad.test(resid(model))
   cvm.test(resid(model))  # --> residuals normally distributed
   acf(resid(model))  # ---> residuals autocorrelated
   AIC(model, nls.model3)  # --> model performs better than without plaice consideration
   anova(model, nls.model3)  # --> sign better with ple effect
   summary(model)
   par(mfrow=c(2,2))
   plot(y = predict(model), x = dat_with_ple$ssb)
   plot(y = predict(model), x = dat_with_ple$year)
   plot(y = predict(model), x = dat_with_ple$mean.f_ple / dat_with_ple$mean.f)
   dev.off()
   
   
  
  
# (4) GAM for sole vs BEL BT ---------------------------------------------------------------------
  
  # data checks
  acf(dat$fpue_bel, na.action = na.omit)  # --> temporal autocorrelation til year 4
  # cross correlation
  cross_corr <- select(.data = dat, year, ssb, effort_bel_rel_2003, fpue_bel)
  cor(cross_corr)
  cor.test(dat$fpue_bel, dat$year)  # sig cor
  cor.test(dat$ssb, dat$year)  # non sig
  cor.test(dat$fpue_bel, dat$ssb)  # sig cor [!!!]
  
  # test family
  qplot(data = dat, fpue_bel) + geom_histogram()  # looks gammaesque
  model1 <- gam(data = dat, fpue_bel ~ s(ssb) + year,
                family = Gamma(link = 'log'))
  model2 <- gam(data = dat, fpue_bel ~ s(ssb) + year,
                family = gaussian(link = 'log'))
  # --> Resids vs linear pred  less biased towards positive values in Gamma
  AIC(model1, model2)
  # --> AIC way lower for Gamma
  # --> k' of 9 seems ok, with k-index > 1.
  plot(model1)
  summary(model1)
  # --> all terms sig in Gamma model.
  # --> curve with local max, no straight line as for NED,
  #   > but can be forced flat with low k.

  
# (4.1) Sole vs BEL: mechanistic model  ----------------------------------
  
  # for BEL, create scaled effort f = F0 (efforts scaled so base q0=1; by scaling effort of fleet relative F at t0, i.e. 1991)
  dat_backup <- dat
  dat <- dat[dat$year >= 1990,]
  dat$f_scaled <- dat$mean.f[dat$year == 1991] * (dat$effort_bel_rel_2003 / dat$effort_bel_rel_2003[dat$year == 1991])  # scale effort to F1991
  
  # with TC
  nls.model1 <- nls(mean.f ~ (1 + creep * (year - min(dat$year[!is.na(dat$f_scaled)]))) *
                    f_scaled * qr0 / (1 + (qr0 - 1) * ssb / dat$ssb[dat$year == 1991]),
                    data = dat, start = c(qr0 = 2, creep = 0.1))
                    # use log transformation
                    nls.model2 <- nls(log(mean.f) ~ log((1 + creep * (year - min(dat$year[!is.na(dat$f_scaled)]))) * f_scaled * qr0 / (1 + (qr0 - 1) * ssb / dat$ssb[dat$year == 1991])),
                                      data = dat, start = c(qr0 = 2, creep = 2),
                                      trace = TRUE)
  # without TC: Ft = ft QRo / [1 + (QRo - 1) Bt / Bo]
  # Ft = ft QRo / [1 + (QRo - 1) Bt / Bo] ;
  nls.model3 <- nls(mean.f ~ f_scaled * qr0 / (1 + (qr0 - 1) * ssb / dat$ssb[dat$year == 1991]),
                    data = dat, start = c(qr0 = 2))
  # plot obs vs pred
  qr0 <- 6.592
  creep <- 0
  dat$pred_f <- c((1 + creep * (dat$year - min(dat$year[!is.na(dat$f_scaled)])) ) * dat$f_scaled * qr0 /  (1 + (qr0 - 1) * 
                                                                                                             dat$ssb / dat$ssb[dat$year == 1991]))
  plot(dat$pred_f ~ dat$mean.f,
       main = paste0('R² = ', round(digits = 3, cor.test(dat$pred_f, dat$mean.f, method = 'pearson')$estimate ^2))  )
  abline(a = 0, b = 1, col = 'red')
  # which model is better?
  AIC(nls.model1, nls.model3)
  
  
  
# (5) What if I GAM-modelled F ~ f + ssb + year? ------------------------------
  
  # check distribution of response var
  range(dat$mean.f)  # between 1 and 0.
  qplot(data = dat, mean.f) + geom_histogram()
  # --> [???] What's that distribution?


  # Test response variable for temporal autocorrelation
  acf(dat$mean.f)
  # --> Massive...
  
  # Use GAM (to compare with simplest GAMM)
  model00 <- gam(data = dat, formula = mean.f ~ s(ssb) + s(effort_rel_2003) + year,
                          family = Gamma(link = 'log') )
  gam.check(model0)
  plot(model0)
  summary(model0)
  
  # The simplest GAMM with no autoregressive term
  model0 <- gamm(data = dat, formula = mean.f ~ s(ssb) + s(effort_rel_2003) + year,
                     family = Gamma(link = 'log'))
  # --> summary(model0$gam)  looks quite like  summary(model00),
  #     so a GAMM with no autoregressive term is a good comparison to the GAM, but:
  AIC(model00)  # is much lower than:
  AIC(model0$lme)
  
  # Does inclusion of autoregressive term to account for temporal autocorrelation help?
  model1 <- gamm(data = dat, formula = mean.f ~ s(ssb) + s(effort_rel_2003) + year,
                 family = Gamma(link = 'log'),
                 correlation = corARMA(form = ~ year, p = 1))
  gam.check(model1$gam)
    # --> Bizarre patterns of residuals !!! .
    #     Is it better with family = gaussian?
  model1_gaussian <- gamm(data = dat, formula = mean.f ~ s(ssb) + s(effort_rel_2003) + year,
                 family = gaussian(link = 'log'),
                 correlation = corARMA(form = ~ year, p = 1))
  gam.check(model1_gaussian$gam)  
  AIC(model1_gaussian$lme, model1$lme)
    #     Looks better with 'gaussian', but still not good. Better than FPUE ~ ... , though.
  plot(model1_gaussian$gam)  # --> predictions look ok
  AIC(model0$lme, model1_gaussian$lme)  # --> Yes, AIC is lower WITH it.
  summary(model1_gaussian$gam)  # --> Only effort is significant. Year effect is negative!
  
  # Make effort effect linear, not smoothed.
  model2 <- gamm(data = dat, formula = mean.f ~ s(ssb) + effort_rel_2003 + year,
                 family = gaussian(link = 'log'),
                 correlation = corARMA(form = ~ year, p = 1))
  gam.check(model2$gam)
    # --> No nice residuals. Quite similar to gaussian with smoothed effort.
  AIC(model1_gaussian$lme, model2$lme)  # --> No big difference AIC-wise either.
  summary(model2$gam)  # --> Still only effort sig., year effect still negative.
  
  # Test linear effect of ssb.
  model2b <- gamm(data = dat, formula = mean.f ~ ssb + s(effort_rel_2003) + year,
                 family = gaussian(link = 'log'),
                 correlation = corARMA(form = ~ year, p = 1))
  gam.check(model2b$gam)
  # --> No nice residuals.
  AIC(model1_gaussian$lme, model2b$lme)  # --> Sort of better.
  summary(model2$gam)  # --> Only effort sig., year effect still negative.
  

  # Kick insignificant terms.
  # Kick year.
  model3 <- gamm(data = dat, formula = mean.f ~ s(ssb) + effort_rel_2003,
                 family = gaussian(link = 'log'),
                 correlation = corARMA(form = ~ year, p = 1))
  gam.check(model3$gam)
    # --> Barely better, but there could now be a trend in resid.s vs predictor.
  AIC(model2$lme, model3$lme)  # --> No big difference AIC-wise.
  summary(model3$gam)  # --> Still only effort sig..
  
  # Kick ssb.
  model4 <- gamm(data = dat, formula = mean.f ~ s(effort_rel_2003),
                 family = gaussian(link = 'log'),
                 correlation = corARMA(form = ~ year, p = 1))
  gam.check(model4$gam)
  # --> Barely better, but there could now be a trend in resid.s vs predictor.
  AIC(model4$lme, model3$lme)  # --> AIC-wise better.
  summary(model4$gam)
  
  
  
  
# (6) plaice vs NED gam  -------------------------------------------------
  
  # create dataset
  plaice <- select(.data = dat, year, effort_rel_2003, effort_bel_rel_2003)
  plaice2 <- select(.data = plaice_total, year, ssb, mean.f)
  plaice2$ssb <- as.numeric(as.character(plaice2$ssb))
  plaice2$mean.f <- as.numeric(as.character(plaice2$mean.f))
  plaice <- merge(plaice, plaice2, all.x = F, all.y = F)
  rm(plaice2)
  plaice$fpue_nld <- plaice$mean.f / plaice$effort_rel_2003
  plaice$fpue_bel <- plaice$mean.f / plaice$effort_bel_rel_2003
  
  # data checks
  acf(plaice[plaice$year > 1977,])  # --> temporal autocorrelation in ALL variables,
                                    #   > but fairly week in fpue (lag 1 only)
    # cross correlation
  cor(plaice, use = 'na.or.complete')
  cor.test(plaice$fpue_nld, plaice$year, use = 'na.or.complete')  # sig
  cor.test(plaice$fpue_nld, plaice$ssb, use = 'na.or.complete')  # non sig
  
  cor.test(plaice$fpue_bel, plaice$year, use = 'na.or.complete')  # non sig
  cor.test(plaice$fpue_bel, plaice$ssb, use = 'na.or.complete')  # non sig
  
  cor.test(plaice$ssb, plaice$year, use = 'na.or.complete', method = 'pearson')  # evtl sig
 
  
# (6.1) GAM for PLE vs NLD  ----------------------------------------------
  
  # test family
  qplot(data = plaice, fpue_nld) + geom_histogram()  # More likely Gamma
  model1 <- gam(data = plaice, fpue_nld ~ s(ssb) + year,
                family = Gamma(link = 'log'))
  model2 <- gam(data = plaice, fpue_nld ~ s(ssb) + year,
                family = gaussian(link = 'log'))
  
  gam.check(model1)
  gam.check(model2)
  # --> qq plots slightly better for gaussian,
  #   > resids vs linear better for Gamma
  AIC(model1, model2)
  # --> AIC slightly lower for Gamma
  # --> k' of 9 is good, k-index is > 1 for both families.
  plot(model1)
  summary(model1)
  # --> All terms sig in Gamma model.
  # --> [!!!] Both models show descending q with ssb at low ssb,
  #   > but rising q at high ssb [!!!]
  # --> Both models say ssb is insignificant [!!!] while year is.
  
  
# (6.2) GAM for PLE vs BEL  -------------------------------------------------------
  
  # test family
  qplot(data = plaice, fpue_bel) + geom_histogram()  # Could be anything from visuals
  model1 <- gam(data = plaice, fpue_bel ~ s(ssb) + year,
                family = Gamma(link = 'log'))
  model2 <- gam(data = plaice, fpue_bel ~ s(ssb) + year,
                family = gaussian(link = 'log'))
  
  gam.check(model1)
  gam.check(model2)
  # --> qq plot and resids vs linear better for gaussian
  AIC(model1, model2)
  # --> AIC alike
  # --> k' of 9 is ok, k-index > 1 for both.
  plot(model2)
  summary(model2)
  # --> SSB not sig in both models.


# (7.1) PLE vs NLD: nls Mechanistic model  ------------------------------------
  
  # for NLD, create scaled effort f = F0 (efforts scaled so base q0=1; by scaling effort of fleet relative F at t0, i.e. 1991)
  plaice$f_scaled <- plaice$mean.f[plaice$year == 1991] * (plaice$effort_rel_2003 / plaice$effort_rel_2003[plaice$year == 1991])  # scale effort to F1991
  
  # with TC
  nls.model1 <- nls(mean.f ~ (1 + creep * (year - min(plaice$year[!is.na(plaice$f_scaled)]))) * f_scaled * qr0 / (1 + (qr0 - 1) * ssb / plaice$ssb[plaice$year == 1991]),
                    data = plaice, start = c(qr0 = 2, creep = 0.1))
  # use log transformation
  nls.model2 <- nls(log(mean.f) ~ log((1 + creep * (year - min(plaice$year[!is.na(plaice$f_scaled)]))) * f_scaled * qr0 / (1 + (qr0 - 1) * ssb / plaice$ssb[plaice$year == 1991])),
                    data = plaice, start = c(qr0 = 2, creep = 2),
                    trace = TRUE)
  # without TC: Ft = ft QRo / [1 + (QRo - 1) Bt / Bo]
  # Ft = ft QRo / [1 + (QRo - 1) Bt / Bo] ;
  nls.model3 <- nls(mean.f ~ f_scaled * qr0 / (1 + (qr0 - 1) * ssb / plaice$ssb[plaice$year == 1991]),
                    data = plaice, start = c(qr0 = 2))
  # plot obs vs pred
  qr0 <- 1.038
  creep <- 0
  plaice$pred_f <- c((1 + creep * (plaice$year - min(plaice$year[!is.na(plaice$f_scaled)])) ) * plaice$f_scaled * qr0 /  (1 + (qr0 - 1) * 
                                                                                                             plaice$ssb / plaice$ssb[plaice$year == 1991]))
  plot(plaice$pred_f ~ plaice$mean.f,
       main = paste0('R² = ', round(digits = 3, cor.test(plaice$pred_f, plaice$mean.f, method = 'pearson')$estimate ^2))  )
  abline(a = 0, b = 1, col = 'red')
  # which model is better?
  AIC(nls.model1, nls.model3)
  
  # --> QR0 significant, but not largely different from 1.
  #   > As comparison, GAM says that ssb has no sig influence.
  
  
  # (7.2) PLE vs BEL: nls Mechanistic model  ------------------------------------
  
  # for BEL, create scaled effort f = F0 (efforts scaled so base q0=1; by scaling effort of fleet relative F at t0, i.e. 1991)
  plaice$f_scaled <- plaice$mean.f[plaice$year == 1991] * (plaice$effort_bel_rel_2003 / plaice$effort_bel_rel_2003[plaice$year == 1991])  # scale effort to F1991
  
  # with TC
  nls.model1 <- nls(mean.f ~ (1 + creep * (year - min(plaice$year[!is.na(plaice$f_scaled)]))) * f_scaled * qr0 / (1 + (qr0 - 1) * ssb / plaice$ssb[plaice$year == 1991]),
                    data = plaice, start = c(qr0 = 2, creep = 0.1))
  # --> Both QR0 and creep have significant influence here!
  
  # plot obs vs pred
  qr0 <- 1.272175
  creep <- 0.013153
  plaice$pred_f <- c((1 + creep * (plaice$year - min(plaice$year[!is.na(plaice$f_scaled)])) ) * plaice$f_scaled * qr0 /  (1 + (qr0 - 1) * 
                                                                                                                            plaice$ssb / plaice$ssb[plaice$year == 1991]))
  plot(plaice$pred_f ~ plaice$mean.f,
       main = paste0('R² = ', round(digits = 3, cor.test(plaice$pred_f, plaice$mean.f, method = 'pearson')$estimate ^2))  )
  abline(a = 0, b = 1, col = 'red')

  