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

  # calculate FPUE, i.e. q
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
  rm(sole_number_at_age, sole_weight_at_age)
  sole_b_at_age <- mutate(.data = sole_b_at_age, b_at_age = weight_at_age * number_at_age)
  # plot
  qplot(data = sole_b_at_age, y = b_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  qplot(data = sole_b_at_age, y = weight_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  qplot(data = sole_b_at_age, y = number_at_age, x = age) + geom_point() + facet_wrap(~year, scales = 'free_y')
  # --> Numbers at age are very variable. Is that real? [!!!]
  
  
  # add biomass info to FPUE data
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

  
  
  
# (1.6) SSB Stats: FPUE ~ biomass + year ----------------------------------
  
  dat <- merge(sole_total, long_effort, all.x = F, all.y = F)
  dat <- select(.data = dat, year, ssb, mean.f, effort_rel_2003)
  dat$fpue <- dat$mean.f / dat$effort_rel_2003
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
  

  # Test family
  model.gaussian <- gam(data = dat, formula = fpue ~ s(ssb, k = -1, fx = F) + year,
               family = gaussian(link = 'log') )
  
  model.gamma <- gam(data = dat, formula = fpue ~ s(ssb, k = -1, fx = F) + year,
                        family = Gamma(link = 'log') )
  
  model.gamma <- gam(data = dat, formula = fpue ~ s(ssb, k = -1, fx = F) + s(year),
                     family = Gamma(link = 'log') )
  
  # --> > family = Gamma  # is better than gaussian because Resids. vs.
  # linear pred. has u-shaped curve.
  # --> Both have bias in Resids. vs. linear pred. towards positive values,
  #     not equal on both sides of y = 0.
  # --> R2 adjusted, however, is higher for gaussian.
  
  
  
  
  # test adding error term in mixed model (gamm):
  # including an autoregressive term to account for temporal autocorrelation
  
  # without correction for autocorrelation and testing ssb effect ONLY:
  model00 <- gamm(data = dat, formula = fpue ~ s(ssb),
                       family = Gamma(link = 'log'))
  
  # testing both time and ssb WITHOUT autocorrelation control:
    model0 <- gamm(data = dat, formula = fpue ~ s(ssb) + year,
                      family = Gamma(link = 'log'))
  gam.check(model0$gam)
  summary(model0$gam)
  plot(model0$gam)

  # including an autoregressive term
  model1 <- gamm(data = dat, formula = fpue ~ s(ssb) + year,
                         family = Gamma(link = 'log'),
                         correlation = corARMA(form = ~ year, p = 1))  # p = 0   doesn't work.
  
   # does that improve the model?
  AIC(model0$lme, model1$lme)
   # --> Yes, AIC much better.
   # But are the residuals ok?
  gam.check(model1$gam)
   # --> Nope, the residuals are u-shaped :-/ .
  
   # That might be because the gam.check resids vs lin pred plot shows raw residuals
   # taking into account the fixed effects terms only.
   # To get residuals that include the autoregressive term, we need normalized residuals:
   # https://stats.stackexchange.com/questions/80823/do-autocorrelated-residual-patterns-remain-even-in-models-with-appropriate-corre/80825#80825
  x11()
  layout(matrix(1:2))
  acf(resid(model1$lme))
  acf(resid(model1$lme, type = "normalized"))
   # --> No more autocorrelation in normalized residuals.
  layout(1)
  dev.off()
   # Plot gam.check with normalized residuals
    gam.check_normalized <- function(model) {  # A function to draw gam.check with norm. res.
    x11()
    par(mfrow = c(2,2))
     # QQ Plot
    qq.gam(model$gam, rep = 0, level = 0.9, type = 'deviance', rl.col = 2, 
           rep.col = "gray80", cex = 3)
    # NOrmalized residuals vs linear predictor
    model_gam_part <- model$gam
    plot(napredict(model_gam_part$na.action, model_gam_part$linear.predictors),
         resid(model$lme, type = 'normalized'),
         main = "Normalized resids vs. linear pred.", 
         xlab = "linear predictor", ylab = "residuals")
    # histogram of normalized residuals
    hist(resid(model$lme, type = 'normalized'), xlab = "Normalized residuals",
         main = "Histogram of normalized residuals")
    # Observed vs fitted values
    plot(fitted(model_gam_part), napredict(model_gam_part$na.action, model_gam_part$y), xlab = "Fitted Values", 
         ylab = "Response", main = "Response vs. Fitted Values")
    }  # End of gam.check_normalized function.
    
    
# (1.6.2) Optimize GAM with autoregressive term ---------------------------
    model2 <- gamm(data = dat, formula = fpue ~ s(ssb) + year,
                   family = gaussian(link = 'log'),
                   correlation = corARMA(form = ~ year, p = 1))  
    # Check normalized model residuals
    gam.check_normalized(model = model2)
    # --> looks sort of better than family = Gamma, as response vs fitted values
    #     starts closer to origin.
    
    
  
# (1.7) If u-shaped resid pattern has another meaning that fixed m --------
  
   #      If u-shaped pattern in residuals is NOT because of the gam.check
   #      plot showing 
   #      there is something else happening, and I suspect that's for the
   #      relationship between effort and FPUE. FPUE decreases with effort,
   #      which I assume is the case because a double effort would not lead
   #      to a doubled F.
  plot(dat$fpue ~ dat$effort_rel_2003)
  lm(dat$mean.f ~ dat$effort_rel_2003)  # 18% rise in F per unit effort.
  
   # including a smoother on 'year' would fix the problem...
  model1x <- gamm(data = dat, formula = fpue ~ s(ssb) + s(year, k = 5, fx = T),
                       family = Gamma(link = 'log'),
                       correlation = corARMA(form = ~ year, p = 1))
  gam.check(model1x$gam)
   # ... but is explanatory ±nonsense.
  
  
# (2) What if I modelled F ~ f + ssb + year? ------------------------------
  
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