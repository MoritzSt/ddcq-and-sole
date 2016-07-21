# A script to compare the spatial distribution of fishing effort from STECF data
# between NLD and BEL beam trawlers.

# working_directory <- 'D:\\workfolder\\TI-2016\\ddcq\\ddcq-and-sole-master\\ddcq-and-sole-master'  # at priv PC
 working_directory <- 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole'  # at TI PC
# working_directory <- 'Z:\\02-TI\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole'  # at ZMT PC

 library(sNoSeR)
 library(plyr)
 library(dplyr)
 
 
# get effort
 # created as csv in file 'P:\\Offlineordner\\Promotion III -- Technological Creep\\10--Sole\\ddcq and sole\\input\\create-stecf-effort-csv.R'
 effort <- get_stecf_landings_per_rectangle(file = paste(working_directory, 'input', 'STECF 2014', 'Effort_by_rectangle.csv', sep = '\\'),
                                            nose_only = TRUE, deep = F, fdf = F, format_long = T)
 # It was 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\input data\\STECF 2014\\Effort_by_rectangle.csv', nose_only = TRUE, deep = F, fdf = F, format_long = T)
 effort <- rename(.data = effort, effort = value)
 effort$year <- as.integer(as.character(effort$year))
 # calculate effort per metier and year per rec
 by_metier <- group_by(.data = effort, country, reg_gear_cod, vessel_length, year, rectangle)
 by_metier <- summarise(.data = by_metier,
                        count = n(),
                        metier_effort = sum(effort, na.rm = TRUE))  # na.rm = T, as we consider NA in STECF effort data to translate to 'no effort in this rectangle'.
 
 