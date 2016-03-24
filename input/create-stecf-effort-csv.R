# Get STECF effort data
# https://stecf.jrc.ec.europa.eu/c/document_library/get_file?uuid=ca439c56-0d27-4b04-8402-de3e3a77bbb8&groupId=43805
# Accessed on 2016-24-03
# Use Effort_by_rectangle.xlsx

library(gdata)
perl <- 'D:\\NochNeuereProgramme\\StraberryPerl\\perl\\bin\\perl.exe'
xls_path <- 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\input data\\STECF 2014\\Effort_by_rectangle.xlsx'
dat <- read.xls(xls = xls_path, perl = 'perl')
dat$annex <- as.character(dat$annex)
dat$reg_area_cod <- as.character(dat$reg_area_cod)
dat$reg_gear_cod <- as.character(dat$reg_gear_cod)
dat$specon <- as.character(dat$specon)
dat$vessel_length <- as.character(dat$vessel_length)
dat$rectangle <- as.character(dat$rectangle)
dat$country <- as.character(dat$country)
effort <- dat
rm(dat, perl, xls_path)
write.csv(effort, file = 'D:\\OfflineOrdner\\Promotion III -- Technological Creep\\input data\\STECF 2014\\Effort_by_rectangle.csv')
