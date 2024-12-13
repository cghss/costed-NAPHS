###########################################################################
## Load required libraries ################################################
###########################################################################

library(readxl) ## to read in Excel files
library(dplyr) ## for data manipulation

###########################################################################
## Read in raw data #######################################################
###########################################################################

## TODO: check start year of Uganda NAPHS, Q/A raw data again
## TODO: update data entry guidelines with new year coding rules
## TODO: check currency for Nigeria

## get a list of all the countries in the sheet (one per tab)
all_countries <- excel_sheets("data/raw/Costed NAPHS data.xlsx")

## read in data for all countries and append into a data frame
## note, by design last tab is excluded as it's the data dictionary
for(i in 1:length(all_countries)-1){
  if (i == 1){ raw_data <- read_excel("data/raw/Costed NAPHS data.xlsx", sheet = all_countries[i]) }
  if (i >  1){ raw_data <- rbind.data.frame(
                              raw_data,
                              read_excel("data/raw/Costed NAPHS data.xlsx", sheet = all_countries[i]))
                              }
}

###########################################################################
## Restructure and rename variables #######################################
###########################################################################

## rename variables
names(raw_data)[which(names(raw_data) == "Country")] <- "country"
names(raw_data)[which(names(raw_data) == "Capacity")] <- "capacity"
names(raw_data)[which(names(raw_data) == "Requirement")] <- "requirement"
names(raw_data)[which(names(raw_data) == "Action")] <- "action"
names(raw_data)[which(names(raw_data) == "Year (1-5)")] <- "year_numeric"
names(raw_data)[which(names(raw_data) == "Year (calendar)")] <- "year_calendar"
names(raw_data)[which(names(raw_data) == "Cost")] <- "cost"
names(raw_data)[which(names(raw_data) == "Currency (year)")] <- "currency"
names(raw_data)[which(names(raw_data) == "Flag")] <- "flag"

###########################################################################
## Export clean dataset ###################################################
###########################################################################