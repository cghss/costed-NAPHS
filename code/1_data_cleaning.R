###########################################################################
## Load required libraries ################################################
###########################################################################

library(readxl) ## to read in Excel files

###########################################################################
## Read in raw data #######################################################
###########################################################################

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

###########################################################################
## Export clean dataset ###################################################
###########################################################################