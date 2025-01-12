###########################################################################
## Load required libraries ################################################
###########################################################################

library(readxl) ## to read in Excel files
library(writexl) ## to save Excel files
library(dplyr) ## for data manipulation

###########################################################################
## Read in raw line-item cost data ########################################
###########################################################################

## TODO: set up here()
## TODO: check citations, update, and document core capacity mapping

## get a list of all the countries in the sheet (one per tab)
all_countries <- excel_sheets("data/raw/Raw Costed NAPHS data.xlsx")

## read in data for all countries and append into a data frame
## note, by design last tab is excluded as it's the data dictionary
for(i in 1:length(all_countries)-1){
  if (i == 1){ raw_data <- read_excel("data/raw/Raw Costed NAPHS data.xlsx", sheet = all_countries[i]) }
  if (i >  1){ raw_data <- rbind.data.frame(
                              raw_data,
                              read_excel("data/raw/Raw Costed NAPHS data.xlsx", sheet = all_countries[i]))
                              }
}

###########################################################################
## Read in other data sources #############################################
###########################################################################

## read in core capacity mapping
core_capacities <- read.csv("data/raw/core_capacity_mapping.csv")

###########################################################################
## Restructure and rename variables #######################################
###########################################################################

## rename variables
names(raw_data)[which(names(raw_data) == "Country")] <- "country"
names(raw_data)[which(names(raw_data) == "Capacity")] <- "capacity_original"
names(raw_data)[which(names(raw_data) == "Requirement")] <- "requirement_raw"
names(raw_data)[which(names(raw_data) == "Action")] <- "action_raw"
names(raw_data)[which(names(raw_data) == "Year (1-5)")] <- "year_numeric"
names(raw_data)[which(names(raw_data) == "Year (calendar)")] <- "year_calendar"
names(raw_data)[which(names(raw_data) == "Cost")] <- "cost_raw"
names(raw_data)[which(names(raw_data) == "Currency (year)")] <- "currency"
names(raw_data)[which(names(raw_data) == "Flag")] <- "flag"

###########################################################################
## Clean written descriptions (minimally) #################################
###########################################################################

## replace line breaks with spaces in select text fields
raw_data$requirement <- gsub("\n", " ", raw_data$requirement_raw)
raw_data$action <- gsub("\n", " ", raw_data$action_raw)

## remove any commas from cost field
raw_data$cost <- gsub(",", "", raw_data$cost_raw)

###########################################################################
## Merge datasets to include common core capacity mapping #################
###########################################################################

updated_raw_data <- raw_data |>
  left_join(core_capacities,
            by = join_by(capacity_original == core_capacity_original))

###########################################################################
## Convert to common currency #############################################
###########################################################################

## TODO: figure out currency conversion

###########################################################################
## Export clean dataset ###################################################
###########################################################################

## TODO: I would prefer this to be open-source, though I suspect
## Excel may be easier for some users. Also save duplicate pipe-delimited file?

write_xlsx(updated_raw_data[, c(1,13,14,2,10,11,5,6,12,8,9)], 
           path = "data/clean/Clean Costed NAPHS data.xlsx", 
           col_names = TRUE, format_headers = FALSE)

###########################################################################
## Remove data from working environment ###################################
###########################################################################

## remove all items in the working environment
rm(list = ls())

