###########################################################################
## Load required libraries ################################################
###########################################################################

## if you don't already have these installed, you'll need to install them
## using the command install.packages()

library(readxl) ## to read in Excel files
library(writexl) ## to save Excel files
library(dplyr) ## for data manipulation
library(here) ## for file path management

###########################################################################
## Specify filepath info ##################################################
###########################################################################

here::i_am("code/1_data_cleaning.R")

###########################################################################
## Read in raw line-item cost data ########################################
###########################################################################

## get a list of all the countries in the sheet (one per tab)
all_countries <- excel_sheets(here("data", "raw", "Raw Costed NAPHS data.xlsx"))
  
## read in data for all countries and append into a data frame
## note, by design last tab is excluded as it's the data dictionary
for(i in 1:length(all_countries)-1){
  if (i == 1){ raw_data <- read_excel(here("data", "raw", "Raw Costed NAPHS data.xlsx"), sheet = all_countries[i]) }
  if (i >  1){ raw_data <- rbind.data.frame(
                              raw_data,
                              read_excel(here("data", "raw", "Raw Costed NAPHS data.xlsx"), sheet = all_countries[i]))
                              }
}

###########################################################################
## Read in other data sources #############################################
###########################################################################

## read in core capacity mapping data
core_capacities <- read.table(here("data", "raw", "core_capacity_mapping.txt"),
                              sep = "\t", header = TRUE)

currency_conversions <- read.table(here("data", "raw", "exchange_rates.txt"),
                                   sep = "\t", header = TRUE)

###########################################################################
## Restructure and rename variables #######################################
###########################################################################

## rename variables
names(raw_data)[which(names(raw_data) == "Country")] <- "country"
names(raw_data)[which(names(raw_data) == "Capacity")] <- "capacity_original"
names(raw_data)[which(names(raw_data) == "Requirement")] <- "requirement_original"
names(raw_data)[which(names(raw_data) == "Activity")] <- "activity_original"
names(raw_data)[which(names(raw_data) == "Year (1-5)")] <- "year_numeric"
names(raw_data)[which(names(raw_data) == "Year (calendar)")] <- "year_calendar"
names(raw_data)[which(names(raw_data) == "Cost")] <- "cost_original"
names(raw_data)[which(names(raw_data) == "Currency (year)")] <- "currency_original"
names(raw_data)[which(names(raw_data) == "Flag")] <- "flag"

###########################################################################
## Clean written descriptions (minimally) #################################
###########################################################################

## replace line breaks with spaces in select text fields
raw_data$requirement <- gsub("\n", " ", raw_data$requirement_original)
raw_data$activity <- gsub("\n", " ", raw_data$activity_original)

## remove any commas from cost field
## supress warnings about NAs, those are expected
raw_data$cost_original_numeric <- suppressWarnings(as.numeric(gsub(",", "", raw_data$cost_original)))

###########################################################################
## Merge datasets to include common core capacity mapping #################
## and to adjust currencies into common unit (USD 2024) ###################
###########################################################################

updated_raw_data <- raw_data |>
  left_join(core_capacities, by = join_by(capacity_original == core_capacity_original)) |>
  left_join(currency_conversions, by = join_by(currency_original == currency_original)) |>
  mutate(cost_usd2024 = cost_original_numeric*currency_multiplier)

###########################################################################
## Create unique ID for each line #########################################
###########################################################################

## create unique ID for each row
updated_raw_data$line_item_id <- 1:nrow(updated_raw_data)

###########################################################################
## Export clean dataset ###################################################
###########################################################################

## I would prefer this to be open-source, though I suspect
## Excel may be easier for some users. Also save duplicate pipe-delimited file.

## create dataset used to manually review for tags
export_no_tags <- updated_raw_data[, c(20, 1, 2, 14, 10, 11)]

# export data for tagging (one line item can have many tags)
## commented out and changed file name, don't run (contains interim tagging work)
## THIS FILE CONTAINS MANUAL WORK/TAGGING STEP
# write_xlsx(export_no_tags,
#            path = here("data", "interim", "XXXXXLine Item TagsXXXXX.xlsx"),
#            col_names = TRUE, format_headers = FALSE)

# export data for primary categorization (one line item belongs to one single category)
## commented out and changed file name, don't run (contains interim tagging work)
## THIS FILE CONTAINS MANUAL WORK/TAGGING STEP
# write_xlsx(export_no_tags,
#            path = here("data", "interim", "XXXXXLine Item CategoriesXXXXX.xlsx"),
#            col_names = TRUE, format_headers = FALSE)

# export complete data
write_xlsx(updated_raw_data[, c(20, 1, 13, 14, 10, 11, 6, 5, 19, 2, 3, 4, 7, 8)],
           path = here("data", "interim", "Interim Line Items.xlsx"),
           col_names = TRUE, format_headers = FALSE)

###########################################################################
## Remove data from working environment ###################################
###########################################################################

## remove all items in the working environment
rm(list = ls())

