###########################################################################
## Load required libraries ################################################
###########################################################################

## if you don't already have these installed, you'll need to install them
## using the command install.libraries()

library(readxl)  ## to read in Excel files
library(writexl) ## to export Excel files
library(dplyr)   ## for data manipulation
library(here)    ## for file path management

###########################################################################
## Specify filepath info ##################################################
###########################################################################

here::i_am("code/1_data_cleaning.R")

############################################################################
## Read in raw data ########################################################
############################################################################

## read in raw line-item data
line_items_raw <- read_excel(here("data", "raw", "raw line item data.xlsx"),
                             col_types = "guess")

## read in raw summary data
summary_costs_raw <- read_excel(here("data", "raw", "raw summary cost data.xlsx"),
                                col_types = "guess")

###########################################################################
## Read in other data sources #############################################
###########################################################################

## read in core capacity mapping data
core_capacity_mapping <- read.table(here("data", "raw", "core_capacity_mapping.txt"),
                                    sep = "\t", header = TRUE)

## read in currency conversion data
currency_conversions <- read.table(here("data", "raw", "exchange_rates.txt"),
                                   sep = "\t", header = TRUE)

###########################################################################
## Clean written descriptions (minimally) #################################
###########################################################################

## replace line breaks with spaces in select text fields
line_items_raw$requirement <- gsub("\n", " ", line_items_raw$requirement_original)
line_items_raw$activity <- gsub("\n", " ", line_items_raw$activity_original)

## remove any commas from cost field
## suppress warnings about NAs, those are expected
line_items_raw$cost_original_numeric <- suppressWarnings(as.numeric(gsub(",", "", line_items_raw$cost_original)))
summary_costs_raw$cost_original_numeric <- suppressWarnings(as.numeric(gsub(",", "", summary_costs_raw$cost_original)))

###########################################################################
## Merge datasets to include common core capacity mapping #################
## and to adjust currencies into common unit (USD 2024) ###################
###########################################################################

line_items_clean <- line_items_raw |>
  left_join(core_capacity_mapping, by = join_by(capacity_original == core_capacity_original)) |>
  left_join(currency_conversions, by = join_by(currency_original == currency_original)) |>
  mutate(cost_usd2024 = cost_original_numeric*currency_multiplier,
         capacity = coalesce(core_capacity, capacity_original))

summary_costs_clean <- summary_costs_raw |>
  left_join(core_capacity_mapping, by = join_by(cost_observation_details == core_capacity_original)) |>
  left_join(currency_conversions, by = join_by(currency_original == currency_original)) |>
  mutate(cost_usd2024 = cost_original_numeric*currency_multiplier,
         capacity = coalesce(core_capacity, cost_observation_details))

###########################################################################
## Export clean dataset ###################################################
###########################################################################

# export complete data
write_xlsx(line_items_clean[, c("line_item_id", "country", "pillar", "core_capacity",
                                "requirement", "activity", "year_numeric", "year_calendar",
                                "cost_original", "currency_original", "cost_usd2024", 
                                "primary_category", "primary_subcategory")],
           path = here("data", "clean", "line item data.xlsx"),
           col_names = TRUE, format_headers = FALSE)
