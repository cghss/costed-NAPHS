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
## Clean written descriptions and text fields (minimally) #################
###########################################################################

## replace line breaks with spaces in select text fields
line_items_raw$requirement <- gsub("\n", " ", line_items_raw$requirement_original)
line_items_raw$activity <- gsub("\n", " ", line_items_raw$activity_original)

## remove any commas from cost field
## suppress warnings about NAs, those are expected for null costs
line_items_raw$cost_original_numeric <- suppressWarnings(as.numeric(gsub(",", "", line_items_raw$cost_original)))
summary_costs_raw$cost_original_numeric <- suppressWarnings(as.numeric(gsub(",", "", summary_costs_raw$cost_original)))

###########################################################################
## Merge datasets to include common core capacity mapping #################
## and to adjust currencies into common unit (USD 2024) ###################
###########################################################################

## normalize fields to join in: lowercase the text and strip commas so capitalization or
## comma differences don't break the core capacity join
normalize_key <- function(x) {
  tolower(trimws(gsub(",", "", x)))
}

## create join key in each table (could do this in the pipe, but easier to read this way)
core_capacity_mapping$join_key <- normalize_key(core_capacity_mapping$core_capacity_original)
line_items_raw$join_key <- normalize_key(line_items_raw$capacity_original)
  
line_items_clean <- line_items_raw |>
  left_join(core_capacity_mapping |> select(-core_capacity_original), by = "join_key") |>
  left_join(currency_conversions, by = join_by(currency_original == currency_original)) |>
  mutate(cost_usd2024 = cost_original_numeric*currency_multiplier,
         capacity = coalesce(core_capacity, capacity_original)) |>
  select(-join_key)

summary_costs_clean <- summary_costs_raw |>
  mutate(join_key = normalize_key(cost_observation_details)) |>
  left_join(core_capacity_mapping |> select(-core_capacity_original), by = "join_key") |>
  left_join(currency_conversions, by = join_by(currency_original == currency_original)) |>
  mutate(cost_usd2024 = cost_original_numeric*currency_multiplier,
         core_capacity = coalesce(core_capacity, cost_observation_details)) |>
  select(-join_key)

###########################################################################
## Data QA checks #########################################################
###########################################################################

## check for missing core capacity mappings (line-items)
line_items_clean |>
  filter(is.na(core_capacity)) |>
  distinct(country, capacity_original) |>
  {\(x) if (nrow(x) > 0) { message("WARNING: ", nrow(x), " unmapped core capacities in line items. Please check mappings against data/raw/core_capacity_mapping."); print(x) } else { message("Test passed: All line-item core capacities mapped successfully.") }}()

## check for missing core capacity mappings (summary costs)
summary_costs_clean |>
  filter(is.na(core_capacity)) |>
  distinct(country, cost_observation_details) |>
  {\(x) if (nrow(x) > 0) { message("WARNING: ", nrow(x), " unmapped core capacities in summary costs. Please check mappings against data/raw/core_capacity_mapping."); print(x) } else { message("Test passed: All summary cost core capacities mapped successfully.") }}()

## check for missing currency conversions  (line items)
line_items_clean |>
  filter(is.na(currency_multiplier)) |>
  distinct(country, currency_original) |>
  {\(x) if (nrow(x) > 0) { message("WARNING: ", nrow(x), " unmapped currencies in line items. Please check mappings against data/raw/exchange_rates."); print(x) } else { message("Test passed: All line-item currencies mapped successfully.") }}()

## check for missing currency conversions (summary costs)
summary_costs_clean |>
  filter(is.na(currency_multiplier)) |>
  distinct(country, currency_original) |>
  {\(x) if (nrow(x) > 0) { message("WARNING: ", nrow(x), " unmapped currencies in summary costs. Please check mappings against data/raw/exchange_rates."); print(x) } else { message("Test passed: All summary cost currencies mapped successfully.") }}()

## check for unexpected NAs in cost conversions where original cost exists (line items)
line_items_clean |>
  filter(!is.na(cost_original_numeric) & is.na(cost_usd2024)) |>
  {\(x) if (nrow(x) > 0) { message("WARNING: ", nrow(x), " line items have original costs but no USD conversion.") } else { message("Test passed: All non-missing line-item costs converted to USD.") }}()

## check for unexpected NAs in cost conversions where original cost exists (summary costs)
summary_costs_clean |>
  filter(!is.na(cost_original_numeric) & is.na(cost_usd2024)) |>
  {\(x) if (nrow(x) > 0) { message("WARNING: ", nrow(x), " summary costs have original costs but no USD conversion.") } else { message("Test passed: All non-missing summary costs converted to USD.") }}()

## check for cost values that failed to parse as numeric
## (non-empty raw text that isn't the expected "N/A" placeholder -- catches
##  unexpected values like "TBD" or stray characters)
cost_parse_mismatches_line_items <- line_items_raw |>
  filter(!is.na(cost_original) & trimws(cost_original) != "" &
         trimws(cost_original) != "N/A" & is.na(cost_original_numeric)) |>
  distinct(cost_original)

cost_parse_mismatches_summary <- summary_costs_raw |>
  filter(!is.na(cost_original) & trimws(cost_original) != "" &
         trimws(cost_original) != "N/A" & is.na(cost_original_numeric)) |>
  distinct(cost_original)

if (nrow(cost_parse_mismatches_line_items) > 0) {
  message("WARNING: ", nrow(cost_parse_mismatches_line_items),
          " distinct raw line-item cost values did not parse as numeric. Review with View(cost_parse_mismatches_line_items).")
  } else {
  message("Test passed: All non-empty line-item cost values parsed as numeric.")}

if (nrow(cost_parse_mismatches_summary) > 0) {
  message("WARNING: ", nrow(cost_parse_mismatches_summary),
          " distinct raw summary cost values did not parse as numeric. Review with View(cost_parse_mismatches_summary).")
  } else {
  message("Test passed: All non-empty summary cost values parsed as numeric.")}

###########################################################################
## Export clean dataset ###################################################
###########################################################################

# export complete data
## select() renames cost_original_numeric -> cost_original on the way out, so
## the exported file stores cost as a true numeric column (not text with "N/A")
write_xlsx(line_items_clean |>
             select(line_item_id, country, pillar, core_capacity,
                    requirement, activity, year_numeric, year_calendar,
                    cost_original = cost_original_numeric, currency_original,
                    cost_usd2024, primary_category, primary_subcategory),
           path = here("data", "clean", "line item data.xlsx"),
           col_names = TRUE, format_headers = FALSE)

write_xlsx(summary_costs_clean |>
             select(summary_cost_id, country, pillar, core_capacity,
                    cost_original = cost_original_numeric, currency_original,
                    cost_usd2024),
           path = here("data", "clean", "summary cost data.xlsx"),
           col_names = TRUE, format_headers = FALSE)

## remove everything from workspace
rm(list = ls())
