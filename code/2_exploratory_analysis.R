###########################################################################
## Load required libraries ################################################
###########################################################################

## these packages are needed needed to run the script 1_data_cleaning.R

## if you don't already have these installed, you'll need to install them
## using the command install.libraries()

library(readxl) ## to read in Excel files
library(writexl) ## to save Excel files
library(dplyr) ## for data manipulation
library(here) ## for file path management


## these packages are needed for the exploratory analysis code below
library(ggplot2) ## for making figures

####################################################################################################
## Run code in other file to clean data ############################################################
####################################################################################################

source("code/1_data_cleaning.R")

####################################################################################################
## Read in clean data ##############################################################################
####################################################################################################

## read in cleaned data
line_items <- read_excel("data/clean/Clean Costed NAPHS data.xlsx")

####################################################################################################
## Manage data types within R ######################################################################
####################################################################################################

## TODO: also do this switch with common currency
line_items$cost_numeric <- as.numeric(line_items$cost)

## TODO: core capacity becomes an ordered factor

####################################################################################################
## Summarize data by capacity by country ###########################################################
####################################################################################################

## TODO: switch this to common currency
## TODO: switch this to common core capacity
## TODO: refine figure

line_items |>
  group_by(country, capacity) |>
  summarize(
    total_line_items = n(),
    costed_line_items = sum(complete.cases(cost_numeric)),
    total_capacity_cost = sum(cost_numeric, na.rm = TRUE)) |>
  ggplot(aes(y = capacity, x = total_capacity_cost)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country)

####################################################################################################
## For each country, what % of funding requirements come from each core capacity? ##################
####################################################################################################

## TODO: switch this to common currency
## TODO: switch this to common core capacity

line_items |>
  group_by(country, capacity) |>
  summarize(total_capacity_cost = sum(cost_numeric, na.rm = TRUE)) |>
  group_by(country) |>
  mutate(total_cost = sum(total_capacity_cost, na.rm = TRUE),
         percent_capacity_cost = total_capacity_cost/total_cost) |> 
  ggplot(aes(y = capacity, x = percent_capacity_cost)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country)


