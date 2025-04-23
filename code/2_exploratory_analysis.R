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

###########################################################################
## Specify filepath info ##################################################
###########################################################################

here::i_am("code/2_exploratory_analysis.R")

####################################################################################################
## Run code in other file to clean data ############################################################
## NOTE: this will remove any other files you have stored in your filepath when it runs ############
####################################################################################################

source(here("code", "1_data_cleaning.R"))

####################################################################################################
## Read in clean data ##############################################################################
####################################################################################################

## read in cleaned data
line_items <- read_excel("data/clean/Clean Costed NAPHS data.xlsx")

####################################################################################################
## Manage data types within R ######################################################################
####################################################################################################

## mange numeric variables
line_items$cost_original <- as.numeric(line_items$cost_original)
line_items$cost_usd2024 <- as.numeric(line_items$cost_usd2024)

line_items$core_capacity_mapped <- factor(line_items$core_capacity_mapped,
                                          levels = c("National Legislation, Policy and Financing",
                                                     "IHR Coordination",
                                                     "Antimicrobial Resistance",
                                                     "Zoonotic Disease",
                                                     "Food Safety",
                                                     "Biosafety and Biosecurity",
                                                     "Immunization",
                                                     "National Laboratory System",
                                                     "Surveillance",
                                                     "Reporting",
                                                     "Workforce",
                                                     "Preparedness",
                                                     "Emergency Response Operations",
                                                     "Linking Public Health and Security Authorities",
                                                     "Medical Countermeasures and Personnel Deployment",
                                                     "Infection Prevention and Control",
                                                     "Risk Communication",
                                                     "Points of Entry",
                                                     "Chemical Events",
                                                     "Radiation Emergencies"))

## TODO: pillars as factors

####################################################################################################
## Summarize data by capacity by country ###########################################################
####################################################################################################

## TODO: refine figure, it needs help

line_items |>
  group_by(country, core_capacity_mapped) |>
  summarize(
    total_line_items = n(),
    costed_line_items = sum(complete.cases(cost_usd2024)),
    total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  ggplot(aes(y = core_capacity_mapped, x = total_capacity_cost)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country)

####################################################################################################
## For each country, what % of funding requirements come from each core capacity? ##################
####################################################################################################

## TODO: refine figure, it needs help

line_items |>
  group_by(country, core_capacity_mapped) |>
  summarize(total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  group_by(country) |>
  mutate(total_cost = sum(total_capacity_cost, na.rm = TRUE),
         percent_capacity_cost = total_capacity_cost/total_cost) |> 
  ggplot(aes(y = core_capacity_mapped, x = percent_capacity_cost)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country)


