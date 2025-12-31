###########################################################################
## Load required libraries ################################################
###########################################################################

## if you don't already have these installed, you'll need to install them
## using the command install.libraries()

library(readxl) ## to read in Excel files
library(dplyr) ## for data manipulation
library(here) ## for file path management

## these packages are needed for the exploratory analysis code below
library(ggplot2) ## for making figures
library(treemapify) ## for treemaps
library(stringr) ## for string wrapping
library(viridis) ## for colors

###########################################################################
## Specify filepath info ##################################################
###########################################################################

here::i_am("code/analysis.R")

####################################################################################################
## Read in clean data ##############################################################################
####################################################################################################

## read in cleaned data
line_items <- read_excel(here("data", "Line item data.xlsx"),
                         col_types = "guess")

####################################################################################################
## Manage data types within R ######################################################################
####################################################################################################

## manage numeric variables
line_items$cost_usd2024 <- as.numeric(line_items$cost_usd2024)
line_items$cost_original_numeric <-  as.numeric(line_items$cost_original)
