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

here::i_am("code/2_add_tags.R")

###########################################################################
## Read in interim data ###################################################
###########################################################################

## read in line item data
line_items <- read_excel(here("data", "interim", "Interim Line Items.xlsx"))

## read in categories
line_item_categories <- read_excel(here("data", "interim", "Line Item Categories.xlsx"))

###########################################################################
## Merge datasets #########################################################
###########################################################################

line_items_complete <- line_items %>%
  left_join(line_item_categories[,c(1, 7)], join_by(line_item_id)) %>%
  mutate(primary_category = case_when(
    primary_subcategory %in% c("Salary support and/or stipends, including overhead",
                               "Consultant fees",
                               "Per diems and travel expenses") ~ "Workforce",
    primary_subcategory %in% c("Laboratories and laboratory equipment",
                               "Healthcare facilities",
                               "Office or other facility space",
                               "Warehouses or storage facilities",
                               "Environmental monitoring equipment") ~ "Physical infrastructure",
    primary_subcategory %in% c("Data analysis and analytics infrastructure",
                               "Computing resources",
                               "Miscellaneous digital infrastructure") ~ "Digital infrastructure",
    primary_subcategory %in% c("Transportation and transport fees, including cold chain",
                               "Water resources and WASH infrastructure") ~ "Civil infrastructure",
    primary_subcategory %in% c("Media Operational Costs",
                               "Media Subscriptions") ~ "Media",
    primary_subcategory %in% c("Medical Countermeasures",
                               "Basic supplies for outbreak investigation and response",
                               "Personal protective equipment and basic supplies for infection prevention and control (IPC)") ~ 
                                "Medical countermeasures and supplies for healthcare delivery",
    TRUE ~ "Other"))

###########################################################################
## Export final clean dataset #############################################
###########################################################################

# export complete data
write_xlsx(line_items_complete,
           path = here("data", "clean", "Costed NAPHS data.xlsx"),
           col_names = TRUE, format_headers = FALSE)

