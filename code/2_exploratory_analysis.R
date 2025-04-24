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
library(viridis) # For a colorblind-friendly palette

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
    costed_line_items = sum(complete.cases(cost_usd2024/1000)),
    total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  ggplot(aes(y = core_capacity_mapped, x = total_capacity_cost)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country) +
  labs(y = "", x = "Cost (2024 USD)") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", suffix = "K"))


line_items |>
  group_by(country, core_capacity_mapped) |>
  summarize(total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = country, y = core_capacity_mapped, fill = total_capacity_cost)) +
  geom_tile(color = "white", linewidth = 0.1) +
  # Color palette - using viridis for colorblind-friendly approach
  scale_fill_viridis(
    name = "Cost (2024 USD)",
    option = "viridis", 
    trans = "log10", # Log scale helps with data that varies widely
    labels = scales::dollar_format(scale = 1e-3, suffix = "K"),
    guide = guide_colorbar(title.position = "top", barwidth = 10)
  ) +
  # Improve text readability
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12)
  ) +
  labs(
    x = "",
    y = "",
    title = "Health Security Capacity Costs by Country",
    subtitle = "Color intensity represents total cost in 2024 USD"
  ) 

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


