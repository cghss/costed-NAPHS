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
library(treemapify) ## for treemaps

## these packages are needed for the exploratory analysis code below
library(ggplot2) ## for making figures
library(treemapify) ## for treemaps
library(stringr) ## for string wrapping

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
line_items <- read_excel(here("data", "clean", "Clean Costed NAPHS data.xlsx"))
specified_labels <- read.table(here("data", "raw", "requirement_highlights.txt"),
                               sep = "\t", header = TRUE)

####################################################################################################
## Manage data types within R ######################################################################
####################################################################################################

## mange numeric variables
line_items$cost_original <- as.numeric(line_items$cost_original)
line_items$cost_usd2024 <- as.numeric(line_items$cost_usd2024)

## country becomes a factor sorted by higheset to lowest total costs, at least to start
line_items$country_factor <- factor(line_items$country,
                                    levels = c("Sierra Leone",
                                               "Myanmar",
                                               "Liberia",
                                               "Nigeria",
                                               "Eritrea",
                                               "Uganda",
                                               "Benin",
                                               "Afghanistan",
                                               "Timor-Leste"))
## core capacity becomes a factor for plotting
line_items$core_capacity_mapped <- factor(line_items$core_capacity_mapped,
                                          levels = rev(c("National Legislation, Policy and Financing",
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
                                                     "Radiation Emergencies")))

## pillar is also a factor
line_items$pillar <- factor(line_items$pillar, levels = c("Prevent", "Detect", "Respond", "Other"))

####################################################################################################
## Manage colors consistently for core capacities ##################################################
####################################################################################################

prevent_colors <- c(
  "National Legislation, Policy and Financing" = "#158A4A", 
  "IHR Coordination" = "#25AB65",
  "Antimicrobial Resistance" = "#4BC17F", 
  "Zoonotic Disease" = "#6DD398",
  "Food Safety" = "#8CE0AE",
  "Biosafety and Biosecurity" = "#A8EDC3",
  "Immunization" = "#C7F5D8"
)

detect_colors <- c(
  "National Laboratory System" = "#FFD12E", 
  "Surveillance" = "#FFDA4F",
  "Reporting" = "#FFE270", 
  "Workforce" = "#FFEBA0"
)

respond_colors <- c(
  "Preparedness" = "#DB7735", # Less intense darkest orange
  "Emergency Response Operations" = "#F8924D",
  "Linking Public Health and Security Authorities" = "#FFA76D",
  "Medical Countermeasures and Personnel Deployment" = "#FFBC8E",
  "Infection Prevention and Control" = "#FFD2B0",
  "Risk Communication" = "#FFE8D2"
)

other_colors <- c(
  "Points of Entry" = "#667A8C",
  "Chemical Events" = "#94A5B8", 
  "Radiation Emergencies" = "#C5D0DC"
)

cc_colors <- c(prevent_colors, detect_colors, respond_colors, other_colors)

####################################################################################################
## Question: What are the overall costs, per country and core capacity? ############################
####################################################################################################

line_items |>
  group_by(country_factor, pillar, core_capacity_mapped) |>
  summarize(total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  ggplot(aes(x = country_factor, y = total_capacity_cost, fill = core_capacity_mapped)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Estimated Health Security Capacity-Building Cost by Country",
    subtitle = "Estimated total 5-year costs in 2024 USD",
    y = "Cost (2024 USD)",
    x = "",
    fill = "") +
  scale_y_continuous(labels = scales::label_number(prefix = "$", scale_cut = scales::cut_short_scale())) +
  scale_fill_manual(values = cc_colors,
                    breaks = names(cc_colors)) +
  theme_minimal() +
  theme(text = element_text(colour = "gray5", family = "Barlow"),
        panel.grid = element_blank(),
        legend.position = "bottom") 

####################################################################################################
## Question: What are the most expensive pillars/core capacities for each country? #################
####################################################################################################

line_items |>
  group_by(country_factor, pillar, core_capacity_mapped) |>
  summarize(total_capacity_cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = pillar, y = total_capacity_cost, fill = core_capacity_mapped)) +
  geom_bar(stat = "identity") +  # Regular stacked bars, not proportional
  facet_wrap(~ country_factor, scales = "free_y", ncol = 3) +  # Separate panel for each country
  labs(
    title = "Estimated Health Security Capacity-Building Cost by Country",
    subtitle = "Estimated total 5-year costs in 2024 USD",
    y = "Cost (2024 USD)",
    x = "") +
  scale_y_continuous(labels = scales::label_number(prefix = "$", scale_cut = scales::cut_short_scale())) +
  scale_fill_manual(values = cc_colors) +
  theme_minimal() +
  theme(
    text = element_text(colour = "gray5", family = "Barlow"),
    legend.position = "none",  # Remove the legend
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

####################################################################################################
## Question: What are the most expensive individual actions, per country? ##########################
####################################################################################################

top_3_by_country <- line_items |>
  left_join(specified_labels, join_by(requirement == requirement)) |>
  group_by(country_factor, pillar, core_capacity_mapped, requirement, brief_label) |>
  summarize(total_cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  group_by(country_factor) |>
  arrange(desc(total_cost), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  mutate(is_top_3 = rank <= 3) |>
  ungroup()

pillar_colors <- c(
  "Prevent" = "#158A4A",
  "Detect" = "#FFD12E",
  "Respond" = "#E67E3A",
  "Other" = "#667A8C")

# Ensure your plot uses the pillar column for coloring
ggplot(top_3_by_country, aes(x = total_cost, y = country_factor, color = pillar)) +
  # Horizontal lines
  geom_segment(aes(x = 0, xend = max(total_cost) * 1.1, 
                   y = country_factor, yend = country_factor),
               color = "gray90", size = 0.8) +
  # Background points with low opacity
  geom_point(alpha = 0.1, size = 2) +
  # Top 3 points highlighted
  geom_point(data = filter(top_3_by_country, is_top_3 == TRUE), 
             aes(fill = pillar), size = 4) +
  # Labels for top 3, positioned above dots with colors and wrapped text
  geom_text(data = filter(top_3_by_country, is_top_3 == TRUE), 
            aes(label = str_wrap(brief_label, width = 20), color = pillar),
            vjust = -0.8, family = "Barlow", size = 3) +
  # Apply the pillar color palette
  scale_color_manual(values = pillar_colors) +
  # X-axis formatting
  scale_x_log10(labels = scales::label_number(prefix = "$", scale_cut = scales::cut_short_scale()),
                     expand = expansion(mult = c(0, 0.25))) +
  # Labels and theme
  labs(
    title = "Top Health Security Investments by Country",
    subtitle = "Highlighting the 3 most expensive capacity areas for each country",
    x = "Cost (2024 USD)",
    y = "",
    color = "") +
  theme_minimal() +
  theme(
    text = element_text(colour = "gray5", family = "Barlow"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none")

####################################################################################################
## Appendix: Additional/rejected figure ideas ######################################################
####################################################################################################


####################################################################################################
## Question: What are the overall costs, per country and core capacity? ############################
####################################################################################################

## notes: 

## Eritrea: Reporting
## while Eritrea displays some high-level summary costs regarding reporting,
## no line-item costs are included in the publicly available NAPHS, and as such,
## data are considered "missing data"

## Nigeria: Immunization
## Estimated costs of Immunization in Nigeria are costed in another document
## and are not considered at the line-item level in their publicly available NAPHS

## Timor-Leste: Reporting
## From NAPHS: "The reporting requirements under the IHR are established and function in Timor- Leste. 
## The two recommendations from the JEE for reporting are duplicated elsewhere and are therefore
## not included in the planning matrix."

## bar chart
line_items |>
  group_by(country, core_capacity_mapped) |>
  summarize(
    total_line_items = n(),
    costed_line_items = sum(complete.cases(cost_usd2024)),
    total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  ggplot(aes(y = core_capacity_mapped, x = total_capacity_cost)) +
  geom_bar(stat = "identity", fill = "#31438D", color = "black") +
  facet_wrap(~country) +
  labs(
    title = "Estimated Health Security Capacity-Building Cost by Country",
    subtitle = "Estimated total 5-year costs in 2024 USD",
    y = "", 
    x = "Cost (2024 USD)") +
  scale_x_continuous(labels = scales::label_number(prefix = "$", scale_cut = scales::cut_short_scale())) +
  theme_minimal() +
  theme(text = element_text(colour = "gray5", family = "Barlow")) 

## heatmap
line_items |>
  group_by(country, core_capacity_mapped) |>
  summarize(total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = country, y = core_capacity_mapped, fill = total_capacity_cost)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Cost (2024 USD)",
    option = "viridis", 
    trans = "log10", # Log scale helps with data that varies widely
    labels = scales::label_number(prefix = "$", scale_cut = scales::cut_short_scale()),
    guide = guide_colorbar(title.position = "top", barwidth = 10)) +
  theme_minimal() +
  theme(
    text = element_text(colour = "gray5", family = "Barlow"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12)) +
  labs(
    x = "",
    y = "",
    title = "Estimated Health Security Capacity-Building Cost by Country",
    subtitle = "Color intensity represents estimated 5-year cost in 2024 USD") 

####################################################################################################
## For each country, what % of funding requirements come from each core capacity? ##################
####################################################################################################

line_items |>
  group_by(country_factor, pillar, core_capacity_mapped) |>
  summarize(total_capacity_cost = sum(cost_usd2024, na.rm = TRUE)) |>
  ggplot(aes(x = country_factor, y = total_capacity_cost, fill = core_capacity_mapped)) +
  geom_bar(stat = "identity", position = "fill") +  # Changed to position="fill" for proportional bars
  labs(
    title = "Proportional Health Security Capacity-Building Cost by Country",
    subtitle = "Showing percentage distribution of 5-year costs",
    y = "Proportion of Cost",
    x = "",
    fill = "") +
  scale_y_continuous(labels = scales::percent) +  # Changed to percentage labels
  scale_fill_manual(
    values = cc_colors,
    breaks = rev(names(cc_colors))
  ) +
  guides(fill = guide_legend(ncol = 3)) +  
  theme_minimal() +
  theme(
    text = element_text(colour = "gray5", family = "Barlow"),
    legend.position = "bottom",
    panel.grid = element_blank()  # Removes all gridlines
  )


####################################################################################################
## For each country, what % of funding requirements come from each core capacity? ##################
####################################################################################################


# Create equal-size country data with pillar grouping
line_items_equal_countries <- line_items |>
  group_by(country_factor, pillar, core_capacity_mapped) |>
  summarize(total_capacity_cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  group_by(country_factor) |>
  mutate(
    proportion = total_capacity_cost / sum(total_capacity_cost),
    country_area = 1) |>
  mutate(area = proportion * country_area)


# Create the treemap with equal-sized countries and pillar grouping
ggplot(line_items_equal_countries, 
       aes(area = area, 
           fill = core_capacity_mapped, 
           subgroup = country_factor,
           subgroup2 = pillar)) + # Add second grouping level by pillar
  geom_treemap() +
  geom_treemap_subgroup2_border(color = "white", size = 0.5) + # Lighter border around pillars
  geom_treemap_subgroup_border(color = "white", size = 2) + # White border around countries
  geom_treemap_subgroup_text(aes(label = country_factor), 
                             place = "center", 
                             color = "black",
                             fontface = "bold",
                             size = 14,
                             min.size = 0) +
  facet_wrap(~ country_factor, ncol = 3) +  # Create a separate panel for each country
  scale_fill_manual(values = cc_colors) +
  labs(
    title = "Proportional Health Security Capacity-Building Costs by Country",
    subtitle = "Area within each country represents proportion of costs, grouped by pillar"
  ) +
  theme_minimal() +
  theme(
    text = element_text(colour = "gray5", family = "Barlow"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank()  # Hide facet labels since we have them in the treemap
  )
