###########################################################################
## Load required libraries ################################################
###########################################################################

## if you don't already have these installed, you'll need to install them
## using the command install.libraries()

library(readxl) ## to read in Excel files
library(dplyr) ## for data manipulation
library(here) ## for file path management
library(sysfonts) ## for fonts
library(showtext) ## for fonts
library(ggplot2) ## for making figures
library(treemapify) ## for treemaps
library(stringr) ## for string wrapping
library(viridis) ## for colors
library(colorspace) ## for colors
library(patchwork) ## for plots
library(ggtext) ## for plots
library(readr) ## to parse numbers

###########################################################################
## Load fonts #############################################################
###########################################################################

# Load fonts
font_add_google("Zalando Sans", "zalandosans")
font_add_google("Radio Canada Big", "radiocanadabig", regular.wt = 600)
showtext_auto()
theme_set(theme_minimal(base_family = "zalandosans"))

###########################################################################
## Specify filepath info ##################################################
###########################################################################

here::i_am("code/2_analysis.R")

####################################################################################################
## Source data cleaning script #####################################################################
####################################################################################################

source(here("code", "1_data_cleaning.R"))
       
####################################################################################################
## Read in clean data ##############################################################################
####################################################################################################

## read in cleaned summary cost data
summary_cost <- read_excel(here("data", "clean", "summary cost data.xlsx"),
                           col_types = c(
                             "numeric",    # summary_cost_id
                             "text",       # country
                             "text",       # pillar
                             "text",       # core_capacity
                             "numeric",    # cost_original
                             "text",       # currency_original
                             "numeric"))  # cost_usd2024
## line-item data
line_items <- read_excel(here("data", "clean", "line item data.xlsx"),
                         col_types = c(
                           "numeric",    # line_item_id
                           "text",       # country
                           "text",       # pillar
                           "text",       # core_capacity
                           "text",       # requirement
                           "text",       # activity
                           "text",       # year_numeric
                           "text",       # year_calendar
                           "text",    # cost_original
                           "text",       # currency_original
                           "numeric",    # cost_usd2024
                           "text",       # primary_category
                           "text" )) |>  # primary_subcategory
  mutate(cost_original = parse_number(cost_original, na = "N/A"))

####################################################################################################
## Variable types and colors for summary cost data #################################################
####################################################################################################

## manage numeric variables
summary_cost$cost_usd2024 <- summary_cost$cost_usd2024

## core capacity becomes a factor for plotting
summary_cost$core_capacity_mapped <- factor(summary_cost$core_capacity,
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

####################################################################################################
## Confirm basic counts ############################################################################
####################################################################################################

length(unique(summary_cost$country))

length(unique(line_items$country))

length(unique(c(summary_cost$country, line_items$country)))

####################################################################################################
## Variable types and colors for line-item data ####################################################
####################################################################################################

## manage numeric variables
line_items$cost_usd2024 <- as.numeric(line_items$cost_usd2024)

## core capacity becomes a factor for plotting
line_items$core_capacity_mapped <- factor(line_items$core_capacity,
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
## Manage colors and order consistently ############################################################
####################################################################################################

# Define one color per pillar
pillar_colors <- c(
  "Prevent" = "#25AB65",
  "Detect" = "#FFD12E",
  "Respond" = "#F8924D",
  "Other" = "#6B8CAE")

# Map long names to short names
category_labels <- c(
  "Workforce" = "Workforce",
  "Physical infrastructure" = "Physical infrastructure",
  "Medical countermeasures and supplies for healthcare delivery" = "MCM and healthcare supplies",
  "Other" = "Other",
  "Media" = "Media",
  "Digital infrastructure" = "Digital infrastructure")

# Map categories to colors
category_colors <- c(
  "Workforce" = "#00496FFF",
  "Physical infrastructure" = "#0F85A0FF",
  "Medical countermeasures and supplies for healthcare delivery" = "#EDD746FF",
  "Digital infrastructure" = "#ED8B00FF",
  "Media" = "#DD4124FF")

####################################################################################################
## Distribution of cost estimates across core capacities, by country ###############################
####################################################################################################

# Order countries by their most expensive core capacity
country_order_fig1 <- c(
  "Tanzania", "Nigeria", "South Sudan", "Afghanistan", "Myanmar", "Cameroon",
  "Timor-Leste", "Sri Lanka", "North Macedonia",
  "Uganda", "Sierra Leone", "Liberia", "Benin", "Eritrea")

# Prepare data with colors
cc_per_country <- summary_cost |>
  filter(core_capacity != "total") |>
  group_by(country, pillar, core_capacity_mapped) |>
  arrange(desc(cost_usd2024)) |>
  group_by(country) |>
  mutate(
    proportion = cost_usd2024 / sum(cost_usd2024, na.rm = TRUE),
    country_area = 1,
    area = proportion * country_area) |>
  ungroup() |>
  mutate(
    country = factor(country, levels = country_order_fig1),
    pillar = factor(pillar, levels = c("Prevent", "Detect", "Respond", "Other")),
    proportion = ifelse(is.na(proportion), 0, proportion),
    base_color = pillar_colors[as.character(pillar)],
    shaded_color = lighten(base_color, amount = 1 - proportion))

# Label all core capacities with their percentage
# bold (and black) any cell at or over 20% (rounded to whole percent)
cc_labels <- cc_per_country |>
  mutate(pct_rounded = round(proportion * 100),
         pct_label = paste0(pct_rounded, "%"),
         is_bold = pct_rounded >= 20,
         label_color = ifelse(is_bold, "black", "grey65"),
         label_face = ifelse(is_bold, "bold", "plain"))

# Create label data 
pillar_labels <- cc_per_country |>
  group_by(pillar) |>
  slice_max(order_by = core_capacity_mapped, n = 1) |>
  distinct(pillar, core_capacity_mapped)

# Main plot
p_main <- ggplot(cc_per_country, aes(x = country, y = core_capacity_mapped, fill = shaded_color)) +
  geom_tile(color = "grey80", linewidth = 0.25) +
  scale_fill_identity() +
  facet_grid(pillar ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL,
       caption = "Percentages represent share of total costed estimates per country.\nCells representing 20% or more (rounded to the nearest whole percent) are labeled in bold black; others are shown in grey.\nAll percentages are rounded to the nearest whole percent, and may not sum exactly to 100% due to rounding error.") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    plot.caption = element_text(size = 8, hjust = 1, color = "grey50")) +
  geom_text(data = cc_labels |> filter(is_bold),
            aes(x = country, y = core_capacity_mapped, label = pct_label),
            size = 2.5, color = "black", fontface = "bold", family = "zalandosans", inherit.aes = FALSE) +
  geom_text(data = cc_labels |> filter(!is_bold),
            aes(x = country, y = core_capacity_mapped, label = pct_label),
            size = 2.5, color = "grey65", fontface = "plain", family = "zalandosans", inherit.aes = FALSE) +
  geom_text(data = pillar_labels, aes(x = -Inf, y = core_capacity_mapped, label = pillar),
            hjust = 1.1, vjust = -1, fontface = "bold", family = "zalandosans", inherit.aes = FALSE) +
  coord_cartesian(clip = "off")

# Title
p_title <- ggplot() +
  labs(title = "NAPHS Cost Distribution by Core Capacity",
       subtitle = "Proportion of total costed estimates by core capacity, per country") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, family = "radiocanadabig"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40", family = "radiocanadabig"),
    plot.margin = margin(b = -10))

# Combine: title and main
figure_1 <- p_title / p_main + plot_layout(heights = c(2, 25))

showtext_opts(dpi = 300)

ggsave(here("results", "figure 1.png"), plot = figure_1,
       width = 9, height = 6.5, units = "in", dpi = 300, bg = "white")

ggsave(here("results", "figure 1.jpeg"), plot = figure_1,
       width = 9, height = 6.5, units = "in", dpi = 300, bg = "white")

####################################################################################################
## Distribution of cost estimates across types of cost, by country #################################
####################################################################################################

category_per_country <- line_items |>
  filter(!is.na(cost_usd2024)) |>
  group_by(country, primary_category) |>
  summarize(category_cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  group_by(country) |>
  mutate(
    total_cost = sum(category_cost),
    pct = category_cost / total_cost * 100,
    rank = row_number(desc(pct))) |>
  arrange(country, desc(pct))

category_bar_data <- line_items |>
  filter(!is.na(cost_usd2024)) |>
  group_by(country, primary_category) |>
  summarize(cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  group_by(country) |>
  mutate(pct = cost / sum(cost) * 100)

workforce_order <- category_bar_data |>
  filter(primary_category == "Workforce") |>
  arrange(desc(pct)) |>
  pull(country)

category_bar_data <- category_bar_data |>
  mutate(country = factor(country, levels = rev(workforce_order)))

figure_2 <- ggplot(category_bar_data,
       aes(x = pct, y = country, fill = primary_category)) +
  geom_col(position = "stack", color = "black", linewidth = 0.2, width = 0.8) +
  scale_fill_manual(values = category_colors, labels = category_labels) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02)),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = function(x) paste0(x, "%")) +
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "NAPHS Cost Distribution by Category",
       subtitle = "Share of total costed estimates by cost category, per country",
       caption = "Countries are ordered by descending share of workforce-related costs.\nOnly countries with available line-item data considered for this analysis.") +
  theme_minimal(base_family = "zalandosans") +
  theme(legend.position = "top",
        legend.justification = "center",
        legend.margin = margin(t = 0, b = 0),
        legend.box.spacing = unit(10, "pt"),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.3, "cm"),
        legend.key.spacing.x = unit(2, "pt"),
        plot.title = element_text(face = "bold", size = 11, hjust = 0.5, family = "radiocanadabig",
                                  margin = margin(b = 2)),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40", family = "radiocanadabig",
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 6, hjust = 1, color = "grey50",
                                    margin = margin(t = 12)),
        plot.margin = margin(t = 6, r = 12, b = 4, l = 4),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE,
                             override.aes = list(color = "black", linewidth = 0.15)))

figure_2

showtext_opts(dpi = 300)

ggsave(here("results", "figure 2.png"), plot = figure_2,
       width = 6, height = 4, units = "in", dpi = 300, bg = "white")

ggsave(here("results", "figure 2.png"), plot = figure_2,
       width = 6, height = 4, units = "in", dpi = 300, bg = "white")

####################################################################################################
## Review data and calculate specific summary statistics ###########################################
####################################################################################################

## Review top ranked categories per country
## "In 8 of 10 countries with available line-item cost data, workforce costs were the largest category of investment"
category_per_country[which(category_per_country$rank == 1),]
table(category_per_country[which(category_per_country$rank == 1),]$primary_category)

## Median % of total costs for workforce, per country
## ".., representing a median of 68% of total cost per country
summary(category_per_country[which(category_per_country$primary_category == "Workforce"),]$pct)


## Percent of workforce costs that occur in core-capacities other then "workforce"
## "Notably, 88% of workforce spending was associated with core capacities other than workforce development,"

line_items |>
  ## identify which lines are in the workforce core capacity
  mutate(in_wf_cc = core_capacity == "Workforce") |>
  ## calculate total costs by whether or not they are in the workforce core capacity (above)
  ## and by functional domain 
  group_by(in_wf_cc, primary_category) |>
  summarize(total_cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  ## now look just at costs with the functional domain "workforce"
  ## what % of those costs were in the workforce core capacity?
  filter(primary_category == "Workforce") |>
  mutate(pct = total_cost / sum(total_cost) * 100)

####################################################################################################
## Workforce details ###############################################################################
####################################################################################################

## % of workforce costs that occur in core-capacities other then "workforce"

## overall 
line_items |>
  mutate(in_wf_cc = core_capacity == "Workforce") |>
  group_by(in_wf_cc, primary_category) |>
  summarize(total_cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  filter(primary_category == "Workforce") |>
  mutate(pct = total_cost / sum(total_cost) * 100)

## by core capacity
line_items |>
  group_by(core_capacity, primary_category) |>
  summarize(total_cost = sum(cost_usd2024, na.rm = TRUE), .groups = "drop") |>
  filter(primary_category == "Workforce") |>
  mutate(pct = total_cost / sum(total_cost) * 100) |>
  arrange(desc(pct))
  
