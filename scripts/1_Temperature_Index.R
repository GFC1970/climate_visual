# Purpose:
# This figure shows the deviation in annual global mean
# temperatures from the normalized temperatures of 1951 to 1980.

# Created by:   Cox, Graham
# Created on:   2022-08-02

# Load libraries functions
source("scripts/0_Functions.R")

# Load data from csv file downloaded from NASA
df <- read_csv("data/GLB.Ts+dSST.csv",

  # remove top header row
  skip = 1,

  # Show NA values for *** entries
  na = "***",

  # hide data details when loading
  show_col_types = FALSE) %>%

  # clean variable names
  janitor::clean_names()

# Select required variables
df %>%
  select(year, temp = `j_d`) %>%

  # remove NA values
  drop_na() %>%

  # Create ggplot
  ggplot(aes(x = year, y = temp)) +

  # Add first line to show temperatue deviation for each year
  geom_line(aes(colour = "1"), size = .5, show.legend = FALSE) +

  # Add points to line
  geom_point(
    aes(colour = "1"),
    fill = "white",
    shape = 21,
    show.legend = TRUE
  ) +

  # Add smoothed line to plot
  geom_smooth(
    aes(colour = "2"),
    size = .5,
    se = FALSE,
    span = .15,
    show.legend = FALSE
  ) +

  # Amend breaks of labels on x axis
  scale_x_continuous(breaks = seq(1880, 2025, 20),
                     expand = c(0, 0)) +

  # Amend breaks on labels on y axis
  scale_y_continuous(limits = c(-0.5, 1.5),
                     expand = c(0, 0)) +

  # Set colours of lines based on the colour values
  # previously set in the `colour` parameter above
  # i.e "1" or "2"
  # Amend labels to show in legend and override
  # aesthetics for legend key shape
  scale_colour_manual(
    name = NULL,
    breaks = c(1, 2),
    values = c("darkgrey", "steelblue"),
    labels = c("Annual Mean Temperature", "Lowess Smoothing"),
    guide = guide_legend(override.aes = list(shape = 15, size = 5))
  ) +

  # Add labels to plot
  labs(
    title = "Global Land-Ocean Temperature Index",
    subtitle = "Deviation in annual global mean temperatures from the normalized temperatures of 1951 to 1980.\n",
    caption = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS",
    x = "Year",
    y = "Temperature Deviation (\u00B0C)"
  ) +

  # Apply standard gfc theme
  theme_gfc() +

  # Apply additional theme components
  theme(
    legend.margin = margin(rep(0, 4)),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +

  # Final adjustment to plot to ensure all items shown
  coord_cartesian(clip = "off")

# Save to a png file in figures directory
export_plot("figures/temperature_index.png")
