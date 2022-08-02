# Purpose: PLot of temperature deviation using bars and colour scales
# Created by:   Cox, Graham
# Created on:   2022-08-02

source("scripts/0_Functions.R")

library(scales)

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

df <- df %>%
  select(year, temp = `j_d`) %>%
  drop_na()

annotation <- df %>%
  arrange(year) %>%
  slice(1, n()) %>%
  mutate(t_diff = 0,
         x = year + c(-5, 5))

max_t_diff <- format(round(max(df$temp), 1), nsmall=1)

df %>%
  ggplot(aes(x=year, y=temp, fill=temp)) +
  geom_col(show.legend=FALSE) +
  scale_fill_stepsn(colors=c("steelblue", "white", "tomato"),
                    values = rescale(c(min(df$temp), 0, max(df$temp))),
                    limits = c(min(df$temp), max(df$temp)),
                    n.breaks=9) +
  theme_gfc() +
  scale_x_continuous(breaks = seq(1880, 2025, 20),
                     expand = c(0, 0)) +

  # Amend breaks on labels on y axis
  scale_y_continuous(limits = c(-0.6, 1.2),
                     expand = c(0, 0)) +
  labs(
    title = "Global Land-Ocean Temperature Index",
    subtitle = "Deviation in annual global mean temperatures from the normalized temperatures of 1951 to 1980.\n",
    caption = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS",
    x = "Year",
    y = "Temperature Deviation (\u00B0C)"
  ) +

  # Final adjustment to plot to ensure all items shown
  coord_cartesian(clip = "off")

export_plot("figures/temperatue_bar.png")
