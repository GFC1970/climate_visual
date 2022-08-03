source("scripts/0_Functions.R")

library(scales)

df <- read_csv("data/GLB.Ts+dSST.csv",
               skip = 1,
               na = "***",
               show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  select(1, 16, 17, 18, 19) %>%
  pivot_longer(-year) %>%
  mutate(name = recode(
    name,
    "djf" = "Winter (December - February)",
    "mam" = "Spring (March - May)",
    "jja" = "Summer (June - August)",
    "son" = "Autumn (September - November)"
  )) %>%
  drop_na()

df$name <- factor(df$name, levels = c("Winter (December - February)", "Spring (March - May)", "Summer (June - August)", "Autumn (September - November)"))

df %>%
  ggplot(aes(x = year, y = value, fill = value)) +
  geom_col(show.legend = FALSE) +
  scale_fill_stepsn(colours=c("steelblue", "white", "tomato"),
                    values = rescale(c(min(df$value), 0, max(df$value))),
                    limits = c(min(df$value), max(df$value)),
                    n.breaks=9) +
  scale_x_continuous(breaks = seq(1880, 2020, 40),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.6, 1.6),
                     breaks = seq(-0.6, 1.4, .3),
                     expand = c(0,0)) +
  facet_wrap(~ name, scales = "free_y") +
  theme_gfc() +
  theme(
    strip.text = element_text(colour = "black",
                              hjust = 0,
                              face = "bold"),
    strip.background = element_blank(),
    plot.subtitle = element_textbox_simple()
  ) +
  labs(
    title = "Global Land-Ocean Temperature Index",
    subtitle = "Deviation in annual global mean temperatures from the normalized temperatures of 1951 to 1980 grouped by season. Winter and Spring have seen the largest increase in deviation.\n",
    caption = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS",
    x = "Year",
    y = "Temperature Deviation (\u00B0C)"
  ) +

  # Final adjustment to plot to ensure all items shown
  coord_cartesian(clip = "off")

export_plot("figures/temperature_bar_season.png")
