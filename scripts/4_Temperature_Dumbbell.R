source("scripts/0_Functions.R")

df <- read_csv("data/GLB.Ts+dSST.csv",
  skip = 1,
  na = "***",
  show_col_types = FALSE) %>%
  janitor::clean_names()

df %>%
  select(1:13) %>%
  pivot_longer(-year) %>%
  group_by(year) %>%
  summarise(a_min_temp = min(value),
            b_max_temp = max(value),
            .groups = "drop") %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = value, y = year, colour = name, group = rev(year))) +
  geom_line(colour = "darkgrey", size = .7, show.legend = FALSE) +
  geom_point(size = 2.5) +
  scale_y_continuous(breaks = seq(1880, 2020, 10)) +
  scale_color_manual(name=NULL,
                     breaks=c("a_min_temp", "b_max_temp"),
                     values=c("tomato", "steelblue"),
                     labels=c("Minimum Deviation", "Maximumm Deviation")) +
  labs(title = "Minimum and Maximum Temperature Anomalies",
       subtitle = "1880 - 2020",
       caption = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS",
       x = NULL,
       y = NULL) +
  theme_gfc() +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom"
  )

export_plot("figures/temperature_dumbbell.png")
