source("scripts/0_Functions.R")

seasons <- rev(c("djf", "mam", "jja", "son"))

pretty_seasons <- rev(c("djf" = "Winter", "mam" = "Spring", "jja" = "Summer", "son" = "Autumn"))

season_data <- read_csv("data/GLB.Ts+dSST.csv",
  skip = 1,
  na = "***",
  show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  select(year, all_of(seasons)) %>%
  pivot_longer(-year, names_to = "season", values_to = "temp") %>%
  mutate(season = factor(season, levels = seasons),
         season_index = as.numeric(season))

this_year <- season_data %>%
  filter(year == 2021)

season_data %>%
  ggplot(aes(x = temp, xend = temp,
             y = season_index - .25, yend = season_index + .25)) +
  geom_segment(colour = "black", alpha = .3, size = 1.6) +
  geom_segment(data = this_year, aes(colour = temp),
               size = 3, lineend = "round") +
  geom_vline(xintercept = 0, colour = "grey50") +
  scale_y_continuous(breaks = 1:4,
                     labels = pretty_seasons) +
  scale_x_continuous(breaks = seq(-0.75, 1.5, .25),
                     labels = seq(-0.75, 1.5, .25),
                     limits = c(-0.75, 1.5)) +
  scale_colour_gradient2(low = "steelblue", mid = "white", high = "tomato",
                         midpoint = 0, guide = "none") +
  theme_gfc() +
  theme(
    panel.grid.major.x = element_line(colour = "grey20")
  ) +
  labs(
    title = "Annual Temperature Anomoly by Season 1880 - 2021",
    subtitle = "Most recent year shown with coloured bar",
    caption = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS",
    x = "Temperatue Anomoly (\u00B0C)",
    y = NULL
  )

export_plot("figures/temperature_rug.png")
