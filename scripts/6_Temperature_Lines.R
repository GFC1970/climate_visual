source("scripts/0_Functions.R")

library(scales)

df <- read_csv("data/GLB.Ts+dSST.csv",
  skip = 1,
  na = "***",
  show_col_types = FALSE) %>%
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to = "month", values_to = "temp") %>%
  drop_na()

last_dec <- df %>%
  filter(month == "Dec") %>%
  mutate(year = year + 1,
         month = "last_Dec")

next_jan <- df %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")

temps <- bind_rows(last_dec, df, next_jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1,
         this_year = year == 2022)

year_label <- temps %>%
  slice_max(year) %>%
  slice_max(month_number)


temps %>%
  ggplot(aes(x = month_number, y = temp, group = year, colour = year, size = this_year)) +
  geom_hline(yintercept = 0, color="white") +
  geom_line() +
  geom_text(data = year_label,
            aes(x=month_number, y=temp, label=year),
            inherit.aes = FALSE,
            hjust = 0, size = 5.5, nudge_x = 0.15, fontface = "bold") +
  scale_x_continuous(breaks=1:12,
                     labels=month.abb,
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_size_manual(breaks= c(FALSE, TRUE),
                    values = c(0.25, 1), guide = "none") +
  scale_colour_stepsn(colors=c("steelblue", "yellowgreen", "tomato"),
                    values = rescale(c(min(temps$temp), 0, max(temps$temp))),
                    n.breaks = 10) +
  coord_cartesian(xlim=c(1,12)) +
  theme_gfc() +
  theme(
    axis.line.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(4, "lines"),
    legend.key.height = unit(.4, "lines"),
    legend.text.align = 1,
    panel.grid = element_blank()
  ) +
  labs(
    title = "Global temperature change since 1880 by month",
    caption = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS",
    x = NULL,
    y = "Temperature Anomaly (\u00B0C)"
  )

export_plot("figures/temperature_lines.png")
