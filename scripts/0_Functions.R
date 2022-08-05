# Purpose:      Functions for setting ggplot plots and exporting to image file
# Created by:   Cox, Graham
# Created on:   2022-MM-DD

# Script with

# Load libraries
library(tidyverse)
library(lubridate)
library(glue)
library(ggtext)
library(showtext)

# Set up fonts ---

font_add_google(family = "roboto-slab", "Roboto Slab")
font_add_google(family = "roboto-condensed", "Roboto Condensed")

showtext_auto()

#' theme_gfc
#'
#' Sets a theme on a ggplot2 plot and applies specific
#' values to certain items within the plot
#'
#' @return
#' @export
#'
#' @examples
theme_gfc <- function() {

  # Set basic theme
  theme_linedraw(base_size = 18,
                 base_family = "roboto-condensed") +

    # Set plot parameters
    theme(

      # Main plot margins
      plot.margin = margin(rep(1, 4), unit = "cm"),

      # Plot title
      plot.title = element_text(
        family = "roboto-slab",
        face = "bold",
        colour = "steelblue"
      ),
      plot.title.position = "plot",

      # Plot caption
      plot.caption = element_text(hjust = 0),
      plot.caption.position = "plot",

      # Panel
      panel.border = element_blank(),
      panel.grid = element_blank(),

      # Axis
      axis.line.x = element_line(size = .4)
    )
}


#' export_plot
#'
#' Exports the last created ggplot2 plot to an
#' image file with a standard width and height
#'
#' @param filename
#'
#' @return none
#' @export
#'
#' @examples
#' export_plot("figures/an_example_plot.png")
#'
export_plot <- function(filename) {
  ggsave(
    filename = filename,
    width = 1200,
    height = 850,
    units = "px",
    dpi = 144
  )
}

