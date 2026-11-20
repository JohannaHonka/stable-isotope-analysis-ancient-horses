############################################################
# Stable isotope analysis of ancient horses
# Stable isotope biplots by temporal group
#
# Author: Johanna Honka
# Date: 2026
#
# Required packages:
# ggplot2
# viridis
# scales
# svglite
############################################################

# Load packages
library(ggplot2)
library(viridis)
library(scales)
library(svglite)


#-----------------------------------------------------------
# Import data
#-----------------------------------------------------------

#Use read.csv() for comma-separated files with dot decimals
#Use read.csv2() for semicolon-separated files with comma decimals

isotope_data_temporal <- read.csv2(
  "Temporal group.csv",
  fileEncoding = "latin1"
)

isotope_data_wild <- read.csv2(
  "Wild horses.csv",
  fileEncoding = "latin1"
)
# ==========================================================
# Plotting function
# ==========================================================

create_temporal_plot <- function(data, grouping_variable, n_shapes) {
  
  ggplot(
    data,
    aes(
      x = d13C,
      y = d15N,
      colour = .data[[grouping_variable]],
      shape = .data[[grouping_variable]],
      fill   = .data[[grouping_variable]]
    )
  ) +
    geom_point(size = 6) +
    scale_colour_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    scale_shape_manual(values = rep(21:25, length.out = n_shapes)) +
    scale_x_continuous(
      breaks = pretty_breaks(n = 8)
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 8)
    ) +
    coord_cartesian(
      xlim = c(-25, -18),
      ylim = c(2, 14)
    ) +
    labs(
      x = expression(delta^13 * "C (‰)"),
      y = expression(delta^15 * "N (‰)")
    ) +
    theme_classic(base_size = 20)
}

# ==========================================================
# FENNOSCANDIA
# ==========================================================

fennoscandia_data <- subset(
  isotope_data_temporal,
  Group == "Fennoscandia"
)

fennoscandia_plot <- create_temporal_plot(
  data = fennoscandia_data,
  grouping_variable = "Temporal_group",
  n_shapes = 11
)

print(fennoscandia_plot)

ggsave(
  "horse_plot_fennoscandia.svg",
  fennoscandia_plot,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  "horse_plot_fennoscandia.png",
  fennoscandia_plot,
  width = 8,
  height = 6,
  dpi = 600
)

# ==========================================================
# BALTIC
# ==========================================================

baltic_data <- subset(
  isotope_data_temporal,
  Group == "Baltic"
)

baltic_plot <- create_temporal_plot(
  data = baltic_data,
  grouping_variable = "Temporal_group",
  n_shapes = 10
)

print(baltic_plot)

ggsave(
  "horse_plot_baltic.svg",
  baltic_plot,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  "horse_plot_baltic.png",
  baltic_plot,
  width = 8,
  height = 6,
  dpi = 600
)

# ==========================================================
# EASTERN
# ==========================================================

eastern_data <- subset(
  isotope_data_temporal,
  Group == "Eastern"
)

eastern_plot <- create_temporal_plot(
  data = eastern_data,
  grouping_variable = "Temporal_group",
  n_shapes = 7
)

print(eastern_plot)

ggsave(
  "horse_plot_eastern.svg",
  eastern_plot,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  "horse_plot_eastern.png",
  eastern_plot,
  width = 8,
  height = 6,
  dpi = 600
)

# ==========================================================
# WILD HORSES
# ==========================================================

wild_plot <- create_temporal_plot(
  data = isotope_data_wild,
  grouping_variable = "Group",
  n_shapes = 2
)

print(wild_plot)

ggsave(
  "horse_plot_wild.svg",
  wild_plot,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  "horse_plot_wild.png",
  wild_plot,
  width = 8,
  height = 6,
  dpi = 600
)

#With subgrouping
wild_plot <- create_temporal_plot(
  data = isotope_data_wild,
  grouping_variable = "Subgroup",
  n_shapes = 2
)

print(wild_plot_subgroups)

ggsave(
  "horse_plot_wild_subgroups.svg",
  wild_plot,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  "horse_plot_wild_subgroups.png",
  wild_plot,
  width = 8,
  height = 6,
  dpi = 600
)


# ==========================================================