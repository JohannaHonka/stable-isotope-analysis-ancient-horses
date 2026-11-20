############################################################
# Stable isotope analysis of ancient horses
# Violin plot of δ15N values by geographic subgroup
#
# Author: Johanna Honka
# Date: 2026
#
# Required packages:
# ggplot2
# viridis
############################################################

# Load libraries
library(ggplot2)
library(viridis)

#optional for .svg export
library(svglite)


#-----------------------------------------------------------
# Import data
#-----------------------------------------------------------

#Use read.csv() for comma-separated files with dot decimals
#Use read.csv2() for semicolon-separated files with comma decimals

isotope_data <- read.csv2(
  "Full isotopic dataset.csv",
  fileEncoding = "latin1"
)

#Rename isotope column if necessary to "d15N"
#Special characters in column name e.g "δ15N (‰)" could be problematic


# Ensure isotope values are numeric
isotope_data$d15N <- as.numeric(isotope_data$d15N)

#-----------------------------------------------------------
# Define subgroup order (west to east)
#-----------------------------------------------------------

isotope_data$Subgroup <- factor(
  isotope_data$Subgroup,
  levels = c(
    "Finland",
    "Sweden",
    "Denmark",
    "Estonia",
    "Lithuania/Latvia",
    "Western wild",
    "Yaroslavl",
    "Middle Volga",
    "Caucasus",
    "western Kazakstan/Ural",
    "Eastern wild"
  )
)

#-----------------------------------------------------------
# Generate plot
#-----------------------------------------------------------

d15N_plot <- ggplot(
  isotope_data,
  aes(
    x = Subgroup,
    y = d15N,
    fill = Subgroup
  )
) +
  
  # Violin plot
  geom_violin(
    trim = FALSE,
    alpha = 0.3,
    colour = "black"
  ) +
  
  # Boxplot
  geom_boxplot(
    width = 0.2,
    outlier.shape = NA,
    alpha = 0.8,
    colour = "black"
  ) +
  
  # Individual observations
  geom_jitter(
    width = 0.12,
    size = 1.8,
    alpha = 0.3,
    colour = "black"
  ) +
  
  # Mean ± SE
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    color = "black",
    linewidth = 0.5
  ) +
  
  # Mean marker
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    size = 3,
    stroke = 1.5,
    colour = "black"
  ) +
  
  # Viridis palette
  scale_fill_viridis_d(option = "D") +
  
  labs(
    x = NULL,
    y = expression(delta^15 * N ~ "(‰)")
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 16
    ),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "none"
  ) +

  # Y-axis limits
scale_y_continuous(
  breaks = seq(0, 16, by = 2)
)

# Display plot
print(d15N_plot)

#-----------------------------------------------------------
# Export figure
#-----------------------------------------------------------

ggsave(
  filename = "Figure_d15N_violin_plot.png",
  plot = d15N_plot,
  width = 10,
  height = 6,
  dpi = 600
)

# SVG (vector format) for potential figure post-processing
ggsave(
  filename = "Figure_d15N_violin_plot.svg",
  plot = d15N_plot,
  width = 10,
  height = 6,
  units = "in"
)