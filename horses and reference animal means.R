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
# dplyr
# FSA
# ggnewscales
# svglite
############################################################

# Load packages
library(ggplot2)
library(viridis)
library(dplyr)
library(FSA)
library(ggnewscale)
library(svglite)


# ==========================================================
# IMPORT DATA
# ==========================================================

horse_isotopes <- read.csv2(
  "Full isotopic dataset.csv",
  fileEncoding = "latin1"
)

animals_fennoscand <- read.csv2(
  "Reference_animals_Fennoscandia.csv",
  fileEncoding = "latin1"
)

animals_baltic <- read.csv2(
  "Reference_animals_Baltic.csv",
  fileEncoding = "latin1"
)

animals_eastern <- read.csv2(
  "Reference_animals_Eastern.csv",
  fileEncoding = "latin1"
)

# ==========================================================
# SUBSET HORSE DATA
# ==========================================================

fennoscand_data <- subset(
  horse_isotopes,
  Region == "Fennoscandia"
)

baltic_data <- subset(
  horse_isotopes,
  Region == "Baltic"
)

eastern_data <- subset(
  horse_isotopes,
  Region == "Eastern"
)


# ==========================================================
# CALCULATE REFERENCE MEANS ± SD
# ==========================================================

calculate_reference_means <- function(data) {
  
  data %>%
    group_by(Group) %>%
    summarise(
      mean_d13C = mean(d13C, na.rm = TRUE),
      mean_d15N = mean(d15N, na.rm = TRUE),
      sd_d13C = sd(d13C, na.rm = TRUE),
      sd_d15N = sd(d15N, na.rm = TRUE),
      .groups = "drop"
    )
  
}

fennoscand_reference <- calculate_reference_means(
  animals_fennoscand
)

baltic_reference <- calculate_reference_means(
  animals_baltic
)

eastern_reference <- calculate_reference_means(
  animals_eastern
)

# ==========================================================
# REFERENCE ANIMAL COLOURS
# ==========================================================

reference_colours <- c(
  "Cattle" = "#e7298a",
  "Hare" = "#e6ab02",
  "Moose" = "#1b9e77",
  "Ovicaprid" = "#d95f02",
  "Pig/wild boar" = "#6a3d9a",
  "Red deer" = "#a50f15",
  "Roe deer" = "#1f78b4",
  "Wild horse" = "black"
)

# ==========================================================
# FUNCTION:
# COMBINED REFERENCE + HORSE PLOT
# ==========================================================

create_combined_plot <- function(
    reference_means,
    horse_data,
    ymax = 12
) {
  
  ggplot(
    reference_means,
    aes(
      x = mean_d13C,
      y = mean_d15N
    )
  ) +
    
    geom_errorbar(
      aes(
        ymin = mean_d15N - sd_d15N,
        ymax = mean_d15N + sd_d15N,
        colour = Group
      ),
      width = 0.1,
      show.legend = FALSE
    ) +
    
    geom_errorbarh(
      aes(
        xmin = mean_d13C - sd_d13C,
        xmax = mean_d13C + sd_d13C,
        colour = Group
      ),
      height = 0.1,
      show.legend = FALSE
    ) +
    
    geom_point(
      aes(colour = Group),
      size = 5,
      shape = 3
    ) +
    
    scale_colour_manual(
      name = "Reference animals",
      values = reference_colours
    ) +
    
    new_scale_colour() +
    
    geom_point(
      data = horse_data,
      aes(
        x = d13C,
        y = d15N,
        colour = Subgroup,
        fill = Subgroup,
        shape = Subgroup
      ),
      size = 5,
      alpha = 0.8
    ) +
    
    scale_colour_viridis(
      name = "Archaeological horses",
      discrete = TRUE
    ) +
    
    scale_fill_viridis(
      name = "Archaeological horses",
      discrete = TRUE
    ) +
    
    scale_shape_manual(
      name = "Archaeological horses",
      values = rep(21:25, length.out = 20)
    ) +
    
    scale_x_continuous(
      limits = c(-25, -16),
      breaks = seq(-25, -16, 1)
    ) +
    
    scale_y_continuous(
      limits = c(2, ymax),
      breaks = seq(2, ymax, 1)
    ) +
    
    labs(
      x = expression(delta^13 * "C (‰)"),
      y = expression(delta^15 * "N (‰)")
    ) +
    
    theme_classic(base_size = 15)
  
}

# ==========================================================
# FENNOSCANDIA
# ==========================================================

horse_animals_plot_fennoscand <- create_combined_plot(
  reference_means = fennoscand_reference,
  horse_data = fennoscand_data,
  ymax = 12
)

print(horse_animals_plot_fennoscand)

ggsave(
  filename = "figures/horse_animals_plot_fennoscand.svg",
  plot = horse_animals_plot_fennoscand,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "figures/horse_animals_plot_fennoscand.png",
  plot = horse_animals_plot_fennoscand,
  width = 8,
  height = 6,
  dpi = 600
)

# ==========================================================
# BALTIC
# ==========================================================

horse_animals_plot_baltic <- create_combined_plot(
  reference_means = baltic_reference,
  horse_data = baltic_data,
  ymax = 12
)

print(horse_animals_plot_baltic)

ggsave(
  filename = "figures/horse_animals_plot_baltic.svg",
  plot = horse_animals_plot_baltic,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "figures/horse_animals_plot_baltic.png",
  plot = horse_animals_plot_baltic,
  width = 8,
  height = 6,
  dpi = 600
)

# ==========================================================
# EASTERN
# ==========================================================

horse_animals_plot_eastern <- create_combined_plot(
  reference_means = eastern_reference,
  horse_data = eastern_data,
  ymax = 15
)

print(horse_animals_plot_eastern)

ggsave(
  filename = "figures/horse_animals_plot_eastern.svg",
  plot = horse_animals_plot_eastern,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "figures/horse_animals_plot_eastern.png",
  plot = horse_animals_plot_eastern,
  width = 8,
  height = 6,
  dpi = 600
)


# ==========================================================
# IMAGE POST-PROCESSING
# ==========================================================

# Image post-processing in Inkscape https://inkscape.org/
# Taxa silhouette images from https://www.phylopic.org/
# Silhouettes centered to mean in Inkscape



# ==========================================================
# Statistical testing
# ==========================================================

# ==========================================================
# FILTER DATASETS
# Exclude wild horses and taxa of mixed domestic/wild status
# ==========================================================

animals_fennoscand_filtered <- animals_fennoscand %>%
  filter(!Status %in% c("Wild horse", "Wild/domestic")) %>%
  droplevels()

animals_baltic_filtered <- animals_baltic %>%
  filter(!Status %in% c("Wild horse", "Wild/domestic")) %>%
  droplevels()

animals_eastern_filtered <- animals_eastern %>%
  filter(!Status %in% c("Wild horse", "Wild/domestic")) %>%
  droplevels()

# ==========================================================
# TESTS OF NORMALITY
# ==========================================================

# Fennoscandia
shapiro.test(animals_fennoscand_filtered$d13C)
shapiro.test(animals_fennoscand_filtered$d15N)

# Baltic
shapiro.test(animals_baltic_filtered$d13C)
shapiro.test(animals_baltic_filtered$d15N)

# eastern
shapiro.test(animals_eastern_filtered$d13C)
shapiro.test(animals_eastern_filtered$d15N)


# ==========================================================
# WILCOXON RANK-SUM TESTS
# ==========================================================

# Fennoscandia
wilcox.test(d13C ~ Status, data = animals_fennoscand_filtered)
wilcox.test(d15N ~ Status, data = animals_fennoscand_filtered)

# Baltic
wilcox.test(d13C ~ Status, data = animals_baltic_filtered)
wilcox.test(d15N ~ Status, data = animals_baltic_filtered)

# Eastern Europe
wilcox.test(d13C ~ Status, data = animals_eastern_filtered)
wilcox.test(d15N ~ Status, data = animals_eastern_filtered)


# ==========================================================
# HOMOGENEITY OF VARIANCES
# ==========================================================

leveneTest(d13C ~ Status, data = animals_fennoscand_filtered)
leveneTest(d15N ~ Status, data = animals_fennoscand_filtered)

leveneTest(d13C ~ Status, data = animals_baltic_filtered)
leveneTest(d15N ~ Status, data = animals_baltic_filtered)

leveneTest(d13C ~ Status, data = animals_eastern_filtered)
leveneTest(d15N ~ Status, data = animals_eastern_filtered)


# ==========================================================
# DESCRIPTIVE STATISTICS
# Medians and sample sizes by group
# ==========================================================

animals_fennoscand_filtered %>%
  group_by(Status) %>%
  summarise(
    n = n(),
    median_d13C = median(d13C, na.rm = TRUE),
    median_d15N = median(d15N, na.rm = TRUE)
  )

animals_baltic_filtered %>%
  group_by(Status) %>%
  summarise(
    n = n(),
    median_d13C = median(d13C, na.rm = TRUE),
    median_d15N = median(d15N, na.rm = TRUE)
  )

animals_eastern_filtered %>%
  group_by(Status) %>%
  summarise(
    n = n(),
    median_d13C = median(d13C, na.rm = TRUE),
    median_d15N = median(d15N, na.rm = TRUE)
  )

# ==========================================================