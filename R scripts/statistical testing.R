############################################################
# Stable isotope analysis of ancient horses
# Statistical testing by group and geographic subgroup
#
# Author: Johanna Honka
# Date: 2026
#
# Required packages:
# FSA
# car
############################################################

# Load libraries
library(FSA)
library(car)


# ============================================================
# Import data
# ============================================================

#Use read.csv() for comma-separated files with dot decimals
# Use read.csv2() for semicolon-separated files with comma decimals
isotope_data <- read.csv2(
  "Full isotopic dataset.csv",
  fileEncoding = "latin1"
)

# ============================================================
# Create regional datasets
# ============================================================

eastern_data <- subset(isotope_data, Group == "Eastern")
western_data <- subset(isotope_data, Group == "Western")

# ============================================================
# EASTERN VS WESTERN COMPARISON
# ============================================================

# ---------- Normality ----------
shapiro.test(eastern_data$d13C)
shapiro.test(western_data$d13C)

shapiro.test(eastern_data$d15N)
shapiro.test(western_data$d15N)

# ---------- Homogeneity of variances ---------
leveneTest(d13C ~ Group, data = isotope_data)
leveneTest(d15N ~ Group, data = isotope_data)

# ---------- Between-group comparisons ----------
# Non-parametric because at least one group deviated from normality

wilcox.test(d13C ~ Group, data = isotope_data)

wilcox.test(d15N ~ Group, data = isotope_data)

# ============================================================
# SUBGROUP ANALYSES: EASTERN POPULATIONS
# ============================================================

# ---------- d13C ----------

# Normality assessment
by(
  eastern_data$d13C,
  eastern_data$Subgroup,
  shapiro.test
)

# Homogeneity of variances
leveneTest(d13C ~ Subgroup, data = eastern_data)


# Kruskal-Wallis test
kruskal_d13C_east <- kruskal.test(
  d13C ~ Subgroup,
  data = eastern_data
)

kruskal_d13C_east

# Dunn's post hoc test (if Kruskal-Wallis significant)
dunn_d13C_east <- dunnTest(
  d13C ~ Subgroup,
  data = eastern_data,
  method = "bh"
)

dunn_d13C_east


# ---------- d15N ----------

# Normality assessment
by(
  eastern_data$d15N,
  eastern_data$Subgroup,
  shapiro.test
)

# Homogeneity of variances
leveneTest(d15N ~ Subgroup, data = eastern_data)

# Kruskal-Wallis test
kruskal_d15N_east <- kruskal.test(
  d15N ~ Subgroup,
  data = eastern_data
)

kruskal_d15N_east

# Dunn's post hoc test (if Kruskal-Wallis significant)
dunn_d15N_east <- dunnTest(
  d15N ~ Subgroup,
  data = eastern_data,
  method = "bh"
)

dunn_d15N_east


# ============================================================
# SUBGROUP ANALYSES: WESTERN POPULATIONS
# ============================================================

# ---------- d13C ----------

# Normality assessment
by(
  western_data$d13C,
  western_data$Subgroup,
  shapiro.test
)

# Homogeneity of variances
leveneTest(d13C ~ Subgroup, data = western_data)


# Kruskal-Wallis test
kruskal_d13C_west <- kruskal.test(
  d13C ~ Subgroup,
  data = western_data
)

kruskal_d13C_west

# Dunn's post hoc test (if Kruskal-Wallis significant)
dunn_d13C_west <- dunnTest(
  d13C ~ Subgroup,
  data = western_data,
  method = "bh"
)

dunn_d13C_west


# ---------- d15N ----------

# Normality assessment
by(
  western_data$d15N,
  western_data$Subgroup,
  shapiro.test
)

# Homogeneity of variances
leveneTest(d15N ~ Subgroup, data = western_data)

# One-way ANOVA
anova_d15N_west <- aov(
  d15N ~ Subgroup,
  data = western_data
)

summary(anova_d15N_west)

# Tukey HSD post hoc test
tukey_d15N_west <- TukeyHSD(
  anova_d15N_west
)

tukey_d15N_west
