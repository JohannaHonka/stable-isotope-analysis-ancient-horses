############################################################
# Stable isotope analysis of ancient horses
# Stable isotope SIBER analysis by temporal group
#
# Author: Johanna Honka
# Date: 2026
#
# Required packages:
# SIBER
# ggplot2
# dplyr
# viridis
# svglite
############################################################

# Load packages
library(SIBER)
library(ggplot2)
library(dplyr)
library(viridis)
library(svglite)

# Reproducibility
set.seed(12345)


#-----------------------------------------------------------
# Import data
#-----------------------------------------------------------

#Use read.csv() for comma-separated files with dot decimals
#Use read.csv2() for semicolon-separated files with comma decimals

fennoscand <- read.csv2(
  file.path(data_dir, "Fennoscandia_SIBER.csv"),
  fileEncoding = "latin1"
)

baltic <- read.csv2(
  file.path(data_dir, "Baltic_SIBER.csv"),
  fileEncoding = "latin1"
)

eastern <- read.csv2(
  file.path(data_dir, "eastern_SIBER.csv"),
  fileEncoding = "latin1"
)

wild <- read.csv2(
  file.path(data_dir, "Wild horses_SIBER.csv"),
  fileEncoding = "latin1"
)


# Input and output directories

data_dir <- "data"
output_dir <- "results"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ============================================================
# Bayesian model settings
# ============================================================

parms <- list(
  n.iter = 13e4,
  n.burnin = 5e3,
  n.thin = 50,
  n.chains = 4
)

priors <- list(
  R = diag(2),
  k = 2,
  tau.mu = 1e-3
)


# ============================================================
# Ellipse plot function
# ============================================================

plot_ellipses <- function(data,
                          region,
                          xlim,
                          ylim) {
  
  p <- ggplot(
    data,
    aes(
      x = iso1,
      y = iso2,
      colour = group,
      fill = group
    )
  ) +
    geom_point(size = 3) +
    
    # Standard Ellipse Area (~40% of data)
    stat_ellipse(
      alpha = 0.25,
      level = 0.40,
      type = "norm",
      geom = "polygon"
    ) +
    
    labs(
      x = expression(paste(delta^13 * "C (‰)")),
      y = expression(paste(delta^15 * "N (‰)")),
      colour = "Horse group",
      fill = "Horse group"
    ) +
    
    theme_classic(base_size = 16) +
    
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    
    coord_cartesian(
      xlim = xlim,
      ylim = ylim
    )
  
  print(p)
  
  ggsave(
    filename = paste0("SIBER_", region, ".svg"),
    path = output_dir,
    plot = p,
    width = 7,
    height = 6,
    units = "in"
  )
  
  return(p)
}


# ============================================================
# SIBER workflow
# ============================================================

run_siber_analysis <- function(data,
                               region,
                               sea_ylim = 6) {
  
  cat("\n=====================================\n")
  cat(region, "\n")
  cat("=====================================\n")
  
  # Create SIBER object
  siber_object <- createSiberObject(data)
  
  # Maximum likelihood estimates
  ml_metrics <- groupMetricsML(siber_object)
  
  cat("\nMaximum likelihood metrics:\n")
  print(ml_metrics)
  
  # Fit Bayesian ellipses
  ellipses_posterior <- siberMVN(
    siber_object,
    parms,
    priors
  )
  
  # Standard ellipse area
  sea_b <- siberEllipses(ellipses_posterior)
  
  # Add group names
  colnames(sea_b) <- colnames(ml_metrics)
  
  # ----------------------------------------------------------
  # Density plot
  # ----------------------------------------------------------
  
  siberDensityPlot(
    sea_b,
    xlab = "Group",
    ylab = expression("Standard Ellipse Area" * "(‰"^2 * ")"),
    bty = "L",
    las = 1,
    main = paste(region, "SEA distributions"),
    ylims = c(0, max(sea_ylim, max(sea_b)))
  )
  
  points(
    1:ncol(sea_b),
    ml_metrics[3, ],
    col = "red",
    pch = 4,
    lwd = 2
  )
  
  # ----------------------------------------------------------
  # Posterior SEA plot
  # ----------------------------------------------------------
  
  sea_df <- data.frame(
    Group = rep(colnames(sea_b),
                each = nrow(sea_b)),
    SEA = as.vector(sea_b)
  )
  
  sea_plot <- ggplot(
    sea_df,
    aes(
      x = Group,
      y = SEA,
      colour = Group,
      fill = Group
    )
  ) +
    
    geom_boxplot(
      alpha = 0.25,
      outlier.shape = NA
    ) +
    
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    
    coord_cartesian(
      ylim = c(0, sea_ylim)
    ) +
    
    labs(
      x = NULL,
      y = expression("Standard Ellipse Area" * "(‰"^2 * ")"),
      colour = "Horse group",
      fill = "Horse group"
    ) +
    
    theme_classic(base_size = 16) +
    
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  print(sea_plot)
  
  ggsave(
    filename = paste0("SIBER_", region, "_SEAB.svg"),
    path = output_dir,
    plot = sea_plot,
    width = 7,
    height = 6,
    units = "in"
  )
  
  # ----------------------------------------------------------
  # Pairwise Bayesian comparisons
  # ----------------------------------------------------------
  
  n_groups <- ncol(sea_b)
  
  prob_matrix <- matrix(
    NA,
    nrow = n_groups,
    ncol = n_groups
  )
  
  rownames(prob_matrix) <- colnames(sea_b)
  colnames(prob_matrix) <- colnames(sea_b)
  
  for (i in 1:n_groups) {
    for (j in 1:n_groups) {
      prob_matrix[i, j] <- mean(
        sea_b[, i] > sea_b[, j]
      )
    }
  }
  
  cat("\nPosterior probability:\n")
  cat("Row group > column group\n\n")
  
  print(round(prob_matrix, 3))
  
  # ----------------------------------------------------------
  # Credible intervals
  # ----------------------------------------------------------
  
  sea_summary <- t(
    apply(
      sea_b,
      2,
      function(x) {
        
        c(
          Mean = mean(x),
          Lower95 = quantile(x, 0.025),
          Upper95 = quantile(x, 0.975)
        )
        
      }
    )
  )
  
  cat("\nSEA posterior summaries:\n")
  print(round(sea_summary, 3))
  
  return(
    list(
      ml_metrics = ml_metrics,
      sea_b = sea_b,
      probability_matrix = prob_matrix,
      sea_summary = sea_summary
    )
  )
}


#
#Run analyses
#
# ============================================================
# Fennoscandia
# ============================================================

plot_ellipses(
  fennoscand,
  region = "fennoscand",
  xlim = c(-24, -19),
  ylim = c(2, 10)
)

fennoscand_results <- run_siber_analysis(
  fennoscand,
  region = "fennoscand",
  sea_ylim = 6
)

# ============================================================
# Baltic
# ============================================================

plot_ellipses(
  baltic,
  region = "baltic",
  xlim = c(-24, -19),
  ylim = c(2, 10)
)

baltic_results <- run_siber_analysis(
  baltic,
  region = "baltic",
  sea_ylim = 6
)

# ============================================================
# Eastern
# ============================================================

plot_ellipses(
  eastern,
  region = "eastern",
  xlim = c(-24, -19),
  ylim = c(2, 10)
)

eastern_results <- run_siber_analysis(
  eastern,
  region = "eastern",
  sea_ylim = 6
)

# ============================================================
# Wild
# ============================================================

plot_ellipses(
  wild,
  region = "wild",
  xlim = c(-26, -17),
  ylim = c(1, 11)
)

wild_results <- run_siber_analysis(
  wild,
  region = "wild",
  sea_ylim = 11
)

# ============================================================
