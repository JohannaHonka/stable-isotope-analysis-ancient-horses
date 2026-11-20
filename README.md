# stable-isotope-analysis-ancient-horses
R scripts and input files in this repository accompany the manuscript "Feeding of prehistoric and historic horses in the Baltic Sea region and western Russia" submitted to journal Archaeological and Anthropological Sciences.

**Overview**
This repository contains the R scripts used for the analysis and visualization of carbon (δ¹³C) and nitrogen (δ¹⁵N) stable isotope data from archaeological horses. The study investigates regional variation in horse diets and management practices across northern Europe and western Russia using collagen stable isotope values.

**Repository Structure**
├── data/ Baltic_SIBER.csv eastern_SIBER.csv Fennoscandia_SIBER.csv Full isotopic dataset.csv Reference animals_Baltic.csv Reference animals_eastern.csv Reference animals_Fennoscandia.csv Temporal groups.csv Wild horses.csv Wild horses_SIBER.csv 
├── scripts/ horses and reference animal means.R isotopic_biplots.R SIBER analyses.R statistical testing.R violin_plot_d13C.R violin_plot_d15N.R
└── README.md

**Requirements**
Software
R ≥ 4.3
RStudio (recommended)

R Packages
library(ggplot2)
library(dplyr)
library(car)
library(tidyverse)
library(viridis)
library(ggnewscale)
library(scales)
library(SIBER)
library(svglite)
library(FSA)

**Data**
The dataset contains collagen stable isotope measurements from archaeological horses and comparative herbivore taxa.

**Citation**
If using the scripts or derived data, please cite the associated pre-print:

Honka J. et al. (2025) Feeding of prehistoric and historic horses at the eastern Baltic Sea region and western Russia, 15 July 2025, PREPRINT (Version 1) available at Research Square [https://doi.org/10.21203/rs.3.rs-7044177/v1]

Contact
Johanna Honka
Postdoctoral Researcher
University of Oulu
Oulu, Finland
