# setup.R - Package installation and data preparation
# EJPR Paper: COVID Survey Replication Files

renv::restore() # Restore the environment from the lockfile

# Install and load required packages
packages <- c(
  # Data manipulation
  "tidyverse", "stringr", "dplyr", "readr", "conflicted",
  
  # Data visualization
  "ggplot2", "RColorBrewer",  
   "grid", "egg", "ggpubr", "tidyr",
  
  # Analysis
  "broom",
  "RColorBrewer", "grid", 
  "broom"
)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
library(cregg)

# Resolve conflicts
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer('ggarrange', 'egg')
conflicted::conflict_prefer('ggsave', 'ggplot2')
conflicted::conflict_prefer('select', 'dplyr')
conflicted::conflict_prefer('mutate', 'dplyr')

# Set graph parameters
par(family = "LM Roman 10")

# Project paths (customize according to your setup)
# Using variable paths to make the code more portable
ROOT_PATH <- getwd() # Adjust this to your project root directory
DATA_PATH <- file.path(ROOT_PATH, "data")
OUTPUT_PATH <- file.path(ROOT_PATH, "output")
PLOTS_PATH <- file.path(OUTPUT_PATH, "plots")

# Create directories if they don't exist
dir.create(OUTPUT_PATH, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOTS_PATH, showWarnings = FALSE, recursive = TRUE)

# Load dataset
dat_combined <- readRDS(file.path(DATA_PATH, "conjoint_pooled_dataset.rds"))

# Create country-specific datasets
dat_AU <- dat_combined %>%
  filter(country == "Australia")

dat_UK <- dat_combined %>%
  filter(country == "United Kingdom")

# Load the marginals datasets
datmarginals_uk <- read_csv(file.path(DATA_PATH, "uk_final.csv"))
datmarginals_au <- read_csv(file.path(DATA_PATH, "AU_final.csv"))

#Load the covid data
covid_data <- read.csv(file.path(DATA_PATH, "owid-covid-data.csv"))
covid_data$date <- as.Date(covid_data$date)

# Load the atsi firms data
atsi_firms <- read.csv(file.path(DATA_PATH, "atsi_firm_data.csv"))

# Print confirmation message
cat("Setup complete. Data loaded and working environment prepared.\n")

