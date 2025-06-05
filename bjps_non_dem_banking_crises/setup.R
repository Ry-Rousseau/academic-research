# setup.R - Package installation and environment setup
# BJPS Paper: Autocratic Bank Bailouts

# Clear environment
rm(list = ls())

# Set working directory to the project root
# Uncomment and modify as needed:
# setwd("G:/.shortcut-targets-by-id/1bpC_DXkiDUELjDlWIZvfQ4BgD0zURCNn/Banking Crises - Non Democracies JC/Banking Crises - Non Democracies/Replication Files")

# Install required packages if not already installed
packages <- c(
  # Data manipulation
  "tidyverse", "data.table", "lubridate", "haven", "readxl", "gmodels", "writexl",
  "stringr", "matrixStats", "clarify", "howManyImputations", "mitools", "rMIDAS", "reticulate",
  "forcats", "rnaturalearthdata", "rnaturalearth", "wbstats", "viridis", "countrycode",
  
  # Statistical analysis
  "lmtest", "sandwich", "MASS", "texreg", "broom", "estimatr",
  
  # Imputation
  "mice", "Amelia", 
  
  # Visualization
  "ggplot2", "gridExtra", "scales", "RColorBrewer",
  
  # Other utilities
  "stargazer", "foreach", "doParallel", "knitr", "rmarkdown"
)

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(ggplot2)
library(haven)
library(rnaturalearth)
library(sf)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(readxl)
library(data.table)
library(wbstats)
library("countrycode")

library(tidyverse)
library(data.table)
library("wbstats")
library(haven)
library(readxl)



# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# Project paths (use absolute paths)
PROJECT_PATH <- getwd()
DATA_PATH <- file.path(PROJECT_PATH, "data")
IMPUTED_DATA_PATH <- file.path(DATA_PATH, "imputed") # Folder with 100 imputed data sets
PROCESSED_DATA_PATH <- file.path(DATA_PATH, "processed") # Folder with .dta country year data
EXTERNAL_DATA_PATH <- file.path(DATA_PATH, "external") # Folder with all external data used for figures
OUTPUT_PATH <- file.path(PROJECT_PATH, "output")
FIGURE_PATH <- file.path(OUTPUT_PATH, "figures")
TABLE_PATH <- file.path(OUTPUT_PATH, "tables")
MIDAS_PATH <- file.path(OUTPUT_PATH, "midas")

# Custom functions 
source("functions.R")  

# Data Loading

## 1) Loading data
data <- read_dta(file.path(PROCESSED_DATA_PATH, "SBC_policy_response_in_non_democracies_january_2025.dta")) %>% 
  mutate(cy = paste0(year,country))

# Generate subsets for 3 autocracy samples
bmr_data<- data %>% filter(canonical_crisis==1,bmr_democracy==0) # This is for BMR autocracies
p2_data<-data %>% filter(canonical_crisis==1,polity2<7) # This is for Polity Autocracies
v2x_data<-data %>% filter(canonical_crisis==1,v2x_polyarchy<=0.5) # This is for V-Dem Autocracies

## 2) Loading imputed data

### Firstly load in imputed data sets as list
csv_files <- list.files(path = file.path(DATA_PATH, "imputed"), pattern = "\\.csv$", full.names = TRUE)
data_imp_prefilter <- lapply(csv_files, fread)

### Generate vectors of non-missing variables not in imputed data, 
# add in non-missing variables that aren't in imputed data 
#make vectors of country-years to keep becuase the dem variable will be imputed - a record of the canonical crisis country years
keep_polity2<- p2_data  %>% 
  dplyr::select(cy) %>% flatten %>% unlist
keep_v2x<- v2x_data  %>% 
  dplyr::select(cy) %>% flatten %>% unlist
keep_bmr<- bmr_data %>% 
  dplyr::select(cy) %>% flatten %>% unlist

### Generate subsets for 3 autocracy measure samples
data_imp_bmr <- lapply(data_imp_prefilter, function(x) mutate(x, cy = paste0(year,country)) %>% 
                         filter(cy%in%keep_bmr) %>% left_join(., data %>% dplyr::select(cy,index_canon_no,period_dummies72, all_crises_before_5_year, crises_last_10_years_5_year, crises_last_5_years_5_year, peer_average_cat3, peer_average_regional, v2x_regime_closed, v2x_regime),by="cy"))

data_imp_p2 <- lapply(data_imp_prefilter, function(x) mutate(x, cy = paste0(year,country)) %>% 
                        filter(cy%in%keep_polity2) %>% left_join(., data %>% dplyr::select(cy,index_canon_no,period_dummies72, all_crises_before_5_year, crises_last_10_years_5_year, crises_last_5_years_5_year, peer_average_cat3, peer_average_regional, v2x_regime_closed, v2x_regime),by="cy"))

data_imp_v2x <- lapply(data_imp_prefilter, function(x) mutate(x, cy = paste0(year,country)) %>% 
                         filter(cy%in%keep_v2x) %>% left_join(., data %>% dplyr::select(cy,index_canon_no,period_dummies72, all_crises_before_5_year, crises_last_10_years_5_year, crises_last_5_years_5_year, peer_average_cat3, peer_average_regional, v2x_regime_closed, v2x_regime),by="cy"))

# 3) Set- up Results Storage and Parameter Initialization

## Define two control sets
control_sets <- c("","+debt+partisan_cons+factor(period_dummies72)") # M2 and M4 have (period effects + debt + partisan_cons) whereas M1 and M3 do not

## Define data object
democracy_data_list <- list(
  BMR = list(nonimp = bmr_data, imp = data_imp_bmr),
  Polity = list(nonimp = p2_data, imp = data_imp_p2),
  `V-Dem` = list(nonimp = v2x_data, imp = data_imp_v2x)
)

# model labels
model_numbers <- c("M1","M2","M3","M4")

# Initialize an empty results storage for Models 1-4 iteration
model_results_empty <- data.frame(
  democracy_type = character(),# BMR, V-Dem or Polity, the underyling autocracy measure sample
  model_number = character(), # either M1, M2 (non-imputed) M3 or M4 (imputed)
  effect_name = character(), # Specifies the specific model coefficient, such as DC_Mand_Revise2 (DC/PF), the current control variable, or the interaction term 
  control_set = character(), # This is another way to distinguish between M1 and M2 or M3 and M4 - record the relevant value in the control_sets object - this is separate from our vector for learnin
  control_variable = character(), # This records which investment peer or learning effect we're including as a control variable, this is how we'll distinguish between different instances of DC_Mand_Revise2 in the effect_name column when we're creating the final table
  estimate = numeric(), # coefficient estimate
  std_error = numeric(),# coefficient standard error
  p_value = numeric(), # coefficient p_value
  nobs = integer(), # model number of observations
  signif_0_05 = logical(), # p-value is sig at 0.05
  signif_0_10 = logical(), # p-value is sig at 0.10
  stringsAsFactors = FALSE
)

# Load in some additional external data for visualizations creation
cvd <- read_excel(file.path(EXTERNAL_DATA_PATH, "c_v_from_web.xlsx"), col_names = F, sheet = 2)
wbc<-wb_countries()  
countries<-read_delim(file.path(EXTERNAL_DATA_PATH, "WID_countries.csv"),delim=";") %>% filter(!grepl("-",alpha2))






