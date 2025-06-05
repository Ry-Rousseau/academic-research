
setwd("G:/.shortcut-targets-by-id/1bpC_DXkiDUELjDlWIZvfQ4BgD0zURCNn/Banking Crises - Non Democracies JC/Banking Crises - Non Democracies/Replication Files")

# This needs to be specified before running script
# Possibly build in a choice between manually generating imputed dfs or using ones already created
# by default we shouldn't run the multiple imputation
PYTHON_EXE_PATH <- "C:/Users/User/Documents/r-midastf/Scripts/python.exe"
PYTHON_EXE_PATH <- "C:/Users/ryanh/anaconda3/python.exe"
PYTHON_EXE_PATH <- "C:/Users/rhrou/AppData/Local/Programs/Python/Python313/python.exe"



Sys.setenv(RETICULATE_PYTHON = PYTHON_EXE_PATH)

# Consider adding a few lines which just create the necessary folders if they don't already exist

source("setup.R")

# Secondly set the directories for data, scripts, tables and figures
#Please update your username ("YOURUSERNAME") and paths ("YOURPATH") to the location of replication data and code. 

# if(
#   Sys.info()["user"]=="YOURUSERNAME"){pathData="YOURPATH";pathR="YOURPATH";pathFig="YOURPATH";pathTab="YOURPATH"}

#Example:

#if(
#Sys.info()["user"]=="nils"){pathData="~/Documents/git/peaceKeepingLight/BJPS_replication";pathR="~/Documents/git/peaceKeepingLight/BJPS_replication";pathFig="~/Desktop/figures";pathTab="~/Desktop/tables"}


# 1) Multiple Imputation Dataset Generation

source("MI_01_data_setup.R")

source("MI_02_hyperparameter_optimization.R") # - this one needs some work, needs to show that hyperparameter optimimzation is successful
# - specifically, we run up until the convert function and then hit an error: non-numeric argument to binary operator

source("MI_03_create_imputed_dfs.R") # you need to run this and see that it writes everything correctly

# Figure out what is up with the second part of the 4_run_all_imputed_models.R

# Add environment cleaning so that we don't overuse our memory

# 2) Manuscript Replication

# Figure 1, 2 and 3
source("fig_1_2_3.R")

# Figure 4 and 5
source("figure_4_5.R")

# Table 2 (Table 1 is qualitative descriptions)
source("table_2.R")

# Figure 6
source("fig_6.R")

# 3) Online Appendix Replication

# Figures A1, A2, A3, A4 
source("fig_A1_A2_A3_A4.R")

# Tables A5 and A6 (Tables A.1-A.4 are qualitative descriptions)
source("table_a5.R")

source("table_a6.R")

# Figure A5
source("fig_A5.R")

# Figure A6, A7, A8, (MIDAS Imputation Diagnostics)

# source("fig_A6_A7_A8.R")

# Table A7 (Middle Class Interests) 
source("table_a7.R")

# Table A8 (Financialization )

# NOTE THIS ONE NEEDS TO BE FIXED - FIGURE OUT THE FORMULA FOR THE MODEL UNDERLYING A8
# source(table_a8.R)

# Table A9 (Autocratic Regime Characteristics 1)
source("table_a9.R")

# Table A10 (Autocratic Regime Characteristics 2)
source("table_a10.R")

# Table A11 (Autocratic Regime Characteristics with Interaction 1)
source("table_a11.R")

# Table A12 (Autocratic Regime Characteristics with Interaction 2)
source("table_a12.R")

# Table A13 (Polity and V-Dem Samples)
source("table_a13.R")

# Table A14 (IMF Program and Capital Account Openness (CAO))
source("table_a14.R")

# Table A15 (Investment Competition and Learning Effects)
source("table_a15.R")

# Table A16 and A17 (Bailout Index on Annual Protest Counts DCPF/ No DCPF)
source("table_a16_a17.R")






