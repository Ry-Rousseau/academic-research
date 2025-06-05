## SCRIPT FOR MULTIPLE IMPUTATION 
# ASSUME THAT setup.R has already been run, so data is loaded as an object

## Figure variables to remove ####
# Define the key variables that are of interest for the analysis
variables_of_interest <- c(
  "C_Prop_Index", "deposits_new", "DC_Mand_Revise", "C_prop_index_real_average",
  "bankloan_mortgage_gdp", "bankloan_household_gdp", "assets_JST_prc",
  "af_party", "af_personal", "bankloan_firm_gdp", "assets_JST_dba", "bankloan_total_gdp",
  "private_oldageprog", "v2xnp_client", "DC_Mand_Revise2", "homeown_imp",
  "bankloan_credit_firm_gdp", "bankloan_credit_household_gdp", "bankloan_credit_total_gdp"
) # Variables from the models to be run

# Define additional variables to keep in the dataset
others_keep <- c(
  "v2regsupgroupssize", "v2regoppgroupssize",
  "v2regsupgroups_nr", "v2regoppgroups_nr",
  "partisan_cons", "debt", "FixedER", "polity2", "cr2",
  "Quinn", "imfprog", "kaopen", "lhc_MF_25_64", "urbanization", "gwf_party", "v2x_regime_electoral"
) # added lhc_MF_25_64 and urbanization and gwf_party 
# added v2x_regime_electoral (16/01/2025) - needed for additional robustness check

# Remove unhelpful columns based on guidelines from:
# https://stefvanbuuren.name/fimd/sec-toomany.html

# Calculate the proportion of missing values for each variable
miss_prop <- data.frame(val = colSums(is.na(data)) / nrow(data) * 100)
miss_prop <- miss_prop %>% arrange(-val)  # Arrange in descending order of missingness

# Define variables that should always be kept
keep <- c("year", "country", "income_WB_classification")

# Identify variables with 0% missing data (fully observed variables)
complete_vars <- rownames(miss_prop)[miss_prop$val == 0]

# Remove fully observed variables that are not essential (excluding 'keep' variables)
remove1 <- complete_vars[!(complete_vars %in% keep)]

# Add additional administrative variables to remove
remove1 <- c(remove1, "p4n", "cown")

# Create a new dataset by removing the identified variables
data_r1 <- data %>% dplyr::select(-all_of(remove1))

# Remove variables with low 'outflux' (less than 0.5)
# 'Outflux' measures how much a variable contributes to explaining missingness in other variables
data_flux <- flux(data_r1, names(data_r1))

# Identify variables with 'outflux' less than 0.5
remove2 <- row.names(data_flux)[data_flux$outflux < 0.5]

# Exclude variables that start with 'e_' (assuming they are important)
remove2 <- remove2[!startsWith(remove2, "e_")]

# Ensure we do not remove variables of interest or other variables to keep
remove2 <- remove2[!(remove2 %in% c(variables_of_interest, others_keep))]

# Remove these variables from the dataset
data_r2 <- data_r1 %>% dplyr::select(-all_of(remove2))

# Repeat the flux analysis to further refine the variable dplyr::selection
data_flux2 <- flux(data_r2, names(data_r2))
remove3 <- row.names(data_flux2)[data_flux2$outflux < 0.5]

# Again, exclude variables starting with 'e_' and variables to keep
remove3 <- remove3[!startsWith(remove3, "e_")]
remove3 <- remove3[!(remove3 %in% c(variables_of_interest, others_keep))]

# Remove these variables from the dataset
data_r3 <- data_r2 %>% dplyr::select(-all_of(remove3))

# Remove additional variables that are conditional on previously removed variables
# and variables that are redundant or not needed for analysis
remove4 <- c(
  names(data_r3)[grepl("v2regsup", names(data_r3))],  # Variables containing 'v2regsup'
  "AUT", "AUT2", "cr2_Lorenzo", "Leg_Election", "commitment", "non_democracy_bmr"
)

# Remove these variables from the dataset
data_r4 <- data_r3 %>% dplyr::select(-all_of(remove4))

# Run a preliminary mice (Multiple Imputation by Chained Equations) model with zero iterations
# This helps to detect issues such as multicollinearity before performing actual imputation
m1 <- mice(data_r4, maxit = 0)

# Check for any logged events (e.g., warnings about collinearity)
m1$loggedEvents

# Remove variables that cause problems (e.g., collinearity) as identified by mice
remove5 <- c("af_Non_Party", "e_total_resources_income_pc", "af_regimenarrowcat", "e_wb_pop")

# Remove these problematic variables from the dataset
data_for_imputation <- data_r4 %>% dplyr::select(-all_of(remove5))

# Next move onto MI_02_hyperparameter_optimization.R