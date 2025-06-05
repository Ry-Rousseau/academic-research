# table_A5.R - Generate Table A.5 from the manuscript
# Summary statistics of Bailout Policy Index models

# Define variables and their corresponding labels
variables <- c(
  "DC_Mand_Revise2", "C_Prop_Index", "deposits_new", 
  "bankloan_mortgage_gdp", "bankloan_household_gdp", "time_since_commitment",
  "v2xnp_client", "v2xnp_regcorr", "v2x_corr", "v2x_execorr", "v2x_pubcorr",
  "bankloan_firm_gdp", "assets_JST_dba", "bankloan_total_gdp", 
  "v2x_regime_electoral", "af_party",
  "polity2", "e_gdppc", "debt", "FixedER", "partisan_cons", "lhc_MF_25_64", 
  "urbanization", "imfprog", "kaopen", "Quinn", "all_crises_before_5_year", 
  "crises_last_5_years_5_year", "crises_last_10_years_5_year", 
  "peer_average_cat3", "peer_average_regional"
)

labels <- c(
  "Mandatory DC/PF", "Property Prices", "Deposits / GDP",
  "Mortgage Debt / GDP", "Household Debt / GDP", "Years Effective Commitment",
  "Clientilism Index", "Regime Corruption Index", "Political Corruption Index", 
  "Executive Corruption Index", "Public Sector Corruption Index",
  "Bank Loans to Firms / GDP", "Bank Assets / GDP", 
  "Banks Loans to Private Non-Financial Sector / GDP", "Electoral Autocracy", 
  "Party-based Regime", "Degree of Democracy", "Log GDP pc", "Public debt burden",
  "Fixed exchange rate", "Partisanship", "Tertiary Education", "Urbanization",
  "IMF Program", "Capital Account Openness (kaopen)", "Capital Account Openness (Quinn)",
  "Learning rate - 5 year horizon, all crises window", 
  "Learning rate - 5 year horizon, 5 year window",
  "Learning rate - 5 year horizon, 10 year window",
  "Investment Competition - Income Peer", "Investment Competition - Regional Peer"
)

# Filter data to BMR autocracies with non-missing index_canon_no
# This selects only the crisis years for BMR autocracies
data_for_summary <- data %>% 
  filter(bmr_democracy == 0)

# Generate table
table_A5 <- robust_summary_stats(data_for_summary, variables, labels)

# Rename columns
colnames(table_A5) <- c("Variable", "N", "Mean", "SD", "Min", "Max", "Median", "% Missing")

# Convert missing proportion to percentage
table_A5$`% Missing` <- table_A5$`% Missing` * 100

# Save the results to a CSV file
write.csv(table_A5, file.path(TABLE_PATH, "table_A5.csv"), row.names = FALSE)
