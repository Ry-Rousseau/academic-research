# Filter data to BMR autocracies with non-missing index_canon_no
# This selects only the crisis years for BMR autocracies
data_for_summary <- data %>% 
  filter(bmr_democracy == 0, !is.na(index_canon_no))


# Define variables and their corresponding labels
variables <- c("Protest_Count_MM", "Protest_DRS_Count_CNTS", "inv_hrp_farris", "CL_cv")

labels <- c(
)

