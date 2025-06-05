# Figures A1,A2,A3,A4

# ==== Figure A1: Formal government commitments to financial stabilization among non-democracies ====

# Prepare data for Figure A1
dat_figure_a1 <- data %>%
  group_by(year) %>%
  summarize(
    number_non_democracies = sum(non_democracy_bmr == 1, na.rm = TRUE),
    number_non_democracies_commitment = sum(non_democracy_bmr == 1 & cr2_Lorenzo == 1, na.rm = TRUE)
  ) %>%
  ungroup()

# Create Figure A1
plot_a1 <- ggplot(data = dat_figure_a1, aes(x = year)) +
  geom_bar(aes(y = number_non_democracies_commitment, 
               fill = "Number of Non-Democracies with Policy Commitment"), 
           stat = "identity") +
  geom_line(aes(y = number_non_democracies, 
                color = "Number of Non-Democracies")) +
  ggtitle("Formal government commitments to financial stabilization among non-democracies since 1800") +
  xlab("Year") + 
  ylab("") + 
  scale_x_continuous(breaks = seq(1800, 2020, by = 20)) +
  scale_fill_manual(name = NULL, 
                    values = c("Number of Non-Democracies with Policy Commitment" = "grey57")) +
  scale_color_manual(name = NULL, 
                     values = c("Number of Non-Democracies" = "black")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 15),
        text = element_text(size = 20))

# Save Figure A1
ggsave(plot = plot_a1, 
       filename = file.path(FIGURE_PATH, "fig_A1.png"), 
       height = 10, width = 20)

# ==== Figure A2: Accumulated years of effective policy commitment by crisis-year ====

# Define crisis codes to include in the figure
selected_crisis_codes <- c(
  # Top 5 that experience at least a canonical crisis
  "CN-1992", "HT-1994", "ROM-1990", "ETH-1994", "PG-1995",
  # Bottom 5 that experience at least a canonical crisis
  "PR-1931", "JP-1927", "IT-1930", "FRA-1864", "BRL-1914", 
  # P10, p25, p50, p75, and p90 from pre-1939
  "UG-1898", "UK-1878", "TK-1931", "SWE-1907", "ES-1924",
  # P10, p25, p50, p75, and p90 from post-1970
  "SAF-1989", "DRC-1991", "KZ-2008", "ALG-1990", "VT-1997"
)

# Prepare data for Figure A2
dat_figure_a2 <- data %>%
  filter(non_democracy_bmr == 1, canonical_crisis == 1, crisis_code %in% selected_crisis_codes) %>%
  dplyr::select(crisis_code, country, year, Canonical_Commit_Years) %>%
  mutate(crisis_year_country = paste(country, year, sep = " ")) %>%
  dplyr::select(crisis_code, crisis_year_country, Canonical_Commit_Years) %>%
  # Add median value as a reference point
  rbind(data.frame(
    crisis_code = NA, 
    crisis_year_country = "Post-Commitment Median",
    Canonical_Commit_Years = 12
  )) %>%
  arrange(Canonical_Commit_Years) %>%
  mutate(crisis_year_country = factor(crisis_year_country, levels = unique(crisis_year_country)))

# Create Figure A2
plot_a2 <- ggplot(data = dat_figure_a2, 
                  aes(x = crisis_year_country, 
                      y = Canonical_Commit_Years, 
                      fill = factor(ifelse(crisis_year_country == "Post-Commitment Median", 
                                           "Highlighted", "Normal")))) +
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("red", "black")) +
  ggtitle("Accumulated years of effective policy commitment by canonical crisis-year, by country (BMR non-democracies)") +
  xlab("") + 
  ylab("Accumulated Years of Effective Policy Commitment") +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(),
        plot.caption = element_text(size = 11, face = "italic", hjust = 0))

# Save Figure A2
ggsave(plot = plot_a2,
       filename = file.path(FIGURE_PATH, "fig_A2.png"), 
       height = 10, width = 15)
# A3 - Proportion of crises by autocratic regime type

# Define the variables for different regime types
regime_vars <- c("af_party", "af_personal", "af_monarch", "af_military", 
                 "v2x_regime_closed", "v2x_regime_electoral")

# Define the readable labels for each variable
regime_labels <- c("Party-based", "Personalist", "Monarchy", "Military", 
                   "Closed Autocracy", "Electoral Autocracy")

# Create a data frame to store the proportions
data_prop_vars <- data.frame(
  Variable = regime_vars,
  Variable_label = regime_labels,
  Prop_Crises = numeric(length(regime_vars)),
  stringsAsFactors = FALSE
)

# Calculate the total number of crisis years in BMR sample
total_crises <- nrow(bmr_data)

# Calculate proportions for each regime type
for (i in seq_along(regime_vars)) {
  var_name <- regime_vars[i]
  
  # Handle v2x variables differently since they might have different format
  if (grepl("v2x_regime", var_name)) {
    prop <- bmr_data %>%
      filter(!is.na(!!sym(var_name)) & !!sym(var_name) == 1) %>%
      summarize(prop = n() / total_crises * 100) %>%
      pull(prop)
  } else {
    prop <- bmr_data %>%
      filter(!is.na(!!sym(var_name)) & !!sym(var_name) == 1) %>%
      summarize(prop = n() / total_crises * 100) %>%
      pull(prop)
  }
  
  data_prop_vars$Prop_Crises[i] <- prop
}

# Print the calculated values to verify
print(data_prop_vars)

# Create factor variables with proper ordering
data_prop_vars <- data_prop_vars %>%
  mutate(Variable_label = factor(
    Variable_label,
    levels = c("Party-based", "Personalist", "Monarchy", "Military", 
               "Electoral Autocracy", "Closed Autocracy")
  ))

# Subset the data for 'af_' measures and 'v2x' measures separately
af_data <- data_prop_vars %>%
  filter(Variable %in% c("af_party", "af_personal", "af_monarch", "af_military"))

v2x_data <- data_prop_vars %>%
  filter(Variable %in% c("v2x_regime_electoral", "v2x_regime_closed"))

# Create the first plot: 'af_' variables
af_plot <- ggplot(af_data, aes(x = Variable_label, y = Prop_Crises, fill = Variable_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(y = "Proportion of Crises") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_blank()
  )

# Create the second plot: 'v2x' variables
v2x_plot <- ggplot(v2x_data, aes(x = Variable_label, y = Prop_Crises, fill = Variable_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_blank()
  )

# Combine the plots
combined_plot <- gridExtra::grid.arrange(
  af_plot, v2x_plot, 
  ncol = 2
)

# Save the plot
ggsave(
  file.path(FIGURE_PATH, "fig_A3.png"),
  combined_plot,
  width = 10, 
  height = 6, 
  dpi = 300
)

cat("Figure A.3 saved to figure_path")

# A4 - Proportion of major occupational and social groups covered by 
# pension schemes by autocratic regime type.

# Define the variables for different regime types
regime_vars <- c("af_party", "af_personal", "af_monarch", "af_military", 
                 "v2x_regime_closed", "v2x_regime_electoral")

# Define the readable labels for each variable
regime_labels <- c("Party-based", "Personalist", "Monarchy", "Military", 
                   "Closed Autocracy", "Electoral Autocracy")

# Create a data frame to store the mean coverage values
pension_coverage_data <- data.frame(
  Variable = regime_vars,
  Variable_label = regime_labels,
  Mean_Coverage = numeric(length(regime_vars)),
  stringsAsFactors = FALSE
)

# Filter the base dataset to focus on autocratic regimes with pension data
autocratic_pension_data <- data %>% 
  filter(bmr_democracy == 0) %>% 
  filter(!is.na(private_oldageprog) & !is.na(horiz_cov_kr)) %>%
  filter(private_oldageprog == 1)

# Calculate mean horizontal pension coverage for each regime type
for (i in seq_along(regime_vars)) {
  var_name <- regime_vars[i]
  
  # Calculate mean coverage for the current regime type
  coverage <- autocratic_pension_data %>%
    filter(!is.na(!!sym(var_name)) & !!sym(var_name) == 1) %>%
    summarize(mean_cov = mean(horiz_cov_kr, na.rm = TRUE)*100) %>%
    pull(mean_cov)
  
  pension_coverage_data$Mean_Coverage[i] <- coverage
}

# Print the calculated values to verify
print(pension_coverage_data)

# Create factor variables with proper ordering
pension_coverage_data <- pension_coverage_data %>%
  mutate(Variable_label = factor(
    Variable_label,
    levels = c("Party-based", "Personalist", "Monarchy", "Military", 
               "Electoral Autocracy", "Closed Autocracy")
  ))

# Subset the data for 'af_' measures and 'v2x' measures separately
af_pension_data <- pension_coverage_data %>%
  filter(Variable %in% c("af_party", "af_personal", "af_monarch", "af_military"))

v2x_pension_data <- pension_coverage_data %>%
  filter(Variable %in% c("v2x_regime_electoral", "v2x_regime_closed"))

# Create the first plot: 'af_' variables
af_pension_plot <- ggplot(af_pension_data, aes(x = Variable_label, y = Mean_Coverage, fill = Variable_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(y = "Proportion of major occupational and social groups covered") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_blank()
  )

# Create the second plot: 'v2x' variables
v2x_pension_plot <- ggplot(v2x_pension_data, aes(x = Variable_label, y = Mean_Coverage, fill = Variable_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_blank()
  )

# Combine the plots
combined_pension_plot <- gridExtra::grid.arrange(
  af_pension_plot, v2x_pension_plot, 
  ncol = 2
)

# Save the plot without a title
ggsave(
  file.path(FIGURE_PATH, "fig_A4.png"),
  combined_pension_plot,
  width = 10, 
  height = 6, 
  dpi = 300
)

cat("Figure A.4 saved to figure_path")

