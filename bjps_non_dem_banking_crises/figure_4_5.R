# Figure 4

# Policy Index over Time Graph ####
#Normalised
data_nobmrna<-data %>% 
  mutate(`DC/PF` = factor(ifelse(DC_Mand_Revise2 == 1, "Yes","No"),levels=c("Yes","No"))) %>% 
  filter(bmr_democracy==1)

#binned
data_nobmrna <- data_nobmrna %>%
  mutate(year_group = floor((year - min(year))/5) * 5 + min(year)+5) # Adjusts based on the min year

data_summary <- data_nobmrna %>%
  group_by(year_group, `DC/PF`) %>%
  filter(!is.na(index_canon_no)) %>% 
  summarise(Norm_Index_canon_avg = mean(index_canon_no, na.rm = TRUE),  # Replace mean with your choice of summary statistic
            count = n()) %>%
  ungroup()

# Create breaks every 20 years from the start to the end of your dataset
year_breaks <- seq(from = 1800, to = 2020, by = 20)
year_breaks_1980 <- seq(from = 1980, to = 2020, by = 5)

# Step 4: Plot
ggplot(data_summary, aes(x = as.numeric(year_group), y = Norm_Index_canon_avg, color = `DC/PF`,size = count)) +
  geom_point(alpha = 0.7) +  # Adjust alpha for point transparency if desired
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  labs(y = "Bailout Policy Index", x = "Year", size = "Number of crises") +
  scale_size_continuous(range = c(1, 10))+ # Adjust the range of point sizes as needed
  scale_x_continuous(breaks = c(year_breaks))  +# Apply the breaks
  guides(color = guide_legend(order = 1), size = guide_legend(order = 2))

ggsave(paste0(FIGURE_PATH, "/policy_index_over_time_binned_size_nondems_dcpf.png"), width = 9, height = 6)

cat(paste("Written figure 4 to", FIGURE_PATH))


# FIGURE 5

icn_by_dcm <- rbind(
  # Summary for Post-1945
  data_cc %>%
    filter(year > 1945) %>%
    group_by(DC_Mand_Revise2) %>%
    summarise(
      mean = mean(index_canon_no, na.rm = TRUE),
      lower_ci = mean - qt(0.975, df = n() - 1) * sd(index_canon_no, na.rm = TRUE) / sqrt(n()),
      upper_ci = mean + qt(0.975, df = n() - 1) * sd(index_canon_no, na.rm = TRUE) / sqrt(n()),
      n = n()
    ) %>%
    mutate(period = "Post-1945"),
  
  # Summary for All Years
  data_cc %>%
    group_by(DC_Mand_Revise2) %>%
    summarise(
      mean = mean(index_canon_no, na.rm = TRUE),
      lower_ci = mean - qt(0.975, df = n() - 1) * sd(index_canon_no, na.rm = TRUE) / sqrt(n()),
      upper_ci = mean + qt(0.975, df = n() - 1) * sd(index_canon_no, na.rm = TRUE) / sqrt(n()),
      n = n()
    ) %>%
    mutate(period = "All Years")
) %>%
  # Create DC/PF label
  mutate(`DC/PF` = ifelse(DC_Mand_Revise2 == 0, "No DC/PF", "DC/PF")) %>%
  
  # Exclude "DC/PF" in "All Years"
  filter(!( `DC/PF` == "DC/PF" & period == "All Years")) %>%
  
  # Create a combined grouping variable for plotting
  mutate(group = case_when(
    `DC/PF` == "DC/PF" & period == "Post-1945"      ~ "DC/PF (Post-1945)",
    `DC/PF` == "No DC/PF" & period == "All Years"   ~ "No DC/PF (All Years)",
    `DC/PF` == "No DC/PF" & period == "Post-1945"   ~ "No DC/PF (Post-1945)"
  )) %>%
  
  # Set the order of the groups
  mutate(group = factor(group, levels = c("DC/PF (Post-1945)", "No DC/PF (All Years)", "No DC/PF (Post-1945)")))

# Create the bar graph
ggplot(icn_by_dcm, aes(x = group, y = mean, fill = `DC/PF`)) +
  geom_col(width = 0.8) +  # Adjust width as needed
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(0.6)) +
  ylim(-1, 2) +
  labs(
    y = 'Mean Bailout Policy Index',
    x = ''
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)
  ) +
  scale_fill_manual(values = c("DC/PF" = "#00CED1", "No DC/PF" = "#F08080"))

ggsave(paste0(FIGURE_PATH,"/index_canon_no_by_dcm2_1945.png"), width = 6, height = 6)

cat(paste("Written figure 5 to", FIGURE_PATH))








