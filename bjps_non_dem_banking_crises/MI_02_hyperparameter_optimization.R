# IMPUTATION TESTING ####
# the below is to identify the best hyperparameters

## 1) Firstly clean the data for imputation testing

#list binary variables for midas
#to view binary variables-- we can view all where the unique() is <=3 -- a lot of these will be binary, some 0 1 2. 
binvar_list<-lapply(data_for_imputation,unique)[lapply(lapply(data_for_imputation,unique),function(x)length(x)<=3)==T]

#figure out other var types
cat_test_data<-data_for_imputation %>% dplyr::select(-all_of(binvar_list %>% names))

#we are going to first do all non-numeric, then test where there are less than 20 values and add dplyr::select
catvar_list<-lapply(cat_test_data,unique)[lapply(lapply(cat_test_data,unique),function(x)is.numeric(x))==F]

#where less that 20 unique values (assuming no cat var has >20 categories)
less_20<-lapply(cat_test_data,unique)[lapply(lapply(cat_test_data,unique),function(x)length(x)<=21)==T]

less_20[!(less_20 %in% catvar_list)]

#add variables that werent in original list
catvar_list<- c(names(catvar_list), "e_regiongeo","v2regimpgroup","e_regionpol","partisan_cons")

#clean
catvars <- catvar_list
binvars <- names(binvar_list) %>% unique

# 2) Prepare rmidas preprocessing

# rmidas preprocessing
data_conv <- convert(data_for_imputation, 
                     bin_cols = binvars, 
                     cat_cols = catvars,
                     minmax_scale = TRUE)

# Train rMIDAS model(s) ####

# Set a grid of hyparameters to test
nodes <- c(64, 128, 256, 512)
layers <- c(2, 3, 4)
node_layers <- list()
for (n in nodes) {
  for (l in layers) {
    node_layers[[length(node_layers) + 1]] <- rep(n, l)
  }
}

# run overimpute ####
hyper_res <- py_capture_output(
  for (nl in node_layers) {
    print("Node structure: ")
    print(nl)
    overimpute(data_conv, layer_structure = nl, vae_layer = FALSE,
               seed = 89, input_drop = 0.75, spikein = 0.3,
               training_epochs = 50, report_ival = 10, plot_vars = FALSE,skip_plot = F, spike_seed = 89,
               save_path = MIDAS_PATH)},
  type = c("stdout"))

fileConn<-file(paste0(MIDAS_PATH,"output.txt"))
writeLines(hyper_res, fileConn)
close(fileConn)

## read and interpret overimpute results ####
lines <- readLines("midas_impute_tables/calibration_figures/spec4/output.txt")

# Function to extract values for a given pattern
extract_values <- function(pattern) {
  matched_lines <- grep(pattern, lines, value = TRUE)
  as.numeric(str_extract(matched_lines, "\\d+\\.\\d+"))
}

# Extract metrics
individual_rmse_spike_in <- extract_values("Individual RMSE on spike-in:")
aggregated_rmse_spike_in <- extract_values("Aggregated RMSE on spike-in:")
individual_error_softmax_spike_in <- extract_values("Individual error on softmax spike-in:")
aggregated_error_softmax_spike_in <- extract_values("Aggregated error on softmax spike-in:")
individual_error_binary_spike_in <- extract_values("Individual error on binary spike-in:")
aggregated_error_binary_spike_in <- extract_values("Aggregated error on binary spike-in:")

#make df 
hyperp_data <- data.frame(
  epochs = rep(c(0, 10,20,30,40,50), length(node_layers)),
  layers = rep(layers, each = 6),
  nodes = rep(nodes, each = 6 * length(layers)),
  indiv_rmse = extract_values("Individual RMSE on spike-in:"),
  agg_rmse = extract_values("Aggregated RMSE on spike-in:"),
  indiv_sm = extract_values("Individual error on softmax spike-in:"), 
  agg_sm = extract_values("Aggregated error on softmax spike-in:"),
  indiv_bin = extract_values("Individual error on binary spike-in:"),
  agg_bin = extract_values("Aggregated error on binary spike-in:"))

write_csv(hyperp_data, "midas_impute_tables/calibration_figures/spec4/hyperparameters_testing.csv")

#make weights for each var according to number in data
sm_weight = length(catvar_list)/length(data_for_imputation)
bin_weight = length(binvar_list)/length(data_for_imputation)
con_weight = (length(data_for_imputation)-(length(catvar_list)+length(binvar_list)))/length(data_for_imputation)

#make summary df
hyperp_data_summary<-hyperp_data %>% 
  pivot_longer(cols = 4:9) %>% 
  mutate(weight = case_when(
    grepl("rmse",name) ~ con_weight,
    grepl("sm",name) ~ sm_weight,
    grepl("bin",name) ~ bin_weight
  )) %>% 
  group_by(epochs,layers,nodes) %>% 
  summarise(wm_error = weighted.mean(value,weight))

write_csv(hyperp_data_summary, "midas_impute_tables/calibration_figures/spec4/hyperparameters_testing_summary.csv")

#the lowest error rate model is 512 nodes 2 layers 50 epochs. 
# but we'll do early stopping, 20 epochs
#from Midas paper:
# "Error is
# minimized with a network comprising two hidden layers of 512 nodes and maximized with a
# network comprising four hidden layers of 64 nodes. These results indicate that the kind of
# complexity that characterizes the CCES is better captured by wider and shallower networks,
# perhaps because it is primarily manifested in interactions between variables rather than in
# nonlinearities."

#lets make a graph for it . also verifies my code. 
hyperp_data<-read_csv("midas_impute_tables/calibration_figures/spec4/hyperparameters_testing.csv")
gg_hyperp<-hyperp_data %>% 
  filter(layers == 2, nodes == 512) %>% 
  pivot_longer(c(4:9)) 

gg_hyperp$name<-gsub("sm","cat", gg_hyperp$name)

gg_hyperp$var <- lapply(strsplit(gg_hyperp$name,"_"),function(x)x[2]) %>% unlist
gg_hyperp$type <- lapply(strsplit(gg_hyperp$name,"_"),function(x)x[1]) %>% unlist

ggplot(gg_hyperp)+
  geom_line(aes(x = epochs, y = value, color = var, linetype = type, fill = name))


# How many M ####
#M = 1+ ½ (FMI / CV(se))2
# https://missingdata.org/
#https://statisticalhorizons.com/how-many-imputations/ josh ericksons package

#NB -- this ends up being less important as we jsut let M vary in our final analysis...

#we'll use the main regression model -- the DC_Mand_Revise etc
# data_train <- train(data_conv,
# training_epochs = 50,
# layer_structure = c(512,512),
# input_drop = 0.75, #low input drop to minimise OF risk
# seed = 99)

#m = 20 as in von hippel 2020
#data_imp_prefilter <- complete(data_train, m = 20, file = "midas_impute_tables/imputation_m_test")
# csv_files <- list.files(path = "midas_impute_tables/imputation_m_test", pattern = "\\.csv$", full.names = TRUE)
# data_imp_prefilter <- lapply(csv_files, function(x)fread(x,verbose=F))
# data_imp_pre_flip<-lapply(data_imp_prefilter, function(x) filter(x, keep_post_impute==0))
# 
# #reverse the binvars
# v <- lapply(data_imp_pre_flip, function(x) mutate(x, across(all_of(binvars), function(x)ifelse(x==0,1,0))))
# 
# mi_model_fit<- lapply(data_imp, function(x) lm_robust(index_canon_no ~  polity2 + e_gdppc + FixedER+bankloan_total_gdp,
#                                                       clusters = country,
#                                                       se_type = "stata",
#                                                       data = x))
# 
# how_many_imputations(mi_model_fit)
# 
# MIcombine(mi_model_fit)

#it varies a lot -- 2 for DC_mand basic model, 60+ for some.
#ok so I think we can do something where we generate 100 Ms or something then for each model use as many as we need

# Sensitivity to random seed ####
# we need to differentiate between SE and PE sensitivity...

#let's try 8 random seeds. we'll use a high M to reduce the sensitivity to SEs. will save them locally 
#use 10 epochs not 20 to speed it up (edit-- this was maybe a mistake?? maybe do 20 for the paper...)
path = "/Users/joehigton/mi_random_seeds"

for (i in 1:8){
  dir.create(paste0("/Users/joehigton/mi_random_seeds/seed",i))
  
  data_train <- train(data_conv,
                      training_epochs = 20,
                      layer_structure = c(512,512),
                      input_drop = 0.7, #low input drop to minimise OF risk
                      seed = i)
  
  print(paste("finished seed",i,"training"))
  
  data_imp_prefilter <- complete(data_train, m = 60, file = paste0("/Users/joehigton/mi_random_seeds/seed",i))
  
  print(paste("finished seed",i,"imputation"))
}

#we now have 8 sets of 60 imputed DFs 
#lets run models on them all and see how we get on
control_sets <- c("","+factor(period_dummies72)")
additional_controls <- c("", "+debt+partisan_cons")

variables_of_interest<-c("C_Prop_Index", "deposits_new", "DC_Mand_Revise", "C_prop_index_real_average",
                         "bankloan_mortgage_gdp","bankloan_household_gdp","Canonical_Commit_Years",
                         "af_party","af_personal","bankloan_firm_gdp","assets_JST_dba","bankloan_total_gdp")

all_formula_pairs <- setNames(lapply(variables_of_interest, generate_formulas, control_sets, additional_controls), 
                              variables_of_interest)

df_base<-data.frame()
for (i in 1:8){
  csv_files <- list.files(path = paste0("/Users/joehigton/mi_random_seeds/seed",i), pattern = "\\.csv$", full.names = TRUE)
  data_imp <- lapply(csv_files, function(x)fread(x,verbose=F))
  print(paste("finished loading csv files for seed",i))
  
  data_imp <- lapply(data_imp, function(x) mutate(x, across(all_of(binvars), function(x)ifelse(x==0,1,0))))
  data_imp <-lapply(data_imp, function(x) filter(x, keep_post_impute==1))
  
  print(paste("data has", nrow(data_imp[[1]]),"rows. should be 216."))
  
  print(paste("running models for seed",i))
  df <- run_models_variable_m(all_formula_pairs, data_imp) %>% mutate(seed = i)
  
  print(paste("finished running models for seed",i))
  df_base <- bind_rows(df, df_base)
}
write_csv(df_base,"midas_impute_tables/calibration_figures/spec4/seed_check.csv")

#we have a summary df across models lets manipulate it
df_piv<-df_base %>% dplyr::select(-Formula,-p_value) %>% 
  pivot_wider(names_from = "seed", values_from = c("Coefficient","is_significant"))

df_piv$coef_sd <- rowSds(as.matrix(df_piv[, 5:12]))
df_piv$p_prop <- rowMeans(df_piv[, 13:20], na.rm = TRUE)

# adding in prop NA of the main var
df_piv<-data_for_imputation %>% filter(keep_post_impute==1) %>% 
  dplyr::select(all_of(variables_of_interest)) %>% 
  summarise(across(everything(), function(x)mean(is.na(x)))) %>% 
  pivot_longer(cols=everything()) %>% 
  rename(prop_na = value) %>% 
  right_join(., df_piv, by = c("name" = "Variable"))

df_piv_fil<- df_piv%>% dplyr::select(1:5, coef_sd, p_prop,prop_na) %>% filter(grepl("No",Varying_Controls)) 

plot(df_piv_fil$prop_na,df_piv_fil$coef_sd) 
cor(df_piv_fil$prop_na,df_piv_fil$coef_sd)

write_csv(df_piv,"midas_impute_tables/calibration_figures/spec4/seed_check_summary.csv")

#ok. so i think basically. using the variable M will help us get less seed-based issues.
#since we adjust the efficiency we need etc. etc. 

# generally what we see is that higher prop NA means less consistent results. as we'd expect. 
# so this should be accounted for (in some sense) by letting M vary.

#ok to summarise
# we use 2 512 layers 20 epochs
# we let M vary -- running first on m = 20 then going with more/less as required 
# modify the how many imputatuions function so we choose the variable, rather than doing the default (highest FMI)