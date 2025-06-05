# Figure A6, A7, A8, (MIDAS Imputation Diagnostics)

# To DO - figure out how to replicate the csv files created in the 2_imputation_testing_2.R script

# Figure A6 is basically derived from hyperparameters testing summary csv, getting the average of the seeds


# Honestly, no idea how this figure was created, I'm at a bit of a loss

# Figure A7

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


# Figure A8 - Variation of the DC MAND REVISE 2 Coefficient on the chosen seed

# A8 is basically a filtered version of the seed_check.csv for 
# Variable == DC_Mand_Revise2 and 
# Varying_Controls = None OR FEs + debt + partisan_cons

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