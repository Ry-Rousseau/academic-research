
#this script generates 100 imputed datasets with the hyperparameters from step 2

rMIDAS::set_python_env(PYTHON_EXE_PATH)

# Assume you have data_for_imputation loaded from the previous script

# prepare data for MIDAS ####

#list binary vriables for midas
#to view binary variables-- we can view all where the unique() is <=3 -- a lot of these will be binary, some 0 1 2. 
binvar_list<-lapply(data_for_imputation,unique)[lapply(lapply(data_for_imputation,unique),function(x)length(x)<=3)==T]

#figure out other var types
cat_test_data<-data_for_imputation %>% dplyr::select(-all_of(binvar_list %>% names))

#we are going to first do all non-numeric, then test where there are less than 20 values and add select
catvar_list<-lapply(cat_test_data,unique)[lapply(lapply(cat_test_data,unique),function(x)is.numeric(x))==F]

#where less that 20 unique values (assuming no cat var has >20 categories)
less_20<-lapply(cat_test_data,unique)[lapply(lapply(cat_test_data,unique),function(x)length(x)<=21)==T]

less_20[!(less_20 %in% catvar_list)]

#add variables that werent in original list
catvar_list<- c(names(catvar_list), "e_regiongeo","v2regimpgroup","e_regionpol")

#clean
catvars <- catvar_list
binvars <- names(binvar_list) %>% unique


# Some code to resolve some errors with python versions - made a virtual python environment in local directory
# library(reticulate)

# Specify the virtual environment
# use_virtualenv("C:/Users/User/Documents/r-midastf", required = TRUE)


# rmidas preprocessing
data_conv <- convert(data_for_imputation, 
                     bin_cols = binvars, 
                     cat_cols = catvars,
                     minmax_scale = TRUE)

# ensure tensorflow is installed
check_and_install_tensorflow()

# train data
data_train <- train(data_conv,
                    training_epochs = 20,
                    layer_structure = c(512,512),
                    input_drop = 0.75, #low input drop to minimise OF risk
                    seed = 89)

rm(list=c("binvar_list","cat_test_data","data_for_imputation","data_conv","less_20"))

data_imp <- rMIDAS::complete(data_train, m = 100)

#below line - midas does this weird thing where it flips 1s and 0s in binary variables. this sorts it out.
data_imp <- lapply(data_imp, function(x) mutate(x, across(all_of(binvars), function(x)ifelse(x==0,1,0))))

# We want to write to the IMPUTED_DATA_PATH
for (i in 1:100){
  write_csv(data_imp[[i]], paste0(IMPUTED_DATA_PATH, "/midas_data_",i,".csv"))
  print(paste0("written file ",i))
}


