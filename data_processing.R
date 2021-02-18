# Title     : Women in data Science Datathon 2021
# Objective : Classification Problem, predict diabetes yes/no
# Team      : <team_name>
# Related rscripts: data_processing.R, target_prediction.R, functions.R
# Created on: 2/18/2021
###----------------------------------------

### Load packages
require(tidyverse)
require(data.table)

### Custom settings
VIEW = FALSE  #TRUE
source(file.path("functions.R"))

# Define directories
getwd()
data_dir <- list.files(getwd(), "download_dat")
list.files(data_dir)

### Load data
codebook <- fread(file.path(data_dir, "DataDictionaryWiDS2021.csv"))
colnames(codebook) <- gsub(" ", "_", tolower(colnames(codebook)))
train_df <- fread(file.path(data_dir, "TrainingWiDS2021.csv"))
if(VIEW)str(train_df)

### Check variables in data and dictionary
cols_not_exist <- which(!((unique(codebook$variable_name)) %in% (colnames(train_df))))
(vars_not_exist <- unique(codebook$variable_name)[cols_not_exist])
codebook <- codebook %>% filter(variable_name != vars_not_exist)
if(VIEW)str(codebook)

### Group variables by category
cols_cat <- f_cols_by_cat(codebook)
if(VIEW)print(cols_cat)

### Group variables by type
cols_intstr <- f_get_cols_strint(train_df)
if(VIEW)print(cols_intstr)

### Explore by variable type
#length(cols_intstr$numeric)
p_hist_by_target(dat=train_df, selected_cols=cols_intstr$numeric[1:10],target_var = 'diabetes_mellitus')
p_hist_by_target(dat=train_df, selected_cols=cols_intstr$numeric[10:20],target_var = 'diabetes_mellitus')
p_bar_by_target(dat=train_df,selected_cols=cols_intstr$character,target_var = 'diabetes_mellitus')

### Explore by variable category
#length(cols_cat$apachecomorbidity)
p_hist_by_target(dat=train_df, selected_cols=cols_cat$labsbloodgas,target_var = 'diabetes_mellitus')
p_bar_by_target(dat=train_df,selected_cols=cols_cat$demographic,target_var = 'diabetes_mellitus')

