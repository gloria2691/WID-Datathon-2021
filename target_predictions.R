# Title     : Women in data Science Datathon 2021
# Objective : Classification Problem, predict diabetes yes/no
# Team      : SuperSweet
# Related rscripts: data_processing.R, target_prediction.R, functions.R
# Created on: 2/18/2021, by Manuela Runge
###----------------------------------------

###----------------------------
### SETTINGS
###----------------------------
### Load packages
library(tidyverse) # data handling using pipes
library(data.table) # faster reading of csv files
library(caret)   # machin learning package
library(caTools) # confusion matrix table
library(ranger) # faster implementation of randomForest

### Custom settings
source(file.path("functions.R"))

# Define directories
getwd()
data_dir <- list.files(getwd(), "download_dat")
list.files(data_dir)

###------------------------------------------------
### Load data
###------------------------------------------------
train_df <- fread(file.path(data_dir, "TrainingWiDS2021_cleaned.csv"))
train_df$diabetes_mellitus <- factor(train_df$diabetes_mellitus,
                                     levels=c('nodiabetes','diabetes'),
                                     labels=c('nodiabetes','diabetes'))
table(train_df$diabetes_mellitus)

### Codebook
codebook <- fread(file.path(data_dir, "DataDictionaryWiDS2021.csv"))
colnames(codebook) <- gsub(" ", "_", tolower(colnames(codebook)))
cols_not_exist <- which(!((unique(codebook$variable_name)) %in% (colnames(train_df))))
(vars_not_exist <- unique(codebook$variable_name)[cols_not_exist])
codebook <- codebook %>% filter(variable_name != vars_not_exist)
cols_cat <- f_cols_by_cat()
demographic <- c("age","bmi","gender","ethnicity")
comorbidity <- cols_cat$apachecomorbidity

###------------------------------------------------
### Different options for pre-selecting features
###------------------------------------------------
cols_highvariance <- fread(file.path(data_dir,"cols_highvariance.csv"))
cols_highvariance <- cols_highvariance$cols_highvariance

cols_highassociation <- c("age", "bmi", "height", "weight","glucose_apache", "d1_glucose_max",
                          "h1_glucose_max","d1_glucose_min", "h1_glucose_min", "d1_creatinine_min",
                          "d1_creatinine_max")

selected_features_A <-  c("d1_glucose_max")
selected_features_B <- c(demographic, comorbidity)
selected_features_C <- cols_highvariance
selected_features_D <- c(selected_features_B, cols_highvariance)
selected_features_E <-  c(cols_highvariance,cols_highassociation)
selected_features_F = cols_highassociation

### Select features --------------------------
selected_predictors <- selected_features_F  ### Feature selection
selected_predictors <- unique(selected_predictors)
train_df <- as.data.frame(train_df)
train_df <- train_df[,(colnames(train_df) %in% c( "diabetes_mellitus",selected_predictors))]
dim(train_df)

###------------------------------------------------
### CARET preprocessing
###------------------------------------------------
### Select preprocessing steps and create preprocessing object to apply on train and test data
#c("zv", "medianImpute","center", "scale","pca") #c("zv", "medianImpute","center", "scale" , "pca") #"corr"
preprocessing=c("medianImpute")

preProc <- preProcess(train_df, method =preprocessing)
train_df_prep <- predict(preProc,train_df )
dim(train_df_prep)

### Split dataframe for easier processing in caret train function
diabetes_mellitus_x <- train_df_prep %>% select(-diabetes_mellitus)
diabetes_mellitus_y <- train_df_prep %>% select(diabetes_mellitus)
diabetes_mellitus_y <- diabetes_mellitus_y$diabetes_mellitus

###----------------------------
### CARET settings
###----------------------------
### Get information about available methods
names(getModelInfo())
### Get information about preprocessing options
?preProcess

# Create custom indices: myFolds
myFolds <- createFolds(diabetes_mellitus_y, k = 5)
# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

###----------------------------
### TRAIN MODELS
### Note random Forest takes very long
### --------------------
TEST=FALSE
methods = c("glm","ranger","glmnet")
if(TEST)methods="glm"

model_list <- list()
for( method in methods){
#method=methods[1]

  model <- train(
    x = diabetes_mellitus_x,
    y = diabetes_mellitus_y,
    metric = "ROC",
    method = method,
    trControl = myControl
)
  model_list[[method]] <- model
}

### Investigate specific model
temp_model <- model_list$glm
p = predict(temp_model, train_df_prep,type = "prob")
dim(p)
p_class <- ifelse(p[,2] > 0.5, "diabetes", "nodiabetes")
p_class <- factor(p_class, levels=c( "nodiabetes","diabetes"),
                  labels=c("nodiabetes","diabetes"))
confusionMatrix(p_class, train_df_prep$diabetes_mellitus)
colAUC(p, train_df_prep$diabetes_mellitus, plotROC = TRUE)

## Investigate caret objects
names(temp_model)
dim(diabetes_mellitus_x)
dim(temp_model$trainingData)

#### Compare models and summarize/visualize results
if(length(model_list)> 1){
  resamp = resamples(model_list)
  # Summarize the results
  summary(resamp)
  dotplot(resamp, metric="ROC")
  xyplot(resamp, metric="ROC" )
}

###----------------------------
### MAKE PREDICTIONS on unlabelled data
###----------------------------

## Load and clean test data
test_df <- fread(file.path(data_dir, "UnlabeledWiDS2021.csv")) %>%
  as.data.frame() %>%
  arrange(encounter_id) %>%
  select(selected_predictors)
dim(test_df)

### Apply same preprocessing as for train data!
str(test_df)
test_dat_prep <- predict(preProc, test_df)
str(test_dat_prep)

## Make predictions and save csv
submit_df <- f_predict_and_save_submission_csv(test_dat=test_dat_prep,
                                               model_list=model_list,
                                               final_method="glm")

### Look at predictions
summary(submit_df$diabetes_mellitus)
p_class_test <- ifelse(submit_df$diabetes_mellitus > 0.5, "diabetes", "nodiabetes")
table(p_class_test)
(length(p_class_test[p_class_test=="diabetes"] )/ length(p_class_test))*100