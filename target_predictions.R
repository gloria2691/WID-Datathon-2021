# Title     : Women in data Science Datathon 2021
# Objective : Classification Problem, predict diabetes yes/no
# Team      : <team_name>
# Related rscripts: data_processing.R, target_prediction.R, functions.R
# Created on: 2/18/2021
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
VIEW = FALSE  #TRUE
source(file.path("functions.R"))

# Define directories
getwd()
data_dir <- list.files(getwd(), "download_dat")
list.files(data_dir)

### Load data
train_df <- fread(file.path(data_dir, "TrainingWiDS2021_cleaned.csv"))
train_df <- f_variable_cleanup(train_df)
train_df$diabetes_mellitus <- factor(train_df$diabetes_mellitus, levels=c('nodiabetes','diabetes'), labels=c('nodiabetes','diabetes'))
target_var = 'diabetes_mellitus'

###----------------------------
###  PREPROCESSING
###----------------------------
preselect_predictors=TRUE
if(preprocess){
  codebook <- fread(file.path(data_dir, "DataDictionaryWiDS2021.csv"))
  colnames(codebook) <- gsub(" ", "_", tolower(colnames(codebook)))
  cols_not_exist <- which(!((unique(codebook$variable_name)) %in% (colnames(train_df))))
  (vars_not_exist <- unique(codebook$variable_name)[cols_not_exist])
  codebook <- codebook %>% filter(variable_name != vars_not_exist)
  cols_cat <- f_cols_by_cat(codebook)
  demographic = c("age","bmi","gender","ethnicity")
  comorbidity = cols_cat$apachecomorbidity
  #lab = c()

  selected_predictors = c(demographic, comorbidity) # cols_intstr$numeric
  selected_predictors = c(demographic, cols_highvariance)
  train_df <- as.data.frame(train_df)
  #train_df = train_df %>% select_at(c(selected_predictors, target_var)) # %>% na.omit()
  train_df <- train_df[,colnames(train_df) %in% c(selected_predictors,target_var)]
  dim(train_df)
}

dim(train_df)
### CARET preprocessing
### Select preprocessing steps and create preprocessing object to apply on train and test data
preprocessing=c("zv", "medianImpute","center", "scale" , "corr") #c("zv", "medianImpute","center", "scale" , "pca")
preProc <- preProcess(train_df, method =preprocessing)
train_df_prep <- predict(preProc,train_df )

diabetes_mellitus_x <- train_df_prep %>% select(-diabetes_mellitus)
diabetes_mellitus_y <- train_df_prep %>% select(diabetes_mellitus)
diabetes_mellitus_y <- diabetes_mellitus_y$diabetes_mellitus

###----------------------------
### CARET SETTINGS
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
TEST=TRUE

methods = c("glm","glmnet","ranger")
if(TEST)methods =methods[3]

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
  model
  #plot(model)
  model_list[[method]] <- model
  p = predict(model, train_df_prep,type = "prob")
  dim(p)
  p_class <- ifelse(p[,2] > 0.5, "diabetes", "nodiabetes")
  p_class <- factor(p_class, levels=c( "nodiabetes","diabetes"),
                    labels=c("nodiabetes","diabetes"))
  confusionMatrix(p_class, train_df_prep$diabetes_mellitus)

  ## Investigate caret objects
  names(model)
  dim(diabetes_mellitus_x)
  dim(model$trainingData)
}

#### Compare models and summarize/visualize results
# Pass model_list to resamples(): resamples
resamp = resamples(model_list)
# Summarize the results
summary(resamp)
dotplot(resamp, metric="ROC")
xyplot(resamp, metric="ROC" )

###----------------------------
### MAKE PREDICTIONS
###----------------------------
final_model = model_list['ranger'] ### Select best model
test_df <- fread(file.path(data_dir, "UnlabeledWiDS2021.csv"))
test_df <- f_variable_cleanup(test_df)
test_df <- as.data.frame(test_df)
test_df <- test_df[,colnames(test_df) %in% c(selected_predictors)]
dim(test_df)
## Load and clean test data
#test_dat = test_df %>% select_at(selected_predictors)

### Apply same preprocessing as for train data
str(test_dat)
test_dat_prep <- predict(preProc, test_df)
str(test_dat_prep)

## Make predictions and save csv
submit_df <- f_predict_and_save_submission_csv(test_dat=test_dat_prep, final_model=final_model)
summary(submit_df$diabetes_mellitus)

### Compare prevalence to train predictions
p_class_test <- ifelse(submit_df$diabetes_mellitus > 0.5, "diabetes", "nodiabetes")
p_class_test <- factor(p_class_test, levels=c( "nodiabetes","diabetes"), labels=c("nodiabetes","diabetes"))
table(p_class_test)

table(train_df_prep$diabetes_mellitus)
(length(p_class_test[p_class_test=="diabetes"] )/ length(p_class_test))*100
(length(p_class[p_class=="diabetes"] )/ length(p_class))*100