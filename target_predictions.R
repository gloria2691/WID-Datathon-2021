# Title     : Women in data Science Datathon 2021
# Objective : Classification Problem, predict diabetes yes/no
# Team      : <team_name>
# Related rscripts: data_processing.R, target_prediction.R, functions.R
# Created on: 2/18/2021
###----------------------------------------

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
train_df$diabetes_mellitus <- factor(train_df$diabetes_mellitus, levels=c('nodiabetes','diabetes'), labels=c('nodiabetes','diabetes'))
target_var = 'diabetes_mellitus'

###------------------------- EXPLORATIVE ------------
### Select variables
preprocess=TRUE ## Preprocessing handled in train function using caret
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
  train_df = train_df %>% select_at(c(selected_predictors, target_var)) # %>% na.omit()
   }

dim(train_df)
diabetes_mellitus_x <- train_df %>% select(-diabetes_mellitus)
diabetes_mellitus_y <- train_df %>% select(diabetes_mellitus)
diabetes_mellitus_y <- diabetes_mellitus_y$diabetes_mellitus

### Caret package
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

### --------------------
###  GLM model
### --------------------
glm_model <- train(
  x = diabetes_mellitus_x,
  y = diabetes_mellitus_y,
  metric = "ROC",
  method = "glm",
  trControl =myControl,
  preProcess = c("zv","medianImpute")
)

glm_model
plot(glm_model)
p = predict(glm_model, train_df)
confusionMatrix(p, train_df$diabetes_mellitus)

### Fit baseline model
set.seed(342)
glmnet_model <- train(
  x = diabetes_mellitus_x,
  y = diabetes_mellitus_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl,
  preProcess = c("zv","knnImpute","pca") # "medianImpute"
)
glmnet_model
plot(glmnet_model)
plot(glmnet_model$finalModel)

### Fit randomForestModel
ranger_model <- train(
  x = diabetes_mellitus_x,
  y = diabetes_mellitus_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl,
  preProcess = c("zv","nzv","knnImpute","pca") # "medianImpute"
)
ranger_model
plot(ranger_model)
plot(ranger_model$finalModel)

#### Compare models
model_list <- list("glmnet" = glmnet_model, "ranger" = ranger_model)
# Pass model_list to resamples(): resamples
resamp = resamples(model_list)
# Summarize the results
summary(resamp)
dotplot(resamp, metric="ROC")
xyplot(resamp, metric="ROC" )


#### Make predictions
test_df <- fread(file.path(data_dir, "UnlabeledWiDS2021.csv"))
test_dat = test_df %>% select_at(c("encounter_id",selected_predictors))
submit_df <- f_save_submission_csv(test_dat, glm_model)
table(submit_df$diabetes_mellitus)

