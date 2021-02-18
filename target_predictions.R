# Title     : Women in data Science Datathon 2021
# Objective : Classification Problem, predict diabetes yes/no
# Team      : <team_name>
# Related rscripts: data_processing.R, target_prediction.R, functions.R
# Created on: 2/18/2021
###----------------------------------------

### Load packages
library(tidyverse)
library(data.table)
library(caret)
library(caTools)

### Custom settings
VIEW = FALSE  #TRUE
source(file.path("functions.R"))

### Load data
codebook <- fread(file.path(data_dir, "DataDictionaryWiDS2021.csv"))
colnames(codebook) <- gsub(" ", "_", tolower(colnames(codebook)))
cols_not_exist <- which(!((unique(codebook$variable_name)) %in% (colnames(train_df))))
(vars_not_exist <- unique(codebook$variable_name)[cols_not_exist])
codebook <- codebook %>% filter(variable_name != vars_not_exist)
cols_cat <- f_cols_by_cat(codebook)

train_df <- fread(file.path(data_dir, "TrainingWiDS2021.csv"))
test_df <- fread(file.path(data_dir, "UnlabeledWiDS2021.csv"))

### Simple caret example (testing)
#target_var = 'diabetes_mellitus'
demographic = c("age","bmi","gender","ethnicity")
comorbidity = cols_cat$apachecomorbidity
#lab = c()

selected_predictors = c(demographic, comorbidity) # cols_intstr$numeric
train_df_sub = train_df %>% select_at(c(selected_predictors, target_var)) %>% na.omit()

###------------------------- EXPLORATIVE ------------
### Take another subset of train dataset
useTRAIN = TRUE
if(useTRAIN){
  ### Split train dataset into two, to assess model performance before making predictions on the unlabelled dataset
  split <- round(nrow(train_df_sub) * 0.80)
  train_dat <- train_df_sub[1:split, ]
  test_dat <- train_df_sub[(split + 1):nrow(train_df_sub), ]
}else{
  train_dat = train_df_sub
  test_dat = test_df %>% select_at(selected_predictors) %>% na.omit()
}

### Fit glm model
model <- glm(diabetes_mellitus ~ .,family = "binomial", train_dat)
### Make predictions on the test dataset
p <- predict(model, test_dat)
### Calculate Squared mean error for assessing performance
if(useTRAIN)sqrt(mean((p - test_dat[["diabetes_mellitus"]])^2))
# Create 'confusion matrix' (false positives, false negatives, true positives, true negatives)
#diab_or_nodiab <- ifelse(p > 0.5, 1, 0)
#p_class <- factor(diab_or_nodiab, levels = levels(test_dat[["diabetes_mellitus"]]))
#confusionMatrix(p_class, test_dat[["diabetes_mellitus"]])
#colAUC(p, test_dat[["diabetes_mellitus"]], plotROC = TRUE)


### Caret package
useCaret=FALSE ### not working yet!
if(useCaret){
  # Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
train_dat[["diabetes_mellitus_fct"]] = factor(train_dat[["diabetes_mellitus"]] , labels=c("No","Yes"))
model <- train(
  diabetes_mellitus_fct ~ .,
  train_dat,
  method = "glm",
  trControl = myControl
)
p <- predict(model, train_dat)
#colAUC(p, train_dat[["diabetes_mellitus"]], plotROC = TRUE)
#cbind(p, train_dat[["diabetes_mellitus_fct"]])
}