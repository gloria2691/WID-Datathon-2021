
#Women in data Science Datathon 2021 
# Objective : Classification Problem, predict diabetes yes/no
# target : diabetes_mellitus

### Load packages
require(tidyverse)
require(data.table)
require(dplyr)
require(ggplot2)
require(randomForest)

### Custom settings

source(file.path("functions.R"))

# Define directories
setwd("C:/Users/Asus/Documents/Gloria Valero/Kaggle/widsdatathon2021")

getwd()
data_dir <- list.files(getwd(), "download_dat")
list.files(data_dir)

### Load data
train_df_clean <- read.csv(file.path(data_dir, "TrainingWiDS2021_cleaned.csv"))
train_df_clean$diabetes_mellitus <- factor(train_df_clean$diabetes_mellitus, levels=c('nodiabetes','diabetes'), labels=c('nodiabetes','diabetes'))
target_var = 'diabetes_mellitus'
str(train_df_clean$diabetes_mellitus)

###---------Featuring selection--------### 

View(train_df_clean)

###Featuring selection for demographic category

train_df_clean$diabetes_mellitus <- as.factor(train_df_clean$diabetes_mellitus)

#using the na.action= na.roughfix to fill the NA values with the mean

sum(is.na(train_df_clean$diabetes_mellitus))
str(train_df_clean$diabetes_mellitus)
View(train_df_clean)

modelo_Demographic <- randomForest(diabetes_mellitus ~ age+bmi + elective_surgery+ ethnicity+ gender
                        + height+ weight+hospital_admit_source+icu_admit_source+
                          icu_stay_type+icu_type+pre_icu_los_days, data = train_df_clean, ntree = 100, nodesize = 10, importance = T,na.action= na.roughfix )

varImpPlot(modelo_Demographic)

#Important features for demographic category : age, bmi, height, weight.

###Featuring selection for APACHE covariate category

col_apache <- codebook$`Variable Name`[codebook$Category =="APACHE covariate"]
col_apache

data_apache <- train_df_clean[, colnames(codebook$`Variable Name`[codebook$Category =="APACHE covariate"])]                      

modelo_apache <- randomForest(diabetes_mellitus ~ ph_apache + resprate_apache+ sodium_apache +temp_apache +urineoutput_apache  
                              +ventilated_apache +wbc_apache +gcs_eyes_apache +
                                +gcs_motor_apache + gcs_unable_apache
                              + gcs_verbal_apache+ glucose_apache + heart_rate_apache + hematocrit_apache + intubated_apache   
                              + map_apache + paco2_apache+ paco2_for_ph_apache+pao2_apache 
                              +gcs_motor_apache+gcs_unable_apache+gcs_verbal_apache+glucose_apache     
                              +heart_rate_apache +hematocrit_apache +intubated_apache  +map_apache           
                              +paco2_apache +paco2_for_ph_apache + apache_3j_diagnosis+apache_post_operative
                              +arf_apache+bilirubin_apache+bun_apache+creatinine_apache+fio2_apache +gcs_eyes_apache
                              +albumin_apache+apache_2_diagnosis, data = train_df_clean, ntree = 100, nodesize = 10, importance = T,na.action= na.roughfix )

varImpPlot(modelo_apache)

#Important features for  category APACHE:  glucose_apache 

##Featuring selection Vitals
View(codebook)
View(train_df_clean)
col_vitals <- codebook$`Variable Name`[codebook$Category =="vitals"]
data_vital <- train_df_clean[,43:88]
data_vital$diabetes_mellitus <- train_df_clean$diabetes_mellitus
View(data_vital)

modelo_vitals <- randomForest(diabetes_mellitus ~ ., data = data_vital, ntree = 100, nodesize = 10, importance = T,na.action= na.roughfix )

varImpPlot(modelo_vitals)

#Important features for  category vitals: not relevant

##Featuring selection Labs

col_lab <- codebook$`Variable Name`[codebook$Category =="labs"]
col_lab
data_lab <- train_df_clean[,89:130]
data_lab$diabetes_mellitus <- train_df_clean$diabetes_mellitus
View(data_lab)
modelo_lab <- randomForest(diabetes_mellitus ~ ., data = data_lab, ntree = 100, nodesize = 10, importance = T,na.action= na.roughfix )
varImpPlot(modelo_lab)

#Important features for  category Labs:  d1_glucose_max, h1_glucose_max, d1_glucose_min, h1_glucose_min, d1_creatinine_min, d1_creatinine_max 

##Featuring selection Labs blood gas 

col_labblood <- codebook$`Variable Name`[codebook$Category =="labs blood gas"]
col_labblood
data_labblood <- train_df_clean[,131:138]
data_labblood$diabetes_mellitus <- train_df_clean$diabetes_mellitus
View(data_labblood)
modelo_labblood <- randomForest(diabetes_mellitus ~ ., data = data_labblood, ntree = 100, nodesize = 10, importance = T,na.action= na.roughfix )
varImpPlot(modelo_labblood)
#Important features for  category Labs blood:  d1_arterial_po2_max, d1_arterial_po2_min, d1_arterial_pco2_max 

##Featuring selection APACHE comorbidity

col_comor <- codebook$`Variable Name`[codebook$Category =="APACHE comorbidity"]

data_comor <- train_df_clean[,139:146]
View(data_comor)
data_comor$diabetes_mellitus <- train_df_clean$diabetes_mellitus
View(data_comor)
modelo_comor <- randomForest(diabetes_mellitus ~ ., data = data_comor, ntree = 100, nodesize = 10, importance = T,na.action= na.roughfix )
varImpPlot(modelo_comor)

#Parameter in APACHE comorbidity :not relevant

####Final dataset to do the model 

df_train <-  train_df_clean[, c("age", "bmi", "height", "weight","glucose_apache",  
                                "d1_glucose_max", "h1_glucose_max","d1_glucose_min", "h1_glucose_min", "d1_creatinine_min",
                                "d1_creatinine_max", "diabetes_mellitus")]

View(df_train)

###Missing values ###

###Age

ggplot(df_train, aes(age)) + geom_bar() + 
  facet_grid(. ~ diabetes_mellitus) + 
  ggtitle(paste("Total Diabetes 0/1 by ","age"))

ggplot(df_train,aes(age)) + geom_histogram(fill = 'blue', bins = 20, alpha = 0.5)    

####Getting the mean value of age for 0 - 1 diabetes


by(df_train, df_train$diabetes_mellitus, function(x){
  
  mean.pl <- mean(x$age, na.rm = TRUE)
})
sum(is.na(df_train$age))

####Filling the NA values of age with the mean of each 0-1

fixed.ages <- impute_NA_mean(df_train$age, df_train$diabetes_mellitus, 61.27 , 64.56 )
df_train$age <- fixed.ages
mean(df_train$age)

###bmi
ggplot(df_train,aes(bmi)) + geom_histogram(fill = 'blue', bins = 20, alpha = 0.5)    

ggplot(df_train, aes(bmi)) +
  geom_bar() + 
  facet_grid(. ~ diabetes_mellitus) + 
  ggtitle(paste("Total Diabetes 0/1 by","bmi"))

by(df_train, df_train$diabetes_mellitus, function(x){
  
  mean.pl <- mean(x$bmi, na.rm = TRUE)
})

fixed.bmi <- impute_NA_mean(df_train$bmi, df_train$diabetes_mellitus, 28.37 , 31.77)
df_train$bmi <- fixed.bmi
mean(df_train$bmi)
sum(is.na(df_train$bmi))
##weight

pl <- ggplot(df_train, aes(diabetes_mellitus,weight)) + geom_boxplot(aes(group = diabetes_mellitus, fill = factor(diabetes_mellitus), alpha = 0.4)) 
pl + scale_y_continuous(breaks = seq(min(50), max(200), by = 20))

by(df_train, df_train$diabetes_mellitus, function(x){
  
  mean.pl <- mean(x$weight, na.rm = TRUE)
})

fixed.weight <- impute_NA_mean(df_train$weight, df_train$diabetes_mellitus, 81.75 , 91.17 )
df_train$weight <- fixed.weight
mean(df_train$weight)  
sum(is.na(df_train$weight))

##height

mean_d1<- formula(df_train,height)
mean_d1

  fixes_d1 <- impute_NA_mean(df_train$height,df_train$diabetes_mellitus ,mean_d1[1], mean_d1[2])
df_train$height <- fixes_d1
mean(df_train$height)


##glucose apache
sum(is.na(df_train$glucose_apache))
by(df_train, df_train$diabetes_mellitus, function(x){
  
  mean.pl <- mean(x$glucose_apache, na.rm = TRUE)
})
fixed.glucoseapache <- impute_NA_mean(df_train$glucose_apache, df_train$diabetes_mellitus, 142.338 , 218.16 )
df_train$glucose_apache <- fixed.glucoseapache

##d1_glucose_max

mean_d1<- formula(df_train,d1_glucose_max )
mean_d1

fixes_d1 <- impute_NA_mean(df_train$d1_glucose_max,df_train$diabetes_mellitus ,mean_d1[1], mean_d1[2])
df_train$d1_glucose_max <- fixes_d1
mean(df_train$d1_glucose_max)

##d1_glucose_max

mean_d1<- formula(df_train,d1_glucose_min )
mean_d1

fixes_d1 <- impute_NA_mean(df_train$d1_glucose_min,df_train$diabetes_mellitus ,mean_d1[1], mean_d1[2])
df_train$d1_glucose_min <- fixes_d1
mean(df_train$d1_glucose_min)

##h1_glucose_min

View(df_train)
sum(is.na(df_train$h1_glucose_min))
mean_d1<- formula(df_train,h1_glucose_min )
mean_d1

fixes_d1 <- impute_NA_mean(df_train$h1_glucose_min ,df_train$diabetes_mellitus ,mean_d1[1], mean_d1[2])
df_train$h1_glucose_min  <- fixes_d1
mean(df_train$h1_glucose_min )

#d1_creatinine_min
sum(is.na(df_train$d1_creatinine_min))
mean_d1<- formula(df_train,d1_creatinine_min )
mean_d1

fixes_d1 <- impute_NA_mean(df_train$d1_creatinine_min ,df_train$diabetes_mellitus ,mean_d1[1], mean_d1[2])
df_train$d1_creatinine_min  <- fixes_d1
mean(df_train$d1_creatinine_min)

#d1_creatinine_max
sum(is.na(df_train$d1_creatinine_max))
mean_d1<- formula(df_train,d1_creatinine_max)
mean_d1

fixes_d1 <- impute_NA_mean(df_train$d1_creatinine_max ,df_train$diabetes_mellitus ,mean_d1[1], mean_d1[2])
df_train$d1_creatinine_max  <- fixes_d1
mean(df_train$d1_creatinine_max)

### Save data

fwrite(df_train, file=file.path(data_dir, "Data_modelo.csv"))
sum(is.na(df_train))

