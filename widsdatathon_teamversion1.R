
#Women in data Science Datathon 2021 


setwd("C:/Users/Asus/Documents/Gloria Valero/Kaggle/widsdatathon2021")
getwd()

#Classification Problem


## Step1 - Collecting data
data <- read.csv("TrainingWiDS2021.csv")
str(data[,0:100])
View(data)
  
#Data dictionary

dict <- read.csv("DataDictionaryWiDS2021.csv")
View(dict)

 # target : diabetes_mellitus

#Step 2 - Pre-processing
  
  # Removing the ID Columns: could cause overfitting
  
data[, c("encounter_id","hospital_id","icu_id","X")]<-list(NULL)
  
View(data)

# Exploratory data analysis

#Removing columns with more than 80 % of missing data

data_new <- data[,colSums(is.na(data))< 0.8*nrow(data)]
View(data_new)


# Plotting the Number of results of the variable target 0 - 1

ggplot(data_new,aes(diabetes_mellitus)) + geom_bar(aes(fill = factor(diabetes_mellitus)), alpha = 0.5)

#Exploring demographic data category
 
##Plotting 

library(ggplot2)
str(data_new[,c(1:13)])

###Age
ggplot(data_new, aes(age)) + geom_bar() + 
      facet_grid(. ~ diabetes_mellitus) + 
      ggtitle(paste("Total Diabetes 0/1 by ","age"))
 
ggplot(data_new,aes(age)) + geom_histogram(fill = 'blue', bins = 20, alpha = 0.5)    

####Getting the mean value of age for 0 - 1 diabetes
       

by(data_new, data_new$diabetes_mellitus, function(x){
      
      mean.pl <- mean(x$age, na.rm = TRUE)
    })
    
####Filling the NA values of age with the mean of each 0-1
    
impute_NA_mean <- function(coluna, class, out0, out1 ){
      out <- coluna
      for (i in 1:length(coluna)){
        
        if (is.na(coluna[i])){
          
          if (class[i] == 0){
            out[i] <- out0
            
          }else {
            out[i] <- out1
            
          }}
      }
      return(out)
        }
      
fixed.ages <- impute_NA_mean(data_new$age, data_new$diabetes_mellitus, 61.27 , 64.56 )
data_new$age <- fixed.ages
mean(data_new$age)
    
###bmi
ggplot(data_new,aes(bmi)) + geom_histogram(fill = 'blue', bins = 20, alpha = 0.5)    

ggplot(data_new, aes(bmi)) +
  geom_bar() + 
  facet_grid(. ~ diabetes_mellitus) + 
  ggtitle(paste("Total Diabetes 0/1 by","bmi"))

by(data_new, data_new$diabetes_mellitus, function(x){
  
  mean.pl <- mean(x$bmi, na.rm = TRUE)
})

fixed.bmi <- impute_NA_mean(data_new$bmi, data_new$diabetes_mellitus, 28.37 , 31.77)
data_new$bmi <- fixed.bmi
mean(data_new$bmi)

###ethnicity    

ggplot(data_new, aes(ethnicity)) +
      geom_bar() + 
      facet_grid(. ~ diabetes_mellitus) + 
      ggtitle(paste("Total Diabetes 0/1 by","ethnicity"))

#Replacing NAs with the value Caucasian
library(tidyr)
data_new$ethnicity <-   replace_na(data_new$ethnicity, "Caucasian")

###gender

    ggplot(data_new, aes(gender)) +
      geom_bar() + 
      facet_grid(. ~ diabetes_mellitus) + 
      ggtitle(paste("Total Diabetes 0/1 by","gender"))
 
##weight
    
pl <- ggplot(data_new, aes(diabetes_mellitus,weight)) + geom_boxplot(aes(group = diabetes_mellitus, fill = factor(diabetes_mellitus), alpha = 0.4)) 
  pl + scale_y_continuous(breaks = seq(min(50), max(200), by = 20))
  
by(data_new, data_new$diabetes_mellitus, function(x){
   
    mean.pl <- mean(x$weight, na.rm = TRUE)
  })

fixed.weight <- impute_NA_mean(data_new$weight, data_new$diabetes_mellitus, 81.75 , 91.17 )
data_new$weight <- fixed.weight
mean(data_new$weight)
  
###Featuring selection for demographic category

colnames(data_new[, c(1:13)])

data_new$diabetes_mellitus<- as.factor(data_new$diabetes_mellitus)
data$diabetes_mellitus<- as.factor(data$diabetes_mellitus)  
library(randomForest)
?randomForest

#using the na.action= na.roughfix to fill the NA values with the mean

modelo <- randomForest( diabetes_mellitus ~ age+bmi + elective_surgery+ ethnicity+ gender
                          + height+ weight+hospital_admit_source+icu_admit_source+
                            icu_stay_type+icu_type+pre_icu_los_days+readmission_status, data = data_new, ntree = 100, nodesize = 10, importance = T,na.action= na.roughfix )
  

varImpPlot(modelo)


#Important features for demographic category : age, bmi, height, weight.
  