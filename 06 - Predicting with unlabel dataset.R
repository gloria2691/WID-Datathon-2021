# Doing prediction with the unlabel dataset


require(randomForest)

#Load Data unlabel

data_unlabel <- read.csv(file.path(data_dir, "UnlabeledWiDS2021.csv"))

View(data_unlabel)
sum(is.na(data_unlabel))

# Predicting values 
?predict
predictions_unlabel <- predict(modelo, newdata = data_unlabel, type = 'prob', na.action=na.fail)


# Result
View(predictions_unlabel)


#performance 
?performance
?prediction
