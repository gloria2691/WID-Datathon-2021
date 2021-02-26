
# Evaluating the model 

# Creating the ROC Curve 

install.packages("ROCR")
library("ROCR")

# Creating the class data 
?predict

class1 <- predict(modelo, newdata = dados_teste, type = 'prob', na.rm=TRUE)
class2 <- dados_teste$diabetes_mellitus

# ROC curve
?prediction
?performance

pred1 <- prediction(class1[,2], class2)
perf <- performance(pred1, "tpr","fpr") 
plot(perf, col = rainbow(10))

# Confusion Matrix with Caret

library(caret)
?confusionMatrix

confusionMatrix(previsoes$observed, previsoes$prediction)
View(previsoes)

