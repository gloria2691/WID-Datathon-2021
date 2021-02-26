#Women in data Science Datathon 2021 
# Objective : Classification Problem, predict diabetes yes/no
# target : diabetes_mellitus

setwd("C:/Users/Asus/Documents/Gloria Valero/Kaggle/widsdatathon2021")
getwd()
getwd()
data_dir <- list.files(getwd(), "download_dat")

# Creating a Model with Random Forest

data <- read.csv(file.path(data_dir, "Data_modelo.csv"))
View(data)
table(data$diabetes_mellitus)

# Spliting training data and test data 

splitData <- function(dataframe, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}
# getting training data and test data 
splits <- splitData(data, seed = 808)

dados_treino <- splits$trainset
dados_teste <- splits$testset

# Checking the row number 
nrow(dados_treino)
sum(is.na(dados_treino))
View(dados_treino)

nrow(dados_teste)
sum(is.na(dados_treino$diabetes_mellitus))
dados_treino$diabetes_mellitus <- as.factor(dados_treino$diabetes_mellitus)

# Creating a model 

modelo <- randomForest( diabetes_mellitus ~ ., 
                        data = dados_treino, 
                        ntree = 100, 
                        nodesize = 10)

# Creating model with importance = TRUE 


modelo2 <- randomForest( diabetes_mellitus ~ ., 
                        data = dados_treino, 
                        ntree = 100, 
                        nodesize = 10, importance=TRUE)
# Printing the result 

print(modelo)
print(modelo2)