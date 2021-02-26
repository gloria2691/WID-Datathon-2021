# Doing predictions

require(randomForest)


# Prediction with test data 

previsoes <- data.frame(observed = dados_teste$diabetes_mellitus,
                        prediction = predict(modelo, newdata = dados_teste))

# Visualizando o resultado
View(previsoes)
View(dados_teste)
