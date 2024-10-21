# UBJ Doctorado en Administracion Gerencial
# Modelo Predictivo de la TRM Utilizando Machine Learning
# LIB ubj/code/testAutoARIMA 
# FECHA 19/08/2018
# Ariel E. Meilij
#
# BRIEF Pronostico de la TRM utilizando machine learning con ARIMA

library(forecast)
library(ggplot2)

load("data/trm_ts")

# Descomposicion de serie de tiempo TRM
plot(decompose(trm_ts))

# Evaluar autocorrelacion
par(mfrow=c(1,2))
acf(trm_ts)
pacf(trm_ts)

# Utilizar funcion auto.arima para modelo automatico ARIMA
modelFit <- auto.arima(trm_ts)
valores_reales <- trm_ts
valores_prediccion <- modelFit$fitted

modelFit
print(paste("R2: ", round(cor(valores_reales, valores_prediccion)^2,3)))

# Plot Data de Prueba vs. Valores Reales
testVector <- data.frame(valores_prediccion, valores_reales)
ggplot(aes(x = valores_prediccion, y = valores_reales), data = testVector) + 
  geom_point(alpha = 0.05) + 
  geom_smooth(method='lm',formula=y~x, colour = "gray") +
  labs(x = "Valores Prediccion", y = "Valores Reales", 
       title = "Valores Reales vs. Valores Prediccion ARIMA(3,1,2) para TRM 2010-2017 ")

# Test individual de valores aleatorios comparativos
indices_aleatorios <-  sample.int(length(trm_ts), 100)
y_values <- trm_ts[indices_aleatorios]
y_hat <- modelFit$fitted[indices_aleatorios]
testMatrix <- data.frame(y_values, y_hat, round(((y_hat/y_values)-1)*100,1))
colnames(testMatrix) = c("VALOR REAL", "PREDICCION", "ERROR %")
print(testMatrix)
print(mean(testMatrix$`ERROR %`))

# Plot del Test
ggplot(testMatrix, aes(x = y_hat, y = y_values)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Test Aleatorio Valores Reales vs. Prediccion ARIMA") +
  theme(plot.title = element_text(hjust = 0.5))

cor(y_values, y_hat, method = "spearman")




