# UBJ Doctorado en Administracion Gerencial
# Modelo Predictivo de la TRM Utilizando Machine Learning
# LIB ubj/code/compararModelos,R
# FECHA 12/07/2021
# Ariel E. Meilij
#
# BRIEF Este script construye la gráfica que compara los valores reales
# de los tres modelos predictivos versus sus predicciones.

# SECCION 1 - MODELO DE REGRESION MULTIVARIABLE

library(tseries)
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(reshape2)
library(ggpubr)
library(caret)

load("data/trm_df")
load("data/palma_df")
load("data/oro_df")
load("data/wti_df")
load("data/cafe_df")
load("data/banana_df")
load("data/niquel_df")
load("data/gasoil_df")
load("data/polipropileno_df")
load("data/hulla_df")
load("data/carbon_df")

df1 <- merge(trm_df, palma_df)
df1 <- merge(df1, oro_df)
df1 <- merge(df1, wti_df)
df1 <- merge(df1, cafe_df)
df1 <- merge(df1, banana_df)
df1 <- merge(df1, niquel_df)
df1 <- merge(df1, gasoil_df)
df1 <- merge(df1, polipropileno_df)
df1 <- merge(df1, hulla_df)
df1 <- merge(df1, carbon_df)
df_data <- df1[, 2:12]
rm(df1, trm_df, palma_df, oro_df, wti_df, cafe_df, banana_df, niquel_df, gasoil_df, polipropileno_df, hulla_df, carbon_df)

set.seed(7556014)
inTrain <- createDataPartition(y = df_data$trm, p = 0.7, list = FALSE)
training <- df_data[inTrain, ]
testing <- df_data[-inTrain, ]

modelFit <- train(trm ~ ., data = training, method = "glm")

valores_reales <- testing[,1]
valores_prediccion <- predict(modelFit, testing[,2:11])
testVector <- data.frame(valores_prediccion, valores_reales)
ggplot(aes(x = valores_prediccion, y = valores_reales), data = testVector) + 
  geom_point(alpha = 0.05)  + geom_smooth(method='lm',formula=y~x, colour = "yellow") +
  labs(x = "Valores Prediccion", y = "Valores Reales (Juego Test)", 
       title = "Regresión Multivariable") + theme(plot.title = element_text(hjust = 0.5))


