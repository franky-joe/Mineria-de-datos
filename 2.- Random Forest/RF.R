# Cargar librerías necesarias
# install.packages("randomForest")
# install.packages("MASS")
library(mclust)
library(dplyr)
library(ggplot2)
library(randomForest)
library(MASS) # para grafico de coor. paralelas


setwd("/home/roko/cursos/mineria/lab2/datos_sin_out_balanceados")
df_original = "wine_so_6_clases.csv"
df_2_clases = "wine_so_2_clases.csv"
df_3_clases = "wine_so_3_clases.csv"
df_4_clases = "wine_so_4_clases.csv"

df_wine <- read.csv(df_2_clases)
print(nrow(df_wine))
conteo_calidad <- table(df_wine$Calidad)
print(conteo_calidad)

# Verifica si la columna 'Calidad' ya es un factor, de lo contrario conviértela
df_wine$Calidad <- as.factor(df_wine$Calidad)

# Seleccionar todas las columnas excepto 'Calidad' sin dplyr
predictors <- df_wine[, !(names(df_wine) %in% 'Calidad')]

# Normalizar las variables predictoras
predictors_normalized <- as.data.frame(scale(predictors))  # Normalizar usando scale()

# Combinar de nuevo los predictores normalizados con la columna 'Calidad'
df_wine_normalized <- cbind(predictors_normalized, Calidad = df_wine$Calidad)
df_wine
df_wine$residual.sugar <- NULL  
df_wine$free.sulfur.dioxide <- NULL
df_wine$citric.acid <- NULL 
df_wine$fixed.acidity <- NULL
df_wine$pH <- NULL  # Aqui sube a 18.5%
df_wine$density <- NULL # Aqui 18.65%
df_wine$chlorides <-NULL # Aqui 19.32%
df_wine$total.sulfur.dioxide <- NULL

# Establece la semilla para la generación del proceso aleatorio
set.seed(11) # 11 fue la mejor para soo 3 caracteristicas

wine_RF <- randomForest(formula = Calidad ~., data=df_wine, ntree = 14900, mtry = 1, importance = TRUE, proximity = TRUE, type = "classification")
plot(wine_RF)
# Muestra los resultados del modelo
print(wine_RF)

# Visualizar la importancia de las variables
importance(wine_RF)
varImpPlot(wine_RF)


##################################
#### Analisis de Importancia #####
##################################

# Calculamos la importancia de las variables del modelo Random Forest
importancia <- round(importance(wine_RF), 2)
print(importancia)

# (*) MeanDecreaseAccuracy: variables más importantes para la precisión
# (*) MeanDecreaseGini: variables más importantes para reducir impurezas

# Gráfica de importancia de las variables
varImpPlot(wine_RF)

##################################
#### Analisis de Proximidad ######
##################################

# Escalamiento clásico multidimensional usando los valores de proximidad
wine_mds <- cmdscale(1 - wine_RF$proximity, eig = TRUE)

# Para sistema de coordenadas cuadrado
op <- par(pty = "s")

# Visualizamos los predictores junto con las coordenadas del MDS
pairs(cbind(df_wine[, 1:7], wine_mds$points), cex = 0.6, gap = 0,
      col = c("red", "blue")[as.numeric(df_wine$Calidad)],
      main = "Datos del Vino: Predictores y MDS de Proximidad Basado en Random Forest")

# Restauramos las configuraciones de gráficos
par(op)

# Mostramos los valores propios de la matriz que representa las dimensiones (varianza explicada)
print(wine_mds$GOF)

# Grafica de MDS para las clases
MDSplot(wine_RF, df_wine$Calidad)

# Ajuste del número de árboles a 16500 y mtry a 3
wine_RF750 <- randomForest(formula = Calidad ~ ., data = df_wine, ntree = 16500, mtry = 1, importance = TRUE, proximity = TRUE)

# Gráfico de la evolución del error del Random Forest
plot(wine_RF750)
legend("topright", legend = levels(df_wine$Calidad), col = c("red", "green"), pch = 1, title = "Calidades de Vino", cex = 0.6)

# Gráfico de coordenadas paralelas
parcoord(df_wine[, 0:3], var.label = TRUE, col = c("green", "red")[as.numeric(df_wine$Calidad)])
legend("bottomright", legend = c("Alta", "Baja"), fill = 1:3)


