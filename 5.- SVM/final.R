##################################
#### Carga de Librerías ##########
##################################

library(e1071)
library(ggplot2)
library(pROC)
library(caret)

##################################
#### Lectura y Preprocesamiento ##
##################################

# Lectura del dataset
setwd("/home/roko/cursos/mineria/lab5/data")
df_2_clases <- "wine_so_2_clases.csv"
#df_wine <- read.csv("winequality-red.csv", sep = ";")
df_wine <- read.csv(df_2_clases)

# Eliminación de variables segun la literatura
df_wine$citric.acid <- NULL 
df_wine$density <- NULL 
df_wine$chlorides <- NULL # 0.2631189 
df_wine$residual.sugar <- NULL # 0.2681051 
df_wine$fixed.acidity <- NULL
df_wine$free.sulfur.dioxide <- NULL # 0.2339

# Eliminación de variables segun RF
df_wine$residual.sugar <- NULL  #
df_wine$citric.acid <- NULL 
df_wine$fixed.acidity <- NULL 
df_wine$free.sulfur.dioxide <- NULL #  0.2389601 
df_wine$pH <- NULL #  0.2581061
df_wine$total.sulfur.dioxide <- NULL 
df_wine$chlorides <-NULL 
df_wine$volatile.acidity <- NULL
df_wine$density <- NULL
df_wine$sulphates <- NULL
df_wine$alcohol <- NULL

# Convertir la variable objetivo a factor
df_wine$Calidad <- as.factor(df_wine$Calidad)

# Normalización de las variables predictoras
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)) * 2 - 1)
}
predictors <- df_wine[, -ncol(df_wine)]
normalized_predictors <- as.data.frame(lapply(predictors, normalize))
df_wine_normalized <- data.frame(normalized_predictors, Calidad = df_wine$Calidad)

x <- subset(df_wine_normalized, select = -Calidad)

##################################
#### SVM Kernel Lineal ###########
##################################

# Entrenamiento con SVM lineal y ajuste de parámetros
set.seed(101)
svm_lineal <- tune(
  svm,
  Calidad ~ ., 
  data = df_wine_normalized, 
  kernel = "linear",
  ranges = list(cost = 2^(-5:5)),
  tunecontrol = tune.control(sampling = "cross", cross = 5)
)

# Mejor modelo lineal
best_model <- svm_lineal$best.model
pred <- predict(best_model, x)

# Info parametros encontrados 
summary(svm_lineal)

# Matriz de confusión y métricas
conf_matrix <- confusionMatrix(pred, df_wine_normalized$Calidad)
print("Matriz de confusión (Kernel Lineal):")
print(conf_matrix)


##################################
#### SVM Kernel Radial ###########
##################################

# Entrenamiento con SVM radial y ajuste de parámetros
set.seed(101)

svm_radial <- tune(
  svm,
  Calidad ~ ., 
  data = df_wine_normalized, 
  kernel = "radial",
  ranges = list(cost = 2^(-10:10), gamma = 2^(-10:10)),
  tunecontrol = tune.control(sampling = "cross", cross = 10)
)

# Mejor modelo radial
best_model_radial <- svm_radial$best.model
pred_radial <- predict(best_model_radial, x)

# Info parametros encontrados 
summary(svm_radial)

# Matriz de confusión y métricas
conf_matrix_radial <- confusionMatrix(pred_radial, df_wine_normalized$Calidad)
print("Matriz de confusión (Kernel Radial):")
print(conf_matrix_radial)

##################################
#### Reducción Dimensional (MDS) #
##################################

# Calcular la matriz de distancias y aplicar MDS
dist_matrix <- dist(df_wine[, -ncol(df_wine)])
mds_result <- cmdscale(dist_matrix, k = 2)

# Crear un dataframe con las coordenadas reducidas
mds_df <- data.frame(
  X1 = mds_result[, 1], 
  X2 = mds_result[, 2], 
  Calidad = df_wine$Calidad
)

##################################
#### Visualización MDS Radial ####
##################################

# Entrenar el modelo SVM con kernel radial en el espacio MDS
best_model_mds <- svm(
  Calidad ~ ., 
  data = mds_df, 
  kernel = "radial", 
  cost = 2, 
  gamma = 1
)

# Crear una malla de puntos y predecir regiones de decisión
x_seq <- seq(min(mds_df$X1) - 1, max(mds_df$X1) + 1, length.out = 200)
y_seq <- seq(min(mds_df$X2) - 1, max(mds_df$X2) + 1, length.out = 200)
grid <- expand.grid(X1 = x_seq, X2 = y_seq)
grid$Predicted <- predict(best_model_mds, grid)

# Graficar las regiones de decisión
ggplot() +
  geom_tile(data = grid, aes(x = X1, y = X2, fill = Predicted), alpha = 0.3) +
  geom_point(data = mds_df, aes(x = X1, y = X2, color = Calidad), size = 3) +
  labs(title = "Visualización del SVM con Kernel Radial en el Espacio MDS",
       x = "Dimensión 1 (MDS)", y = "Dimensión 2 (MDS)") +
  theme_minimal() +
  scale_fill_viridis_d(name = "Regiones (Predicted)") +
  scale_color_viridis_d(name = "Clases Reales")

##################################
#### Visualización MDS Lineal ####
##################################

# Entrenar el modelo SVM con kernel lineal en el espacio MDS
best_model_mds_linear <- svm(
  Calidad ~ ., 
  data = mds_df, 
  kernel = "linear", 
  cost =  0.5
)

# Crear una malla de puntos y predecir regiones de decisión
grid$Predicted <- predict(best_model_mds_linear, grid)

# Graficar las regiones de decisión
ggplot() +
  geom_tile(data = grid, aes(x = X1, y = X2, fill = Predicted), alpha = 0.3) +
  geom_point(data = mds_df, aes(x = X1, y = X2, color = Calidad), size = 3) +
  labs(title = "Visualización del SVM con Kernel Lineal en el Espacio MDS",
       x = "Dimensión 1 (MDS)", y = "Dimensión 2 (MDS)") +
  theme_minimal() +
  scale_fill_viridis_d(name = "Regiones (Predicted)") +
  scale_color_viridis_d(name = "Clases Reales")


##################################
#### Visualización MDS Polinomial #
##################################

# Entrenar el modelo SVM con kernel polinomial en el espacio MDS
best_model_mds_poly <- svm(
  Calidad ~ ., 
  data = mds_df, 
  kernel = "polynomial", 
  cost = 4,   # Ajustar este parámetro según sea necesario
  degree = 5  # Grado del polinomio (puedes ajustar este valor)
)

# Crear una malla de puntos y predecir regiones de decisión
grid$Predicted <- predict(best_model_mds_poly, grid)

# Graficar las regiones de decisión
ggplot() +
  geom_tile(data = grid, aes(x = X1, y = X2, fill = Predicted), alpha = 0.3) +
  geom_point(data = mds_df, aes(x = X1, y = X2, color = Calidad), size = 3) +
  labs(title = "Visualización del SVM con Kernel Polinomial en el Espacio MDS",
       x = "Dimensión 1 (MDS)", y = "Dimensión 2 (MDS)") +
  theme_minimal() +
  scale_fill_viridis_d(name = "Regiones (Predicted)") +
  scale_color_viridis_d(name = "Clases Reales")

