# Cargar librerías necesarias
library(mclust)
library(dplyr)
library(ggplot2)

setwd("/home/roko/cursos/mineria/lab1/code_r")
df_original_cp = "wine_cp-calidad.csv"
df_2_clases = "df_2_clases_SO.csv"
df_3_clases = "df_3_clases_SO.csv"
df_4_clases = "df_4_clases_SO.csv"

df_intermedios = "df_intermedios.csv" # solo contiene los vinos con calidad 4, 5 y 6 del df original
df_buenos_malos = "df_buenos_malos.csv" # solo tiene los vinos buenos y malos del df original

df_wine <- read.csv(df_2_clases)
print(nrow(df_wine))
conteo_calidad <- table(df_wine$Calidad)
conteo_calidad

# Guardar la columna 'Calidad' para luego usarla en la comparación
categorias <- df_wine$Calidad
df_wine <- df_wine %>% select(-Calidad)


# =========================================================================================== #
# ======== Agrupamiento con Gaussian Mixture Model - Selección BIC ========================== #
# =========================================================================================== #

BIC=mclustBIC(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC) #se grafican los BIC por configuración de parámetros
summary(BIC)
mod11=Mclust(df_wine, x=BIC)
plot(mod11, what = "classification")
print(tabla_comparacion <- table(categorias, mod11$classification))
ari_value <- adjustedRandIndex(categorias, mod11$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# ========================================================================================== #
# ======================================= Prueba manual ==================================== #
# ========================================================================================== #

# Modelo EII
mod_EII = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EII")
plot(mod_EII, what = "classification")
print(tabla_comparacion_EII <- table(categorias, mod_EII$classification))
ari_value <- adjustedRandIndex(categorias, mod_EII$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo VII
mod_VII = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VII")
plot(mod_VII, what = "classification")
print(tabla_comparacion_VII <- table(categorias, mod_VII$classification))
ari_value <- adjustedRandIndex(categorias, mod_VII$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo EEI
mod_EEI = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EEI")
plot(mod_EEI, what = "classification")
print(tabla_comparacion_EEI <- table(categorias, mod_EEI$classification))
ari_value <- adjustedRandIndex(categorias, mod_EEI$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo VEI
mod_VEI = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VEI")
plot(mod_VEI, what = "classification")
print(tabla_comparacion_VEI <- table(categorias, mod_VEI$classification))
ari_value <- adjustedRandIndex(categorias, mod_VEI$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo EVI
mod_EVI = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EVI")
plot(mod_EVI, what = "classification")
print(tabla_comparacion_EVI <- table(categorias, mod_EVI$classification))
ari_value <- adjustedRandIndex(categorias, mod_EVI$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))
# Graficar las curvas de nivel de cada par de variables
plot(mod_EVI, what = "density", type = "contour")

# Modelo VVI
mod_VVI = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VVI")
plot(mod_VVI, what = "classification")
print(tabla_comparacion_VVI <- table(categorias, mod_VVI$classification))
ari_value <- adjustedRandIndex(categorias, mod_VVI$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo EEE
mod_EEE = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EEE")
plot(mod_EEE, what = "classification")
print(tabla_comparacion_EEE <- table(categorias, mod_EEE$classification))
ari_value <- adjustedRandIndex(categorias, mod_EEE$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo EEV
mod_EEV = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EEV")
plot(mod_EEV, what = "classification")
print(tabla_comparacion_EEV <- table(categorias, mod_EEV$classification))
ari_value <- adjustedRandIndex(categorias, mod_EEV$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo VEV
mod_VEV = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VEV")
plot(mod_VEV, what = "classification")
print(tabla_comparacion_VEV <- table(categorias, mod_VEV$classification))
ari_value <- adjustedRandIndex(categorias, mod_VEV$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# Modelo VVV
mod_VVV = Mclust(df_wine, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VVV")
plot(mod_VVV, what = "classification")
print(tabla_comparacion_VVV <- table(categorias, mod_VVV$classification))
ari_value <- adjustedRandIndex(categorias, mod_VVV$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

# ========================================================================================== #
# ====================== Comparar Clusters con clases originales =========================== #
# ========================================================================================== #

# Llamar a la función con las categorías originales y la clasificación del modelo
print(tabla_comparacion <- table(categorias, mod11$classification ))

mod2 = Mclust(df_wine, G = 6)
ari_value <- adjustedRandIndex(categorias, mod2$classification)
print(paste("Adjusted Rand Index (ARI):", ari_value))

summary(mod2)
print(tabla_comparacion <- table(categorias, mod2$classification ))
plot(mod2, what = "classification")


