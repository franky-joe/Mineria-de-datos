################################################################
###################### Cargar librerías ########################
################################################################

install.packages("bnlearn")
library(bnlearn)

################################################################
####################### Exploracion ############################
################################################################

# Cargar el dataset de hailfinder
data(hailfinder)
summary(hailfinder)
hailfinder <- data.frame(hailfinder)
# 2. Obtener el número de observaciones y variables
num_observaciones <- nrow(hailfinder)
head(hailfinder)
num_variables <- ncol(hailfinder)
cat("Número de observaciones:", num_observaciones, "\n")
cat("Número de variables:", num_variables, "\n")

# 3. Dividir en conjuntos de entrenamiento y prueba
set.seed(123)  # Fijar semilla para reproducibilidad
train_indices <- sample(seq_len(num_observaciones), size = 0.7 * num_observaciones)
hailfinder_train <- hailfinder[train_indices, ]
hailfinder_train <- hailfinder
hailfinder_test <- hailfinder[-train_indices, ]

################################################################
############# Aplicación de Hill-Climbing ######################
################################################################
whitelist <- data.frame(
  from = c("AreaMesoALS", "AreaMesoALS", "AreaMesoALS", 
           "ScnRelPlFcst", "CapInScen","CurPropConv"),
  to = c("AreaMoDryAir", "CldShadeOth", "CompPlFcst",
         "PlainsFcst", "PlainsFcst", "PlainsFcst")
)
blacklist <- data.frame(
  from = c("PlainsFcst", "PlainsFcst","N34StarFcst","N34StarFcst","R5Fcst","R5Fcst"),
  to = c("LatestCIN", "LLIW", "LatestCIN", "LLIW","LatestCIN", "LLIW")
)

hill_clim <- hc(hailfinder,whitelist = whitelist, blacklist = blacklist)

fitted_bn <- bn.fit(hill_clim, data = hailfinder)

print(hill_clim)
sc<-score(hill_clim,hailfinder) # BIC por default
print(sc)

write.dot(fitted_bn, file = "~/cursos/mineria/lab4/hail-ajustado.dot")

)


################################################################
############### Query sobre emergencia climatica ###############
################################################################
# MountainFcst PlainsFcst N34StarFcst R5Fcst
levels(hailfinder$CombMoisture)

# Viento
# N07muVerMo SubjVertMo QGVertMotion CombVerMo WndHodograph
evidence_viento <- list(
  N07muVerMo = "StrongUp",     # Viento fuerte en N07muVerMo
  SubjVertMo = "StrongUp",     # Viento fuerte en SubjVertMo
  QGVertMotion = "StrongUp",   # Viento fuerte en QGVertMotion
  CombVerMo = "StrongUp",      # Viento fuerte en CombVerMo
  WndHodograph = "StrongWest" # Viento fuerte en WndHodograph
)
evidence_viento_debil <- list(
  N07muVerMo = "Neutral",     # Viento fuerte en N07muVerMo
  SubjVertMo = "Neutral",     # Viento fuerte en SubjVertMo
  QGVertMotion = "Neutral",   # Viento fuerte en QGVertMotion
  CombVerMo = "Neutral",      # Viento fuerte en CombVerMo
  WndHodograph = "StrongWest" # Viento fuerte en WndHodograph
)
levels(hailfinder$N07muVerMo)


# Nubes
evidence_nubes <- list(
  VISCloudCov = "Cloudy",       # Alta cobertura de nubes visibles
  IRCloudCover = "Cloudy",      # Alta cobertura de nubes infrarrojas
  CombClouds = "Cloudy",        # Alta combinación de nubes
  CldShadeOth = "Cloudy",       # Sombras de nubes presentes
  CldShadeConv = "Marked"       # Convección de sombras de nubes presente
)

# Humedad 
evidence_humedad <- list(
  SatContMoist = "VeryWet",       # Humedad muy alta en SatContMoist
  RaoContMoist = "VeryWet",       # Humedad muy alta en RaoContMoist
  CombMoisture = "VeryWet",       # Humedad combinada muy alta
  AreaMoDryAir = "VeryWet",        # Mínima presencia de aire seco
)

# Inestabilidad  y conveccion
evidence_inestabilidad_conveccion <- list(
  AMInstabMt = "Strong",         # Alta inestabilidad matutina en montañas
  InsInMt = "Strong",            # Alta inestabilidad en montañas
  InsChange = "Increasing",      # Incremento en la inestabilidad
  InsSclInScen = "MoreUnstable"      # Escala de inestabilidad fuerte
)

evidence_mix <- list(
  VISCloudCov = "Cloudy",       # Alta cobertura de nubes visibles
  IRCloudCover = "Cloudy",      # Alta cobertura de nubes infrarrojas
  CombClouds = "Cloudy",        # Alta combinación de nubes
  CldShadeOth = "Cloudy",       # Sombras de nubes presentes
  AMInstabMt = "Strong",         # Alta inestabilidad matutina en montañas
  InsInMt = "Strong",            # Alta inestabilidad en montañas
  InsChange = "Increasing",      # Incremento en la inestabilidad
  OutflowFrMt = "Strong",    # Presencia de flujo de salida de montaña
  MorningBound = "Strong",   # Límites matutinos presentes
  Boundaries = "Strong"      # Presencia de límites significativos
)
# Conveccion
evidence_conveccion <- list(
  OutflowFrMt = "Strong",    # Presencia de flujo de salida de montaña
  MorningBound = "Strong",   # Límites matutinos presentes
  Boundaries = "Strong"      # Presencia de límites significativos
)

x <- evidence_inestabilidad_conveccion

# MountainFcst
query_humedad_MountainFcst <- cpquery(
  fitted = fitted_bn,
  event = (MountainFcst == "SVR"),   # Evento de interés: clima severo en MountainFcst
  evidence = x,       # Evidencia de todas las variables de humedad
  method = "lw"                      # Método de cálculo: Likelihood Weighting
)
cat("Probabilidad de MountainFcst = 'SVR' dado que todas las variables de humedad son altas:", query_humedad_MountainFcst, "\n")


################################################################
######## maxmin_hill_clim y maxmin_parents_childrens ###########
################################################################

maxmin_hill_clim <- mmhc(hailfinder) 
maxmin_parents_childrens <- mmpc(hailfinder) 

arcs <- maxmin_parents_childrens$arcs

hill_clim <- bn.fit(hill_clim, data = hailfinder)

sc_clim<-score(hill_clim, hailfinder) 
print(sc)

# Ejemplo de eliminación de aristas redundantes o cíclicas

print(arcs)
# Grafico 
write.dot(maxmin_parents_childrens, file = "~/cursos/mineria/lab4/hailfinder_paren.dot")

print(hill_clim)
print(maxmin_hill_clim)
print(maxmin_parents_childrens)


sc_maxmin_hill_clim <-score(maxmin_hill_clim, hailfinder) 
sc_maxmin_parents_childrens <- score(maxmin_parents_childrens,hailfinder)

print(sc_maxmin_hill_clim)
print(sc_maxmin_parents_childrens)
# ajustar
fittedbn <- bn.fit(hill_clim, data = hailfinder)
print(fittedbn$Grunting)


################################################################
################### Evaluación con datos de prueba #############
################################################################
# Definir listas blanca y negra
whitelist <- data.frame(
  from = c("AreaMesoALS", "AreaMesoALS", "AreaMesoALS", 
           "ScnRelPlFcst", "CapInScen", "CurPropConv"),
  to = c("AreaMoDryAir", "CldShadeOth", "CompPlFcst",
         "PlainsFcst", "PlainsFcst", "PlainsFcst")
)
blacklist <- data.frame(
  from = c("PlainsFcst", "PlainsFcst", "N34StarFcst", "N34StarFcst", "R5Fcst", "R5Fcst"),
  to = c("LatestCIN", "LLIW", "LatestCIN", "LLIW", "LatestCIN", "LLIW")
)

# Aplicar el algoritmo Hill-Climbing
hill_clim <- hc(hailfinder_train, whitelist = whitelist, blacklist = blacklist)

# Ajustar la red bayesiana
fitted_bn <- bn.fit(hill_clim, data = hailfinder_train)

# Imprimir estructura aprendida y puntaje
print(hill_clim)
sc <- score(hill_clim, hailfinder_train) # BIC por defecto
cat("Puntaje del modelo (BIC):", sc, "\n")

# Calcular predicciones para los datos de prueba
predicciones <- predict(fitted_bn, node = "MountainFcst", data = hailfinder_test)

# Evaluar el desempeño de las predicciones
accuracy <- sum(predicciones == hailfinder_test$MountainFcst) / nrow(hailfinder_test)
cat("Precisión del modelo en el conjunto de prueba:", accuracy, "\n")

