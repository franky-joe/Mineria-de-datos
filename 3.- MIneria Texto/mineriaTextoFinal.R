
require(RTextTools)
# Cargar el archivo CSV con el corpus
corpus <- read.csv("~/Escritorio/taller3mineria/rt_reviews.csv")

#=============================================================#
#=================== Reducir cantidad ========================#
#=============================================================#

# Filtrar 2400 reseñas de cada clase
fresh_samples <- corpus[corpus$Freshness == "fresh", ][sample(1:240000, 15000), ]
rotten_samples <- corpus[corpus$Freshness == "rotten", ][sample(1:240000, 15000), ]

# Combinar las muestras en un nuevo dataframe
sampled_corpus <- rbind(fresh_samples, rotten_samples)

# Barajar el conjunto de datos muestreados
set.seed(123)  # Para reproducibilidad
sampled_corpus <- sampled_corpus[sample(nrow(sampled_corpus)), ]

# Mostrar la cantidad de cada clase en el nuevo conjunto de datos
table(sampled_corpus$Freshness)

#=============================================================#
#=================== Crear la matriz de términos =============#
#=============================================================#

# Crear la matriz de términos usando las reseñas (columna "Review")
matrix <- create_matrix(sampled_corpus$Review,
                        language="english", 
                        removeNumbers=TRUE,        # Elimina los números del texto.
                        removeStopwords=TRUE,      # Elimina palabras comunes (stopwords) como "the", "and", "of".
                        toLower=TRUE,              # Convierte todo el texto a minúsculas para evitar duplicados por mayúsculas/minúsculas.
                        stemWords=TRUE,            # Reduce las palabras a su raíz (stemming), es decir, "running" se convierte en "run".
                        removeSparseTerms=.99)    # Elimina términos que aparecen en menos del 0.3% de los documentos (0.997 especifica la "sparsity").

ncol(matrix)

#=============================================================#
#=================== Crear el contenedor =====================#
#=============================================================#

# Definir el porcentaje para el conjunto de entrenamiento (ej. 80%)
train_percentage <- 0.20

# Calcular el tamaño del conjunto de entrenamiento y prueba
train_size <- floor(nrow(sampled_corpus) * train_percentage)
test_size <- nrow(sampled_corpus) - train_size

# Ver los tamaños calculados
print(paste("Tamaño del conjunto de entrenamiento:", train_size))
print(paste("Tamaño del conjunto de prueba:", test_size))

# Crear el contenedor con el conjunto de entrenamiento y prueba en base al porcentaje
container <- create_container(matrix, sampled_corpus$Freshness, 
                              trainSize=1:train_size, testSize=(train_size+1):nrow(sampled_corpus), 
                              virgin=TRUE)

#=============================================================#
#=================== Entrenar los modelos ====================#
#=============================================================#

# Registrar el tiempo de inicio
start_time <- Sys.time()

# Entrenar los modelos
models <- train_models(container, algorithms=c("RF", "SLDA"))

# Registrar el tiempo de fin
end_time <- Sys.time()

# Calcular la duración
duration <- end_time - start_time
print(as.numeric(duration, units = "mins"))

# Clasificar usando los modelos entrenados
results <- classify_models(container, models)

#=============================================================#
#=================== Analizar resultados =====================#
#=============================================================#

# Crear análisis de precisión, recall y F1
precision_recall_f1 <- create_precisionRecallSummary(container, results)

# Mostrar precisión, recall y F1 para cada modelo
print(precision_recall_f1)
ncol(matrix)
