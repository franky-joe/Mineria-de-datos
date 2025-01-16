require(RTextTools)

# Cargar el archivo CSV con el corpus
corpus <- read.csv("~/Escritorio/taller3mineria/rt_reviews.csv")

#=============================================================#
#=================== Reducir cantidad ========================#
#=============================================================#

# Filtrar 24000 reseñas de cada clase
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

# Crear la matriz de términos usando las reseñas
matrix <- create_matrix(sampled_corpus$Review,
                        language = "english", 
                        removeNumbers = TRUE,
                        removeStopwords = TRUE,
                        toLower = TRUE,
                        #removeSparseTerms = 0.99,
                        stemWords = TRUE)  # Mantener términos no tan esparcidos

ncol(matrix)

#=============================================================#
#===== Extraer las 200 palabras más frecuentes ===============#
#=============================================================#

# Sumar las frecuencias de cada palabra
word_frequencies <- colSums(as.matrix(matrix))

# Ordenar palabras por frecuencia descendente
sorted_words <- sort(word_frequencies, decreasing = TRUE)

# Extraer las palabras más repetidas
top_words <- names(sorted_words)[1:400]

# Lista de palabras importantes que deseas añadir
important_words <- c("good", "captures", "quiet", "gripping", "intimate", "superb", 
                     "engrossing", "delightful", "poignant", "thoughtful", "wonderfully", 
                     "affecting", "riveting", "haunting", "friendship", "quietly", 
                     "vivid", "tense", "absorbing", "refreshing", "allows", "strength", 
                     "achievement", "thought-provoking", "exhilarating", "finest", 
                     "mature", "gentle", "incredible", "triumph", "captivating", 
                     "try", "brilliantly", "bleak", "reminder", "timely", "universal", 
                     "visceral", "chilling", "heartbreaking", "funniest", "gem", 
                     "inspiring", "unsettling", "unusual", "sensitive", "understated", 
                     "astonishing", "nuanced", "melancholy", "outstanding", 
                     "suspenseful", "doc", "carries", "exquisite", "tender", 
                     "blast", "challenging", "meditation", "nonetheless", "poetic", 
                     "vital", "brave", "colorful", "exploration", "unfortunately", 
                     "flat", "tedious", "bland", "sadly", "unfunny", "lazy", "mediocre", 
                     "tired", "waste", "pointless", "uninspired", "lame", "bore", 
                     "repetitive", "poorly", "sandler", "badly", "hollow", "pretentious", 
                     "annoying", "offensive", "superficial", "bloated", "clumsy", 
                     "unnecessary", "sitcom", "sloppy", "wasted", "convoluted", 
                     "incoherent", "tiresome", "excuse", "unless", "stale", "fail", 
                     "misfire", "underwhelming", "clunky", "confused", "confusing", 
                     "crude", "muddled", "parody", "inert", "trouble", "desperately", 
                     "soap", "unpleasant", "sitting", "disjointed", "powerful", 
                     "lifeless", "depressing", "misguided", "stereotypes", "misses", 
                     "simplistic", "apparently", "appear", "terribly", "dreary", "horrible")


# Combinar la lista de palabras importantes con las top palabras más repetidas
combined_words <- unique(c(important_words, top_words))

# Filtrar la matriz de términos para usar solo las palabras combinadas
filtered_matrix <- as.matrix(matrix)[, colnames(matrix) %in% combined_words]

ncol(filtered_matrix)
#=============================================================#
#================== Verificar documentos vacíos ==============#
#=============================================================#

# Identificar las filas (documentos) vacías, es decir, aquellas con solo ceros
empty_docs <- rowSums(filtered_matrix) == 0

# Contar cuántos documentos quedaron vacíos
num_empty_docs <- sum(empty_docs)
cat("Número de documentos vacíos:", num_empty_docs, "\n")

# Eliminar los documentos vacíos de la matriz y del corpus original
filtered_matrix <- filtered_matrix[!empty_docs, ]
sampled_corpus <- sampled_corpus[!empty_docs, ]

# Verificar el nuevo tamaño de la matriz después de eliminar documentos vacíos
cat("Número de documentos después de eliminar vacíos:", nrow(filtered_matrix), "\n")


#=============================================================#
#=================== Crear el contenedor =====================#
#=============================================================#

# Definir el porcentaje para el conjunto de entrenamiento
train_percentage <- 0.20

# Calcular el tamaño del conjunto de entrenamiento y prueba
train_size <- floor(nrow(sampled_corpus) * train_percentage)
test_size <- nrow(sampled_corpus) - train_size

# Crear el contenedor con el conjunto de entrenamiento y prueba
container <- create_container(filtered_matrix, sampled_corpus$Freshness, 
                              trainSize = 1:train_size, 
                              testSize = (train_size + 1):nrow(sampled_corpus), 
                              virgin = TRUE)

#=============================================================#
#=================== Entrenar los modelos ====================#
#=============================================================#

# Registrar el tiempo de inicio
start_time <- Sys.time()

# Entrenar los modelos
models <- train_models(container, algorithms = c("RF", "SLDA"))

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

# Mostrar el número de columnas (número de términos seleccionados)
ncol(filtered_matrix)

