#install.packages("doParallel") 
#install.packages("foreach") 
#install.packages("iterators") 
#install.packages("parallel") 
require(doParallel)
require(e1071)


######################
#### PARTE 0  ########
######################


datos=read.csv("/home/roko/cursos/mineria/lab6/G3_002.csv")
print(colnames(datos))
# Get summary statistics
summary(datos)

# Histogram of PAM
hist(datos$PAM, main="Histograma  PAM", xlab="PAM", col="blue")

# Histogram of VFSC
hist(datos$VFSC, main="Histograma VFSC", xlab="VFSC", col="green")

# Scatter plot between PAM and VFSC
plot(datos$PAM, datos$VFSC, main="Scatter Plot of PAM vs VFSC", xlab="PAM", ylab="VFSC", pch=19, col="red")

######################
#### PARTE 1  ########
######################

Ts=0.2
Tiempo=seq(Ts,(length(datos$VFSC))*Ts,Ts)

model <- svm(datos$VFSC ~ datos$PAM , datos)

VFSC_estimated <- predict(model, datos$PAM)
eficiencia<-cor(VFSC_estimated,datos$VFSC,method = "pearson")

plot(Tiempo,datos$VFSC,type="l")
lines(Tiempo,VFSC_estimated, col = "red")
legend("topright", c("VFSC","VFSC_estimated"), title = paste("Corr=",round(eficiencia,digits=5)), pch = 1, col=c("blue","red"),lty=c(2,1),inset = 0.01)



######################
#### PARTE 2  ########
######################

tuneResult <- tune(svm,datos$VFSC ~ datos$PAM,  data = datos,
                   ranges = list(nu = seq(0.1,0.2,0.1), cost = 2^(-4:2), type="nu-regression")
)

tunedModel <- tuneResult$best.model
VFSC_tunedModel <- predict(tunedModel, datos$PAM)
eficienciaTuned<-cor(VFSC_tunedModel,datos$VFSC,method = "pearson")

plot(Tiempo,datos$VFSC,type="l")
lines(Tiempo,VFSC_estimated, col = "red")
lines(Tiempo,VFSC_tunedModel, col = "blue")
legend("topright", c("VFSC",paste("VFSC_estimated corr=",round(eficiencia,5)),paste("VFSC_Tuned corr",round(eficienciaTuned,5))), title = "Correlacion", pch = 1, col=c("blue","red"),lty=c(2,1),inset = 0.01)



######################
#### PARTE 3  ########
######################
retardos_multi <- function(signalData,lags)
{
  
  signal.uni <- signalData
  max.lag <- max(unlist(lags)) + 1
  indices <- 1:nrow(signal.uni)
  lag.mat <- embed(indices, max.lag)
  
  col.names <- list("PAMn","VFSCn")
  columns <- NULL
  lagged.columns.names <- c()
  for(colname in col.names){
    
    lag.order <- lags[[colname]]
    columns[[colname]] <- signal.uni[lag.mat[, 1], colname]
    if(!is.null(lag.order) && lag.order > 0)
      for(i in 1:lag.order){
        new.colname <- paste(colname, paste0("lag", i), sep = ".")
        lagged.columns.names <- c(lagged.columns.names, new.colname)
        columns[[new.colname]] <- signal.uni[lag.mat[, i+1], colname]
      }
  }
  folded.signal <- data.frame(columns)
  sorting <- order(lag.mat[, 1])
  folded.signal <- folded.signal[sorting, ]
  list(folded.signal = folded.signal, lagged.columns.names = lagged.columns.names)
}

PAMn<-(datos$PAM-min(datos$PAM))/(max(datos$PAM)-min(datos$PAM))
VFSCn<-(datos$VFSC-min(datos$VFSC))/(max(datos$VFSC)-min(datos$VFSC))
data <- data.frame(PAMn, VFSCn) #Datos ya normalizado

dataPAMn <- data['PAMn']
dataVFSCn <- data['VFSCn']


##EJECUTAR INICIO
# Definir el grid de parámetros para la optimización
cost <- 2^seq(-14, 20, 1)
nu <- seq(0.1, 0.9, 0.1)
gamma <- 2^seq(-16, 16 , 2)
lagsList <- seq(1, 6, 1)
parms <- expand.grid(lagsList=lagsList, cost=cost, nu=nu, gamma=gamma)
registerDoParallel(cores = 15)

midpoint <- nrow(data) / 2
dataA <- data[1:midpoint, , drop=FALSE]
dataB <- data[(midpoint + 1):nrow(data), , drop=FALSE]


salida <- (c( foreach(i = 1:nrow(parms),  combine = rbind, .inorder = FALSE) %dopar% {
  c <- parms[i, ]$cost
  n <- parms[i, ]$nu
  g <- parms[i, ]$gamma
  l <- parms[i, ]$lagsList
  lag<-list(PAMn = l,VFSCn = 0)
  signal.train <- retardos_multi(dataA, lag)
  retDatos=signal.train$folded.signal
  x=subset(retDatos, select = -VFSCn)
  y=retDatos$VFSCn
  modelo <- e1071::svm(x, y, type = "nu-regression", kernel = "radial", cost = c, nu = n, gamma=g)
  
  predTrain <- predict(modelo, x) 
  corrTrain <- cor(predTrain,y,method = "pearson")
  
  signal.test <- retardos_multi(dataB, lag)
  xTest <- subset(signal.test$folded.signal,select= -VFSCn)
  yTest <- signal.test$folded.signal$VFSCn
  
  predTest <- predict(modelo, xTest)
  corrTest <- cor(predTest, yTest, method= "pearson")
  c(l, c, n, g, corrTrain, corrTest)
}))

froutput <- matrix(unlist(salida), ncol = 6, byrow = TRUE)

mejoresModelos<-output[order(output[,6], decreasing = TRUE),]
# restardo, cost, nu, gamma, correlacionTrain, correlacionTest 
print(mejoresModelos)
#EJECUTAR FINAL


salida2 <- (c( foreach(i = 1:nrow(parms),  combine = rbind, .inorder = FALSE) %dopar% {
  c <- parms[i, ]$cost
  n <- parms[i, ]$nu
  g <- parms[i, ]$gamma
  l <- parms[i, ]$lagsList
  lag<-list(PAMn = l,VFSCn = 0)
  signal.train <- retardos_multi(dataB, lag)
  retDatos=signal.train$folded.signal
  x=subset(retDatos, select = -VFSCn)
  y=retDatos$VFSCn
  modelo <- e1071::svm(x, y, type = "nu-regression", kernel = "radial", cost = c, nu = n, gamma=g)
  
  predTrain <- predict(modelo, x) 
  corrTrain <- cor(predTrain,y,method = "pearson")
  
  signal.test <- retardos_multi(dataA, lag)
  xTest <- subset(signal.test$folded.signal,select= -VFSCn)
  yTest <- signal.test$folded.signal$VFSCn
  
  predTest <- predict(modelo, xTest)
  corrTest <- cor(predTest, yTest, method= "pearson")
  c(l, c, n, g, corrTrain, corrTest)
}))

output2 <- matrix(unlist(salida2), ncol = 6, byrow = TRUE)
mejoresModelos2<-output2[order(output2[,6], decreasing = TRUE),]
print(mejoresModelos2)

######################
#### PARTE 4  ########
######################

# Parámetros del modelo 2
# 1,2,3,  --- 33--
restardo <- 4
cost <- 32
nu <- 0.5
gamma <- 2.441406e-04

# Generar señal de prueba: escalón en PAMn
escalon <- c(rep(1, 100), rep(0, 100))
PAMn_escalon <- data.frame(PAMn = escalon)

# Función para generar retardos
lags <- list(PAMn = restardo, VFSCn = 0)

escalon_lagged <- retardos_multi(PAMn_escalon, lags)  # Crear retardos para el escalón
signal.train <- retardos_multi(data, lags)  # Generar retardos para el conjunto de datos de entrenamiento
retDatos <- signal.train$folded.signal  # Extraer datos procesados

# Crear variables de entrenamiento x (características) y y (etiquetas)
x <- subset(retDatos, select = -VFSCn)  # Extraer las características
y <- retDatos$VFSCn  # Etiquetas para el entrenamiento

# Crear el modelo SVM con los mejores parámetros
modelo_final <- e1071::svm(x, y, type = "nu-regression", kernel = "radial", cost = cost, nu = nu, gamma = gamma)

# Predecir con el escalón ajustado
x_escalon <- subset(escalon_lagged$folded.signal, select = colnames(x))  # Coincidir columnas del modelo
pred_escalon <- predict(modelo_final, x_escalon)

# Crear un tiempo ajustado con la longitud de la señal de predicción
tiempo_escalon <- seq(1, length(pred_escalon)) * Ts

# Recortar las señales para evitar problemas de longitud
escalon_recortado <- escalon[(restardo + 1):length(escalon)]

# Graficar resultados ajustados
plot(tiempo_escalon, escalon_recortado, type = "l", col = "red", ylim = c(0, 1), ylab = "Respuesta", xlab = "Tiempo")
lines(tiempo_escalon, pred_escalon, col = "blue")
legend("topright", legend = c("Escalón PAM", "Respuesta VFSC"), col = c("red", "blue"), lty = 1)

# Graficar zoom
plot(
  tiempo_escalon, escalon_recortado, 
  type = "l", col = "red", 
  ylim = c(-0.5, 1), xlim = c(18, 25), 
  ylab = "Respuesta", xlab = "Tiempo (Segundos)"
)
lines(tiempo_escalon, pred_escalon, col = "blue")
legend("topright", legend = c("Escalón PAM", "Respuesta VFSC"), col = c("red", "blue"), lty = 1)

