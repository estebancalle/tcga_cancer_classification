
###     SECTION 7.A CLASSIFICATION MODEL: KNN 

### Descripción -------------------------------------------------------------

# 1 - Entrenamientos modelos KNN en base a las selecciones FS (SECTION 6).
# 2 - Generamos predicciones



## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 7.A CLASSIFICATION MODEL: KNN      
#

### Instalación y carga de librerías ---------------------------------------

if (!require("Require")) {install.packages("Require")}


## Require carga los paquetes de CRAN y los instala si faltan
Require(c("here",  # rutas fáciles y reproducibles
          "glue", # f-strings
          "rio", # fácil importación y exportación
          "dplyr", # pipes
          "tibble", # Tibbles functionality
          "purrr", # functional programing
          "tidyr", # tidy data tools
          "ggeasy", # utils for ggplot,
          "ggpubr", # Publication ready plots
          "caret", # Machine Learning
          "stringr",# string manipulation
          "doParallel", # parallel computing
          "glmnet", #  regression models
          "tictoc", # time
          "MLmetrics" # ML metrics
          
))




# Función para entrenar  modelo KNN

KNN_model_func <- function(traindf) {
  

# Variables Cross Validation
particiones  <- 10
repeticiones <- 5



# Hiperparámetros
hiperparametros <- data.frame(k = c(1, 2, 7, 10, 17, 19,20,21))

# Generar semillas para CV

# Iniciamos una lista vacía que almacenará las semillas de aleatoriedad con las
# que trabajaremos. Haremos 10 folds * 5 repeticiones = lista con 50 bolsillos +
# 1 para el modelo final (Lo pone en la documentación de ?trainControl() ).
# Debemos llenar todos los bolsillos con vectores con 50 numeros cada uno, menos
# en el último bolsillo de la lista semillas, que debe ser un único número.

semillas <- vector(mode = "list", length = (particiones*repeticiones) +1)  


# Llenamos la lista/vector con semillas al azar, elegidas entre 1 y 1000.
# Creamos, por tanto, una lista de listas. Todos los bolsillos de la lista,
# menos el último, tendrán vectores con 16 números de longitud 
#
# ejemplo: (10 folds * 5 repeticiones) + 1 evaluación final = 51
# Hemos usado lapply porque queremos que nos devuelva una lista/vector

semillas <- lapply(semillas, function(x) sample.int(1000, size=nrow(hiperparametros)))

semillas[[(particiones*repeticiones) +1]] <- sample.int(1000, size= 1)


#str(semillas)


tic() # Inicio conteo tiempo

# Definir control de entrenamiento con validación cruzada repetida con SMOTE y semillas generadas

control <- trainControl(method = "repeatedcv", # Método de validación cruzada
                        number = particiones, # Número de particiones
                        repeats = repeticiones, # Número de repeticiones
                        sampling = "smote", # Método de sobremuestreo (SMOTE)
                        classProbs = TRUE, # Usar probabilidades de clase para predicciones
                        summaryFunction = twoClassSummary, # Métricas de evaluación para clases multiclase
                        seeds = semillas,
                        #summaryFunction = multiClassSummary, 
                        verboseIter = T,
                        #summaryFunction = defaultSummary,
                        # selectionFunction = "best", 
                        # savePredictions = "final",
                        returnResamp = "final",
                        allowParallel = TRUE) # Semillas para reproducibilidad


# Clusters para procesamiento paralelo
cl = makePSOCKcluster(1)
registerDoParallel(cl)

# Ajustamos modelo

set.seed(1993)

modelo_knn <- train(Labels ~ ., data = traindf, 
                method = "knn",
                trControl = control, 
                tuneGrid = hiperparametros, 
                metric = "ROC", # Incluir métricas de evaluación
                maximize = TRUE) # Indicar que se debe maximizar la métrica MCC

# finalizamos paralelización
stopCluster(cl)

modelo_knn                

toc() # fin conteo tiempo

# Visualizamos optimización hiperparámetros
ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy en función de K", x = "K") +
  theme_bw() +
  geom_line(color="blue")

# La función devuelve el modelo
return(modelo_knn)
}


### KNN - BORUTA --------------------------------------------------------
# importación datasets FS: PCA
traindfBoruta <- readRDS(here("Data/FS_datasets/BORUTA/datos_train_boruta.rds"))
testdfBoruta <- readRDS(here("Data/FS_datasets/BORUTA/datos_test_boruta.rds"))

# Entrenamos knn
modelo_knn_boruta <- KNN_model_func(traindfBoruta)

# Guardamos la info del resultado
cat(capture.output(print(modelo_knn_boruta)), file = here("Info_outputs/","modelo_knn_boruta.txt"))

# Predicciones
pred_knn_boruta <- predict(modelo_knn_boruta,newdata = testdfBoruta, type = "raw")

# Probabilidades Predicciones
prob_knn_boruta <- predict(modelo_knn_boruta,newdata = testdfBoruta, type = "prob")

# Matriz confusión
(conf_knn_boruta<- confusionMatrix(pred_knn_boruta, testdfBoruta$Labels, positive= "Tumor"))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_knn_boruta, here("Models/conf_matrices/conf_knn_boruta.rds"))
saveRDS(modelo_knn_boruta, here("Models/Classification/knn_boruta.rda"))
saveRDS(pred_knn_boruta, here("Models/Predictions/pred_knn_boruta.rds"))
saveRDS(prob_knn_boruta, here("Models/Predictions/prob_knn_boruta.rds"))
### KNN - PCA ---------------------------------------------------------------

# importación datasets FS: PCA
traindfPCA <- readRDS(here("Data/FS_datasets/PCA/datos_train_pca.rds"))
testdfPCA <- readRDS(here("Data/FS_datasets/PCA/datos_test_pca.rds"))
traindfPCA$Labels <- as.factor(traindfPCA$Labels)
testdfPCA$Labels <- as.factor(testdfPCA$Labels)


# Entrenamos knn
modelo_knn_pca <-  KNN_model_func(traindfPCA)

# Guardamos la info del resultado
cat(capture.output(print(modelo_knn_pca)), file = here("Info_outputs/","modelo_knn_pca.txt"))

# Predicciones
pred_knn_pca <- predict(modelo_knn_pca,newdata = testdfPCA, type = "raw")

# Probabilidades Predicciones
prob_knn_pca <- predict(modelo_knn_pca,newdata = testdfPCA, type = "prob")

# Matriz confusión
(conf_knn_pca<- confusionMatrix(pred_knn_pca, testdfPCA$Labels, positive= "Tumor"))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_knn_pca, here("Models/conf_matrices/conf_knn_pca.rds"))
saveRDS(modelo_knn_pca, here("Models/Classification/knn_pca.rda"))
saveRDS(pred_knn_pca, here("Models/Predictions/pred_knn_pca.rds"))
saveRDS(prob_knn_pca, here("Models/Predictions/prob_knn_pca.rds"))

### KNN - LASSO -------------------------------------------------------------

# Importación datasets FS: LASSO
traindflasso <- readRDS(here("Data/FS_datasets/LASSO/datos_train_lasso.rds"))
testdflasso <- readRDS(here("Data/FS_datasets/LASSO/datos_test_lasso.rds"))
traindflasso$Labels <- as.factor(traindflasso$Labels)
testdflasso$Labels <- as.factor(testdflasso$Labels)

# Entrenamos knn
modelo_knn_lasso <-  KNN_model_func(traindflasso)

# Guardamos la info del resultado
cat(capture.output(print(modelo_knn_lasso)), file = here("Info_outputs/","modelo_knn_lasso.txt"))

# Predicciones
pred_knn_lasso <- predict(modelo_knn_lasso,newdata = testdflasso, type = "raw")

# Probabilidades Predicciones
prob_knn_lasso <- predict(modelo_knn_lasso,newdata = testdflasso, type = "prob")

# Matriz confusión
(conf_knn_lasso<- confusionMatrix(pred_knn_lasso, testdflasso$Labels, positive= "Tumor"))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_knn_lasso, here("Models/conf_matrices/conf_knn_lasso.rds"))
saveRDS(modelo_knn_lasso, here("Models/Classification/knn_lasso.rda"))
saveRDS(pred_knn_lasso, here("Models/Predictions/pred_knn_lasso.rds"))
saveRDS(prob_knn_lasso, here("Models/Predictions/prob_knn_lasso.rds"))

#### KNN - sPLS-DA -----------------------------------------------------------

# importación datasets FS: sPLS-DA
traindfSPLSDA <- readRDS(here("Data/FS_datasets/SPLSDA/datos_train_SPLSDA.rds"))
testdfSPLSDA <- readRDS(here("Data/FS_datasets/SPLSDA/datos_test_SPLSDA.rds"))
traindfSPLSDA$Labels <- as.factor(traindfSPLSDA$Labels)
testdfSPLSDA$Labels <- as.factor(testdfSPLSDA$Labels)

# Entrenamos knn
modelo_knn_SPLSDA <-  KNN_model_func(traindfSPLSDA)

# Guardamos la info del resultado
cat(capture.output(print(modelo_knn_SPLSDA)), file = here("Info_outputs/","modelo_knn_splsda.txt"))

# Predicciones
pred_knn_SPLSDA <- predict(modelo_knn_SPLSDA,newdata = testdfSPLSDA, type = "raw")

# Probabilidades Predicciones
prob_knn_SPLSDA <- predict(modelo_knn_SPLSDA,newdata = testdfSPLSDA, type = "prob")

# Matriz confusión
(conf_knn_SPLSDA<- confusionMatrix(pred_knn_SPLSDA, testdfSPLSDA$Labels, positive= "Tumor"))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_knn_SPLSDA, here("Models/conf_matrices/conf_knn_splsda.rds"))
saveRDS(modelo_knn_SPLSDA, here("Models/Classification/knn_splsda.rda"))
saveRDS(pred_knn_SPLSDA, here("Models/Predictions/pred_knn_splsda.rds"))
saveRDS(prob_knn_SPLSDA, here("Models/Predictions/prob_knn_SPLSDA.rds"))
