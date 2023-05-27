

###     SECTION 7.E CLASSIFICATION MODEL: NAIVE BAYES

### Descripción -------------------------------------------------------------

# 1 - Entrenamientos modelos NAIVE BAYES en base a las selecciones FS (SECTION 6).
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
# Script Name: SECTION 7.D CLASSIFICATION MODEL: SVM RADIAL     
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
          "MLmetrics", # ML metrics
          "naivebayes" # Naive Bayes
          
))

# Función para entrenar  modelo SVML

NB_model_func <- function(traindf) {
  
  
  # Variables Cross Validation
  particiones  <- 10
  repeticiones <- 5
  
  
  
  # Hiperparámetros
  hiperparametros <- data.frame(laplace = c(0,1),
                                usekernel = FALSE,
                                adjust = 1)
  
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
                          summaryFunction = defaultSummary, # Métricas de evaluación para clases multiclase
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
  
  
  set.seed(1993)
  modelo_NB <- train(Labels ~ ., data = traindf, 
                       method = "naive_bayes",
                       trControl = control, 
                       tuneGrid = hiperparametros, 
                       metric = "Kappa", # Incluir métricas de evaluación
                       maximize = TRUE) # Indicar que se debe maximizar la métrica MCC
  
  # finalizamos paralelización
  stopCluster(cl)
  
  modelo_NB                
  
  toc() # fin conteo tiempo
  
  # Visualizamos optimización hiperparámetros
  ggplot(modelo_NB, highlight = TRUE) +
    scale_x_continuous(breaks = hiperparametros$k) +
    labs(title = "Evolución del accuracy en función de K", x = "K") +
    theme_bw() +
    geom_line(color="blue")
  
  # La función devuelve el modelo
  return(modelo_NB)
}


### NB - BORUTA --------------------------------------------------------
# importación datasets FS: PCA
traindfBoruta <- readRDS(here("Data/FS_datasets/BORUTA/datos_train_boruta.rds"))
testdfBoruta <- readRDS(here("Data/FS_datasets/BORUTA/datos_test_boruta.rds"))

# Entrenamos NB
modelo_nb_boruta <- NB_model_func(traindfBoruta)

# Guardamos la info del resultado
cat(capture.output(print(modelo_nb_boruta)), file = here("Info_outputs/","modelo_nb_boruta.txt"))

# Predicciones
pred_nb_boruta <- predict(modelo_nb_boruta,newdata = testdfBoruta, type = "raw")

# Matriz confusión
(conf_nb_boruta<- confusionMatrix(pred_nb_boruta, testdfBoruta$Labels))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_nb_boruta, here("Models/conf_matrices/conf_nb_boruta.rds"))
saveRDS(modelo_nb_boruta, here("Models/Classification/nb_boruta.rda"))
saveRDS(pred_nb_boruta, here("Models/Predictions/pred_nb_boruta.rds"))

### NB - PCA ---------------------------------------------------------------

# importación datasets FS: PCA
traindfPCA <- readRDS(here("Data/FS_datasets/PCA/datos_train_pca.rds"))
testdfPCA <- readRDS(here("Data/FS_datasets/PCA/datos_test_pca.rds"))
traindfPCA$Labels <- as.factor(traindfPCA$Labels)
testdfPCA$Labels <- as.factor(testdfPCA$Labels)


# Entrenamos NB
modelo_nb_pca <-  NB_model_func(traindfPCA)

# Guardamos la info del resultado
cat(capture.output(print(modelo_nb_pca)), file = here("Info_outputs/","modelo_nb_pca.txt"))

# Predicciones
pred_nb_pca <- predict(modelo_nb_pca,newdata = testdfPCA, type = "raw")

# Matriz confusión
(conf_nb_pca<- confusionMatrix(pred_nb_pca, testdfPCA$Labels))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_nb_pca, here("Models/conf_matrices/conf_nb_pca.rds"))
saveRDS(modelo_nb_pca, here("Models/Classification/nb_pca.rda"))
saveRDS(pred_nb_pca, here("Models/Predictions/pred_nb_pca.rds"))

### NB - LASSO -------------------------------------------------------------

# Importación datasets FS: LASSO
traindflasso <- readRDS(here("Data/FS_datasets/LASSO/datos_train_lasso.rds"))
testdflasso <- readRDS(here("Data/FS_datasets/LASSO/datos_test_lasso.rds"))
traindflasso$Labels <- as.factor(traindflasso$Labels)
testdflasso$Labels <- as.factor(testdflasso$Labels)

# Entrenamos nb
modelo_nb_lasso <-  NB_model_func(traindflasso)

# Guardamos la info del resultado
cat(capture.output(print(modelo_nb_lasso)), file = here("Info_outputs/","modelo_nb_lasso.txt"))

# Predicciones
pred_nb_lasso <- predict(modelo_nb_lasso,newdata = testdflasso, type = "raw")

# Matriz confusión
(conf_nb_lasso<- confusionMatrix(pred_nb_lasso, testdflasso$Labels))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_nb_lasso, here("Models/conf_matrices/conf_nb_lasso.rds"))
saveRDS(modelo_nb_lasso, here("Models/Classification/nb_lasso.rda"))
saveRDS(pred_nb_lasso, here("Models/Predictions/pred_nb_lasso.rds"))


#### NB - sPLS-DA -----------------------------------------------------------

# importación datasets FS: sPLS-DA
traindfSPLSDA <- readRDS(here("Data/FS_datasets/SPLSDA/datos_train_SPLSDA.rds"))
testdfSPLSDA <- readRDS(here("Data/FS_datasets/SPLSDA/datos_test_SPLSDA.rds"))
traindfSPLSDA$Labels <- as.factor(traindfSPLSDA$Labels)
testdfSPLSDA$Labels <- as.factor(testdfSPLSDA$Labels)

# Entrenamos NB
modelo_nb_SPLSDA <-  NB_model_func(traindfSPLSDA)

# Guardamos la info del resultado
cat(capture.output(print(modelo_nb_SPLSDA)), file = here("Info_outputs/","modelo_nb_splsda.txt"))

# Predicciones
pred_nb_SPLSDA <- predict(modelo_nb_SPLSDA,newdata = testdfSPLSDA, type = "raw")

# Matriz confusión
(conf_nb_SPLSDA<- confusionMatrix(pred_nb_SPLSDA, testdfSPLSDA$Labels))

# Guardamos modelo, matriz de confusión y predicciones
saveRDS(conf_nb_SPLSDA, here("Models/conf_matrices/conf_nb_splsda.rds"))
saveRDS(modelo_nb_SPLSDA, here("Models/Classification/nb_splsda.rda"))
saveRDS(pred_nb_SPLSDA, here("Models/Predictions/pred_nb_splsda.rds"))

