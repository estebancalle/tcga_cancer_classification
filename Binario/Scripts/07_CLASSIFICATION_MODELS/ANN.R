
###     SECTION 7.B CLASSIFICATION MODEL: ANN

### Descripción -------------------------------------------------------------

# 1 - Entrenamientos modelos de redes neuronales 1D con keras y tensorflow con las selecciones FS (SECTION 6).
# 2 - Generamos predicciones


## ***Notas: Debes crear un enviroment en miniconda sencillo con la instalación de Tensorflow y keras.
# Llama a dicho environment "tf"

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

Require(c("here",  # rutas fáciles y reproducibles
          "glue", # f-strings
          "rio", # fácil importación y exportación
          "dplyr", # pipes
          "tibble", # Tibbles functionality
          "purrr", # functional programing
          "tidyr", # tidy data tools
          "ggeasy", # utils for ggplot,
          #"patchwork", # easy organization of plots
          "ggpubr", # Publication ready plots
          "caret", # Machine Learning
          "stringr",# string manipulation
          "doParallel",
          "glmnet",
          "tictoc",
          "MLmetrics",
          "naivebayes",
          "reticulate",
          "keras"
          
))


# Usamos el env "tf" creado en miniconda donde hemos instalado tensorflow
use_condaenv("tf")


# IMPORTAMOS


ANN_model_func <- function(datos_train, datos_test) {

# convertimos a matriz el data frame
matrix_datos_train <- as.matrix(datos_train[-1])
matrix_datos_test <- as.matrix(datos_test[-1])

# Eliminamos nombres variables de la matriz
dimnames(matrix_datos_train) <- NULL
dimnames(matrix_datos_test) <- NULL


# Por otro lado, es necesario recodificar la variable con las etiquetas (labels) a formato numerico matricial. Como tenemos factores, transformamos a valor numérico los distintos niveles y le restamos 1 para que la cuenta de valores otorgados empiece en 0 en vez de en 1. Después con la función to_categorical de Keras convertiremos un vector de clase (enteros) en una matriz de clase binaria.
y_train <- as.matrix(as.numeric(datos_train$Labels) -1)
y_test <- as.matrix(as.numeric(datos_test$Labels) -1)
y_train <- to_categorical(y_train)
y_test <- to_categorical(y_test)


dim(matrix_datos_train)[1]
# Modelo secuencial: load
mod_keras <- keras_model_sequential()


# Añadiendo capas: configuración del modelo
mod_keras %>% 
  layer_dense(units = 400, activation = "relu",input_shape =c(dim(matrix_datos_train)[2])) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 200, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 100, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dense(units = 2, activation = "sigmoid")


# Resumen del modelo
summary(mod_keras)


# Compilando el modelo
mod_keras %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = "adam",
          metrics = "AUC")

# Semilla
set.seed(1993)
# Ajuste del modelo
mod_nnet <- mod_keras %>% 
  fit(matrix_datos_train,
      y_train,
      epochs = 50,
      batch_size = 128,
      validation_split = 0.2,
      verbose = TRUE,
      callbacks= list(callback_early_stopping(patience=10)))


mod_keras %>% 
  evaluate(matrix_datos_test,
           y_test)


#predicciones
y_pred <- mod_keras %>% predict(matrix_datos_test) %>% k_argmax() %>% as.integer()
y_probs <- mod_keras %>% predict(matrix_datos_test)
  #mod_keras %>% model.predict(matrix_datos_test)
#y_pred <- mod_keras %>% predict(matrix_datos_test) %>% `>`(0.5) %>% k_cast("int32") %>% as.integer()
#y_pred_probs <-  model.predict_proba(matrix_datos_test) %>% k_argmax() %>% as.integer()

# etiquetas actuales y predichas
y_pred.f<-as.factor(y_pred)
y_test.f<-as.factor(as.numeric(datos_test$Labels) -1)

# Pasamos a factor
y_pred.f <- factor(
  y_pred.f,
  levels = 0:1,
  labels = c(
    "Tumor",
    "Normal"
  )
)

y_test.f <- factor(
  y_test.f,
  levels = 0:1,
  labels = c(
    "Tumor",
    "Normal"
  )
)


return(list(mod_nnet = mod_nnet, y_pred.f = y_pred.f, y_test.f = y_test.f, y_probs = y_probs))
}



### ANN - BORUTA --------------------------------------------------------
# importación datasets FS: boruta
traindfBoruta <- readRDS(here("Data/FS_datasets/BORUTA/datos_train_boruta.rds"))
testdfBoruta <- readRDS(here("Data/FS_datasets/BORUTA/datos_test_boruta.rds"))

# Entrenamos Ann
resultado_ann_boruta <- ANN_model_func(traindfBoruta,testdfBoruta)
modelo_ann_boruta <- resultado_ann_boruta$mod_nnet

# Guardamos la info del modelo
cat(capture.output(print(modelo_ann_boruta)), file = here("Info_outputs/","modelo_ann_boruta.txt"))


# Guardar gráfico Entrenamiento
png(here("Plots", "train_modelo_ann_boruta.png"))
# plot the stability of each feature for the first three components, 'h' type refers to histogram
plot(modelo_ann_boruta)

dev.off()

# predicciones 
y_predf_boruta <- resultado_ann_boruta$y_pred.f

# probabilidades
options(scipen = 999)
probs_ann_boruta <- 100*(resultado_ann_boruta$y_probs)

# Guardamos predicciones para acceder a ellas más tarde
saveRDS(y_predf_boruta, here("Models/Predictions/y_predf_boruta.rds"))
saveRDS(probs_ann_boruta, here("Models/Predictions/probs_ann_boruta.rds"))

# probabilidades
 
#y_preprobsdf_boruta <- resultado_ann_boruta$y_pred_probs

# Guardamos predicciones para acceder a ellas más tarde
#saveRDS(y_preprobsdf_boruta, here("Models/Predictions/y_preprobsdf_boruta.rds"))

# labels verdaderas
y_test.f <- resultado_ann_boruta$y_test.f


# Matriz de confusion Ann_ boruta
(conf_ann_boruta <- confusionMatrix(y_predf_boruta,y_test.f, positive= "Tumor"))

# Guardamos la matriz y el modelo
saveRDS(conf_ann_boruta, here("Models/conf_matrices/conf_ann_boruta.rds"))
saveRDS(modelo_ann_boruta, here("Models/Classification/ann_boruta.rda"))


### ANN - PCA --------------------------------------------------------


# importación datasets FS: PCA
traindfPCA <- readRDS(here("Data/FS_datasets/PCA/datos_train_pca.rds"))
testdfPCA <- readRDS(here("Data/FS_datasets/PCA/datos_test_pca.rds"))
traindfPCA$Labels <- as.factor(traindfPCA$Labels)
testdfPCA$Labels <- as.factor(testdfPCA$Labels)

# Entrenamos Ann
resultado_ann_pca <- ANN_model_func(traindfPCA,testdfPCA)
modelo_ann_pca <- resultado_ann_pca$mod_nnet

# Guardamos la info del modelo
cat(capture.output(print(modelo_ann_pca)), file = here("Info_outputs/","modelo_ann_pca.txt"))


# Guardar gráfico Entrenamiento
png(here("Plots", "train_modelo_ann_pca.png"))
# plot the stability of each feature for the first three components, 'h' type refers to histogram
plot(modelo_ann_pca)

dev.off()

# predicciones 
y_predf_pca <- resultado_ann_pca$y_pred.f


# Probabilidades
probs_ann_pca <- 100*(resultado_ann_pca$y_probs)

# Guardamos predicciones para acceder a ellas más tarde
saveRDS(y_predf_pca, here("Models/Predictions/y_predf_pca.rds"))
saveRDS(probs_ann_pca, here("Models/Predictions/probs_ann_pca.rds"))


# labels verdaderas
y_test.f <- resultado_ann_pca$y_test.f


# Matriz de confusion Ann pca
(conf_ann_pca <- confusionMatrix(y_predf_pca,y_test.f, positive= "Tumor"))

# Guardamos la matriz y el modelo
saveRDS(conf_ann_pca, here("Models/conf_matrices/conf_ann_pca.rds"))
saveRDS(modelo_ann_pca, here("Models/Classification/ann_pca.rda"))


### ANN - LASSO --------------------------------------------------------

# Importación datasets FS: LASSO
traindflasso <- readRDS(here("Data/FS_datasets/LASSO/datos_train_lasso.rds"))
testdflasso <- readRDS(here("Data/FS_datasets/LASSO/datos_test_lasso.rds"))
traindflasso$Labels <- as.factor(traindflasso$Labels)
testdflasso$Labels <- as.factor(testdflasso$Labels)

# Entrenamos Ann
resultado_ann_lasso <- ANN_model_func(traindflasso,testdflasso)
modelo_ann_lasso <- resultado_ann_lasso$mod_nnet

# Guardamos la info del modelo
cat(capture.output(print(modelo_ann_lasso)), file = here("Info_outputs/","modelo_ann_lasso.txt"))


# Guardar gráfico Entrenamiento
png(here("Plots", "train_modelo_ann_lasso.png"))
# plot the stability of each feature for the first three components, 'h' type refers to histogram
plot(modelo_ann_lasso)

dev.off()

# predicciones 
y_predf_lasso <- resultado_ann_lasso$y_pred.f

# Probabilidades
probs_ann_lasso <- 100*(resultado_ann_lasso$y_probs)

# Guardamos predicciones para acceder a ellas más tarde
saveRDS(y_predf_lasso, here("Models/Predictions/y_predf_lasso.rds"))
saveRDS(probs_ann_lasso, here("Models/Predictions/probs_ann_lasso.rds"))

# labels verdaderas
y_test.f <- resultado_ann_lasso$y_test.f


# Matriz de confusion Ann lasso
(conf_ann_lasso <- confusionMatrix(y_predf_lasso,y_test.f, positive= "Tumor"))

# Guardamos la matriz y el modelo
saveRDS(conf_ann_lasso, here("Models/conf_matrices/conf_ann_lasso.rds"))
saveRDS(modelo_ann_lasso, here("Models/Classification/ann_lasso.rda"))

#### ANN - sPLS-DA -----------------------------------------------------------

# importación datasets FS: sPLS-DA
traindfSPLSDA <- readRDS(here("Data/FS_datasets/SPLSDA/datos_train_SPLSDA.rds"))
testdfSPLSDA <- readRDS(here("Data/FS_datasets/SPLSDA/datos_test_SPLSDA.rds"))
traindfSPLSDA$Labels <- as.factor(traindfSPLSDA$Labels)
testdfSPLSDA$Labels <- as.factor(testdfSPLSDA$Labels)


# Entrenamos Ann
resultado_ann_splsda <- ANN_model_func(traindfSPLSDA,testdfSPLSDA)
modelo_ann_splsda <- resultado_ann_splsda$mod_nnet

# Guardamos la info del modelo
cat(capture.output(print(modelo_ann_splsda)), file = here("Info_outputs/","modelo_ann_splsda.txt"))


# Guardar gráfico Entrenamiento
png(here("Plots", "train_modelo_ann_splsda.png"))
# plot the stability of each feature for the first three components, 'h' type refers to histogram
plot(modelo_ann_splsda)

dev.off()

# predicciones 
y_predf_splsda <- resultado_ann_splsda$y_pred.f

# Probabilidades
probs_ann_splsda <- 100*(resultado_ann_splsda$y_probs)

# Guardamos predicciones para acceder a ellas más tarde
saveRDS(y_predf_splsda, here("Models/Predictions/y_predf_splsda.rds"))
saveRDS(probs_ann_splsda, here("Models/Predictions/probs_ann_splsda.rds"))
# labels verdaderas
y_test.f <- resultado_ann_splsda$y_test.f


# Matriz de confusion Ann lasso
(conf_ann_splsda <- confusionMatrix(y_predf_splsda,y_test.f, positive= "Tumor"))

# Guardamos la matriz y el modelo
saveRDS(conf_ann_splsda, here("Models/conf_matrices/conf_ann_splsda.rds"))
saveRDS(modelo_ann_splsda, here("Models/Classification/ann_splsda.rda"))



