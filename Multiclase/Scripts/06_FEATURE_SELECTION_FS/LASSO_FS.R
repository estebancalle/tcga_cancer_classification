

###  SECTION 6.B FEATURE SELECTION: Multinomial logistic regression with sparse group lasso penalty 

### Descripción -------------------------------------------------------------

# 1 - Selección de predictores (genes) con Multinomial logistic regression with sparse group lasso penalty 
# 2 - Guardamos datasets train y test con los predictores seleccionados
# 3 - Predicciones clasificación


## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 6.B FEATURE SELECTION: Multinomial logistic regression with sparse group lasso penalty      
#

### Instalación y carga de librerías ---------------------------------------


## Require carga los paquetes de CRAN y los instala si faltan


if (!require("Require")) {install.packages("Require")}

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
          "Boruta", # Boruta fs
          "msgl" #  Multinomial Sparse Group Lasso
          
))

### Importación data set train (procesado)----------------------------------------------

# train
traindf <- readRDS(here("Data/Merged_dataset/Processed/traindf_processed.rds")) %>% dplyr::select(-Sample) 
classes <- as.factor(traindf$Labels)

## Reestructuración datos
# Matriz de datos de entrenamiento
train_matrix <- traindf[,-1]


# Tabla de clases
table(classes)

# test
testdf <- readRDS(here("Data/Merged_dataset/Processed/testdf_processed.rds")) %>% dplyr::select(-Sample) 

# Vector con las clases test
classes_test <- as.factor(testdf$Labels)

# Matriz de datos de test
test_matrix <- as.matrix(testdf[,-1])

# Tabla de clases
table(classes_test)





### Feature Selection: Lasso ------------------------------------------------------------


# Cluster para parallel procesing
cl <- makeCluster(2)
registerDoParallel(cl)

#  Ajustamos modelo con cross validation
# Choose lambda (fraction of lambda.max) and alpha, with alpha = 1 for lasso, alpha = 0 for group lasso and alpha in the range (0,1) for sparse group lasso.
# Use msgl::cv to estimate the error for each lambda in a sequence decreasing from the data derived lambda.max to lambda * lambda.max. Lambda.max is the lambda at which the first penalized parameter becomes non-zero. 
set.seed(1993)
fit.cv <- msgl::cv(train_matrix, classes, fold = 10, alpha = 0.5, lambda = 0.1, use_parallel = TRUE)

# Paramos los clusters de paralelización
stopCluster(cl)

# Mostramos resultado del ajuste
fit.cv

# Guardamos la info del resultado
cat(capture.output(print(fit.cv)), file = here("Info_outputs/","info_fs_cv_lasso.txt"))

# Ajustamos modelo final
fit <- msgl::fit(train_matrix, classes, alpha = 0.5, lambda = 0.1)
fit


#Guardamos modelo
saveRDS(fit, here("Models/FS","final_lasso.rds"))


# Non-zero features in best model. -1 para quitar el intercepto
sel_lasso_features <- features(fit)[[best_model(fit.cv)]][-1] # Non-zero features in best model
length(sel_lasso_features)

# datasets train y test con las variables seleccionadas por lasso
datos_train_lasso <- traindf %>% dplyr::select(Labels, all_of(sel_lasso_features))
datos_test_lasso <-  testdf %>% dplyr::select(Labels, all_of(sel_lasso_features)) 

# salvamos datasets con las variables seleccionadas
saveRDS(datos_train_lasso, here("Data/FS_datasets/LASSO/","datos_train_lasso.rds"))
export(datos_train_lasso, here("Data/FS_datasets/LASSO/","datos_train_lasso.csv"))
saveRDS(datos_test_lasso, here("Data/FS_datasets/LASSO/","datos_test_lasso.rds"))
export(datos_test_lasso, here("Data/FS_datasets/LASSO/","datos_test_lasso.csv"))



# Predicciones modelo clasificación ---------------------------------------

# Predicciones en test

pred_lasso <- predict(fit, test_matrix)

pred_lasso$classes[,best_model(fit.cv)] # Classes predicted by best model
pred_lasso_lasso <- as.factor(pred_lasso$classes[,best_model(fit.cv)])

(conf_lasso<- confusionMatrix(as.factor(pred_lasso$classes[,best_model(fit.cv)]), classes_test))


# Salvamos modelos
saveRDS(conf_lasso, here("Models/conf_matrices/conf_lasso_lasso.rds"))
#saveRDS(fit, here("Models/lasso_lasso.rda"))
saveRDS(fit.cv, here("Models/FS/cv_lasso_lasso.rda"))
saveRDS(pred_lasso_lasso, here("Models/Predictions/pred_lasso_lasso.rds"))
