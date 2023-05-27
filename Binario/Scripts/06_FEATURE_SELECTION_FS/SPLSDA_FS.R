
###  SECTION 6.D FEATURE SELECTION: SPLS-DA
### Descripción -------------------------------------------------------------

# 1 - Selección de predictores (genes) con SPLS-DA 
# 2 - Guardamos datasets train y test con los predictores seleccionados
# 3 - VIP predictores detection. Plots.


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
          #"patchwork", # easy organization of plots
          "ggpubr", # Publication ready plots
          "caret", # Machine Learning
          "stringr",# string manipulation
          "caret", # Machine Learning
          "stringr",# string manipulation
          "doParallel", # parallel computing
          "glmnet", #  regression models
          "tictoc", # time
          "plsVarSel", # PLS methods
          # "mdatools", # PLS methods
          "mixOmics", # PLS methods
          "spls" # PLS methods
          
))

### Importación datasets (procesados)----------------------------------------------

# train
traindf <- readRDS(here("Data/Merged_dataset/Processed/traindf_processed.rds")) %>% dplyr::select(-Sample) 
classes <- as.factor(traindf$Labels)

# Matriz de datos de entrenamiento
train_matrix <- as.matrix(traindf[,-1])

# test
testdf <- readRDS(here("Data/Merged_dataset/Processed/testdf_processed.rds")) %>% dplyr::select(-Sample) 
# Matriz de datos de test
test_matrix <- as.matrix(testdf[,-1])


classes_test <- as.factor(testdf$Labels)

### Feature Selection: SPLSDA ------------------------------------------------------------


# Cluster para parallel procesing
cl <- makeCluster(1)
registerDoParallel(cl)

# semilla reproducible
set.seed(1993)

# SPLSDA inicial ----------------------------------------------------------

#Marcamos 10 componentes aunque evaluaremos el número optimo posteriormente
splsda_inicial <- mixOmics::splsda(train_matrix, traindf[,1], ncomp = 10)  



# plot que traza las muestras proyectadas en los dos primeros componentes del subespacio PLS-DA
mixOmics::plotIndiv(splsda_inicial , comp = 1:2, 
          group = traindf[,1], ind.names = TRUE,  # colour points by class
          ellipse = TRUE, # include 95% confidence ellipse for each class
          legend = TRUE, title = '(a) PLSDA with confidence ellipses')

# usa la medida max.dist use la medida max.dist para formar límites de decisión entre clases según los datos de PLS-DA
background = background.predict(splsda_inicial, comp.predicted=2, dist = "max.dist")

# plot que traza las muestras proyectadas en los dos primeros componentes del subespacio PLS-DA
mixOmics::plotIndiv(splsda_inicial, comp = 1:2,
          group = traindf[,1], ind.names = TRUE, # colour points by class
          background = background, # include prediction background for each class
          legend = TRUE, title = " (b) PLSDA with prediction background")




# Tuning sPLS-DA  -----------------------

## Selecting the number of components

# Con la función perf realizamos una evalución de rendimiento para ajustar el número de componentes a utilizar
# Realizamos la optimización con 10 fold Cross validation con 5 repeticiones
perf_splsda_inicial <- perf(splsda_inicial, validation = "Mfold", 
                          folds = 10, nrepeat = 5, # use repeated cross-validation
                          progressBar = TRUE, auc = TRUE) # include AUC values


# Guardar gráfico como un archivo PNG
png(here("Plots", "perf_splsda_inicial_ncomp_plot.png"))

# plot resultado de la evaluación del desempeño en los diez componentes
# Se suele seleccionar el número de componentes que tenga la forma de "codo"
plot(perf_splsda_inicial, col = color.mixo(5:7), sd = TRUE,
     legend.position = "horizontal")


dev.off()



#cuál es el valor óptimo de los componentes según perf().
#Ejecuta pruebas t para una diferencia significativa en la tasa de error promedio entre los componentes
perf_splsda_inicial$choice.ncomp


## Selecting number of variables

# Para determinar el número de variables utilizadas para construir cada componente latente

# grid of possible keepX values that will be tested for each component
list.keepX <- c(1:10,  seq(20, 300, 10))

# Con la función perf realizamos una evalución de rendimiento para ajustar el número optimo de variables
# Realizamos la optimización con 10 fold Cross validation con 5 repeticiones
tune.splsda <- tune.splsda(train_matrix, traindf[,1], ncomp = 4, # calculate for first 4 components
                                 validation = 'Mfold',
                                 folds = 10, nrepeat = 5, # use repeated cross-validation
                                 dist = 'max.dist', # use max.dist measure
                                 measure = "BER", # use balanced error rate of dist measure
                                 test.keepX = list.keepX,
                                 cpus = 2) # allow for paralleliation to decrease runtime

# Guardar gráfico como un archivo PNG
png(here("Plots", "perftune_splsda_val_ncomp_plot.png"))

# plot output of variable number tuning
plot(tune.splsda, col = color.jet(4)) 

dev.off()

# what is the optimal value of components according to tune.splsda()
tune.splsda$choice.ncomp$ncomp


# what are the optimal values of variables according to tune.splsda()
tune.splsda$choice.keepX 
# Guardamos los valores optimos en variables
optimal.ncomp <- tune.splsda$choice.ncomp$ncomp
optimal.keepX <- tune.splsda$choice.keepX[1:optimal.ncomp]



# sPLS-DA modelo final ----------------------------------------------------

# Ajuste modelo final

# formar el modelo final con valores optimizados de numero de componentes y variables
final.splsda <-
  mixOmics::splsda(train_matrix, traindf[, 1], ncomp = optimal.ncomp,
                   keepX = optimal.keepX)

#Guardamos modelo
saveRDS(final.splsda, here("Models/FS","final.splsda.rds"))

#summary(final.splsda)

# Gráficos para visualizar 2D entre componentes
sPLSDA_plot12 <- plotIndiv(final.splsda, comp = c(1,2), # plot samples from final model
          group = traindf[,1], ind.names = FALSE, # colour by class label
          ellipse = TRUE, legend = TRUE, # include 95% confidence ellipse
          title = ' (a) sPLS-DA, comp 1 & 2')

#  Guardamos gráfico
ggsave(here("Plots", "sPLSDA_plot12.png"))



sPLSDA_plot13 <- plotIndiv(final.splsda, comp = c(1,3), # plot samples from final model
          group = traindf[,1], ind.names = FALSE,  # colour by class label
          ellipse = TRUE, legend = TRUE,
          # include 95% confidence ellipse
          title = '(b) sPLS-DA, comp 1 & 3')

# guardamos gráfico
ggsave(here("Plots", "sPLSDA_plot13.png"))

# sPLSDA_plot14 <- plotIndiv(final.splsda, comp = c(1,4), # plot samples from final model
#           group = traindf[,1], ind.names = TRUE,  # colour by class label
#           ellipse = TRUE, legend = FALSE, # include 95% confidence ellipse
#           title = '(c) sPLS-DA, comp 1 & 4')
# #guardamos gráfico
# ggsave(here("Plots", "sPLSDA_plot14.png"))
# 


# usa la medida max.dist use la medida max.dist para formar límites de decisión entre clases según los datos de PLS-DA
background = background.predict(final.splsda, comp.predicted=2, dist = "max.dist")

# plot que traza las muestras proyectadas en los dos primeros componentes del subespacio PLS-DA
sPLSDA_plot12_back <- mixOmics::plotIndiv(final.splsda, comp = 1:2,
                    group = traindf[,1], ind.names = TRUE, # colour points by class
                    background = background, # include prediction background for each class
                    legend = TRUE, title = " (b) PLSDA with prediction background")

#guardamos gráfico
ggsave(here("Plots", "sPLSDA_plot12_back.png"))



# Paramos los clusters de paralelización
stopCluster(cl)

# Salvamos modelos
#saveRDS(conf_lasso, here("Models/conf_matrices/conf_lasso_lasso.rds"))
saveRDS(final.splsda, here("Models/FS/final.splsda.rda"))

# extract the variables used to construct the firsts latent component
# La función selectVar() genera las variables seleccionadas para un componente dado y sus valores de carga (clasificados en valor absoluto decreciente).
PLS1 <- mixOmics::selectVar(final.splsda, comp = 1)$name 
PLS2 <- mixOmics::selectVar(final.splsda, comp = 2)$name 
PLS3 <- mixOmics::selectVar(final.splsda, comp = 3)$name 
#PLS4 <- mixOmics::selectVar(final.splsda, comp = 4)$name 
# vector con todas las variables
splsda_variables <- c(PLS1, PLS2, PLS3)

# Guardamos selección variables -------------------------------------------

# datasets train y test con las variables seleccionadas por splsda
datos_train_splsda <- traindf %>% dplyr::select(Labels, all_of(splsda_variables))
datos_test_splsda <-  testdf %>% dplyr::select(Labels, all_of(splsda_variables)) 

# salvamos datasets
saveRDS(datos_train_splsda, here("Data/FS_datasets/SPLSDA/","datos_train_splsda.rds"))
export(datos_train_splsda, here("Data/FS_datasets/SPLSDA/","datos_train_splsda.csv"))
saveRDS(datos_test_splsda, here("Data/FS_datasets/SPLSDA/","datos_test_splsda.rds"))
export(datos_test_splsda, here("Data/FS_datasets/SPLSDA/","datos_test_splsda.csv"))






# VIP Variable plots ------------------------------------------------------


# La estabilidad de una característica determinada se define como la proporción de folds en CV (entre repeticiones) en los que se seleccionó para ser utilizada para un componente determinado. 
# Los valores de estabilidad se pueden extraer a través de perf.splsda.srbct$features$stable. 
# Es probable que aquellos con la estabilidad más alta sean mucho más "importantes" para un componente dado. 
# Las características utilizadas para el primer componente tenían una estabilidad consistentemente más baja que los siguientes.


# Variable plots

# form new perf() object which utilises the final model
perf.splsda.final <- perf(final.splsda, 
                          folds = 10, nrepeat = 5, # use repeated cross-validation
                          validation = "Mfold", dist = "max.dist",  # use max.dist measure
                          progressBar = TRUE)

# Guardar gráfico como un archivo PNG
png(here("Plots", "feature_stability_splsda1.png"), width = 561, height = 500, res=150)
# plot the stability of each feature for the first three components, 'h' type refers to histogram
#
plot(perf.splsda.final$features$stable[[1]], type = 'h', 
     ylab = 'Stability', 
     xlab = '', 
     main = '(a) Stability of each feature Comp 1', las =3,
     cex.axis = 0.7)

dev.off()


png(here("Plots", "feature_stability_splsda2.png"), width = 561, height = 500, res=150)

plot(perf.splsda.final$features$stable[[2]], type = 'h', 
     ylab = 'Stability', 
     xlab = '', 
     main = '(b) Stability of each feature Comp 2', las =3,
     cex.axis = 0.7)

dev.off()

png(here("Plots", "feature_stability_splsda3.png"), width = 561, height = 500, res=150)

plot(perf.splsda.final$features$stable[[3]], type = 'h', 
     ylab = 'Stability', 
     xlab = '',
     main = '(c) stability of each feature Comp 3', las =2,
     cex.axis = 0.7)

dev.off()
# 
# png(here("Plots", "feature_stability_splsda4.png"), width = 561, height = 500, res=150)
# 
# plot(
#   perf.splsda.final$features$stable[[4]],
#   type = 'h',
#   ylab = 'Stability',
#   xlab = '',
#   main = '(d) stability of each feature Comp 4',
#   las = 3,
#   cex.axis = 0.6
# )
# 
# dev.off()


## Graficos top variables por componentes. Guardamos los gráficos

# Comp1
# Guardar gráfico como un archivo PNG
png(here("Plots", "top_splsda_variables1.png"))

# depict weight assigned to each of these variables
top5_splsda_variable <- mixOmics::plotLoadings(final.splsda, comp = 1, method = 'mean', contrib = 'max')

dev.off()

# comp 2
png(here("Plots", "top_splsda_variables2.png"))

# depict weight assigned to each of these variables
top5_splsda_variable <- mixOmics::plotLoadings(final.splsda, comp = 2, method = 'mean', contrib = 'max')

dev.off()

# comp 3
png(here("Plots", "top_splsda_variables3.png"))


# depict weight assigned to each of these variables
top5_splsda_variable <- mixOmics::plotLoadings(final.splsda, comp = 3, method = 'mean', contrib = 'max')

dev.off()

# # comp 4
# png(here("Plots", "top_splsda_variables4.png"))
# 
# # depict weight assigned to each of these variables
# top5_splsda_variable <- mixOmics::plotLoadings(final.splsda, comp = 4, method = 'mean', contrib = 'max')
# 
# dev.off()



# Predicciones ------------------------------------------------------------

