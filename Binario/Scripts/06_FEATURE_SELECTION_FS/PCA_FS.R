

###  SECTION 6.C FEATURE EXTRACTION: PCA

### Descripción -------------------------------------------------------------

# 1 - Estudio PCA: componentes necesarias, gráficos, importancia variables
# 2 - PCA 95% comp varianza. Extracción componentes principales
# 3 - Guardamos datasets train y test con los componentes generados



## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 6.C FEATURE EXTRACTION: PCA
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
          "msgl", #  Multinomial Sparse Group Lasso
          "FactoMineR", # multivariate Exploratory Data Analysis
          "factoextra" # Extract Visualize Results Multivariate Data Analyses
          
))


### Importación data set train (procesado)----------------------------------------------

# train y test
traindf <- readRDS(here("Data/Merged_dataset/Processed/traindf_processed.rds")) %>% dplyr::select(-Sample) 
classes <- as.factor(traindf$Labels)


testdf <- readRDS(here("Data/Merged_dataset/Processed/testdf_processed.rds")) %>% dplyr::select(-Sample) 


### Estudio PCA -------------------------------------------------------------

#pca
genes_pca <- FactoMineR::PCA(traindf[,-1], scale.unit = TRUE, graph = FALSE,)
print(genes_pca)


# eigenvalues
eig.val <- get_eigenvalue(genes_pca)

# Componentes según criterio de Kaiser
sum(eig.val > 1)
# 652

# componentes según screeplot
fviz_screeplot_PCA<- fviz_screeplot(genes_pca, addlabels = TRUE)
# 6 dimensiones

# Guardar el gráfico como un archivo PNG
png(here("Plots", "fviz_screeplot_PCA.png"))
print(fviz_screeplot_PCA)
dev.off()

# 36.4 varianza top 20 genes tres primeros pcas
fviz_contrib_PCA_dim1 <- fviz_contrib(genes_pca, choice = "var", axes = 1, top = 20)
fviz_contrib_PCA_dim2 <- fviz_contrib(genes_pca, choice = "var", axes = 2, top = 20)
# fviz_contrib_PCA_dim3 <- fviz_contrib(genes_pca, choice = "var", axes = 3, top = 20)


# Guardar gráficos como un archivo PNG
png(here("Plots", "fviz_contrib_PCA_dim1.png"))
print(fviz_contrib_PCA_dim1)
dev.off()

png(here("Plots", "fviz_contrib_PCA_dim2.png"))
print(fviz_contrib_PCA_dim2)
dev.off()


### PCA 95% comp varianza. Extracción componentes principales ---------------

# PCA conservando el 95% de la varianza
# Non-numeric data will not be pre-processed 
transformacion_pca <- preProcess(x = traindf, method = "pca", thresh = 0.95)

# Generamos los componentes en train y test
datos_train_pca <- predict(object = transformacion_pca, newdata = traindf)
datos_test_pca     <- predict(object = transformacion_pca, newdata = testdf)
toc()

#Guardamos los datasets
saveRDS(datos_train_pca, here("Data/FS_datasets/PCA/","datos_train_pca.rds"))
export(datos_train_pca, here("Data/FS_datasets/PCA/","datos_train_pca.csv"))
saveRDS(datos_test_pca, here("Data/FS_datasets/PCA/","datos_test_pca.rds"))
export(datos_test_pca, here("Data/FS_datasets/PCA/","datos_test_pca.csv"))



