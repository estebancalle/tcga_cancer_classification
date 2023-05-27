


###     SECTION 6.A FEATURE SELECTION: BORUTA   

### Descripción -------------------------------------------------------------

# 1 - Selección de predictores (genes) con Boruta
# 2 - Guardamos datasets train y test con los predictores seleccionados



## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 6.A FEATURE SELECTION: BORUTA        
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
          "Boruta" # Boruta fs
          
))

### Importación data set train (procesado)----------------------------------------------
traindf <- readRDS(here("Data/Merged_dataset/Processed/traindf_processed.rds")) %>% dplyr::select(-Sample) 
traindf$Labels <- as.factor(traindf$Labels)

### Feature Selection: Boruta----------------------------------------------

tic() # Toma de tiempo

set.seed(1993) # semilla para generar numeros aleatorios reproducibles

# Cluster para parallel procesing
cl <- makeCluster(1)
registerDoParallel(cl)

# Boruta
boruta <-
  Boruta(Labels ~ .,
         data = traindf,
         doTrace = 2,
         maxRuns = 100) # Número de iteraciones

print(boruta)

# Paramos los clusters de paralelización
stopCluster(cl)

# Tentativa de arreglo provisional de Boruta
# La siguiente decisión sobre los atributos provisionales se basa en comparar
# la puntuación Z mediana de cada atributo con la puntuación Z mediana del
# mejor atributo de sombra. Los atributos que tengan una puntuación Z mediana
# mayor o igual que la del mejor atributo de sombra se clasificarán como 
# confirmados, mientras que los que tengan una puntuación Z mediana menor
# se clasificarán como rechazados.
final.boruta <- TentativeRoughFix(boruta)
print(final.boruta)

# Guardamos la info del resultado de Boruta
cat(capture.output(print(final.boruta)), file = here("Info_outputs/","info_final_boruta.txt"))

#Guardamos los modelos
saveRDS(final.boruta, here("Models/FS","final.boruta.rds"))
saveRDS(boruta, here("Models/FS","boruta.rds"))

# vector con los genes (predictores) seleccionados
sel <- getSelectedAttributes(final.boruta, withTentative = F)
sel_formula <- getNonRejectedFormula(final.boruta)

toc() # vemos tiempo transcurrido

# Extraemos el data set train final con los predictores seleccionados por boruta
boruta_sel_traindf_labels <- as.data.frame(traindf$Labels)
boruta_sel_traindf <- traindf[,sel]
datos_train_boruta <- cbind(boruta_sel_traindf_labels,boruta_sel_traindf)
datos_train_boruta <- datos_train_boruta %>% rename(Labels = "traindf$Labels")


# seleccionamos los predictores finales seleccionados por boruta en el data frame test
testdf <- readRDS(here("Data/Merged_dataset/Processed/testdf_processed.rds")) %>% dplyr::select(-Sample) 
testdf$Labels <- as.factor(testdf$Labels)
boruta_sel_testdf <- testdf[,sel]
datos_test_boruta <- cbind(testdf$Labels,boruta_sel_testdf)
datos_test_boruta <- datos_test_boruta %>% rename(Labels = "testdf$Labels")

# Guardamos Datasets train y test boruta
saveRDS(datos_train_boruta, here("Data/FS_datasets/BORUTA","datos_train_boruta.rds"))
export(datos_train_boruta, here("Data/FS_datasets/BORUTA","datos_train_boruta.csv"))

saveRDS(datos_test_boruta, here("Data/FS_datasets/BORUTA","datos_test_boruta.rds"))
export(datos_test_boruta, here("Data/FS_datasets/BORUTA","datos_test_boruta.csv"))









