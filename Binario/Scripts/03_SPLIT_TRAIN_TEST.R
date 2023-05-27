

###     SECTION 3 TRAIN AND TEST SPLIT       

### Descripción -------------------------------------------------------------

# 1 - Partición estratificada de datos en 80% datos train, 20 % datos test. 
# 2 - Guardar datos train y test.
# 3 - Tabla de proporciones.
# 4 - Detección "Accuracy basal" clase mayoritaria como predictor

## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 3 TRAIN AND TEST SPLIT   
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
          "janitor"
))


### Cargar datos y mostrar dimensiones --------------------------------------

#Directorio de trabajo
cat(glue("El directorio de trabajo es {here::here()}"))

#Cargar el df normal y tumor combinado
datos_preprocessed <- readRDS(here("Data/Merged_dataset/Preprocessed/df_tumor_and_normal.rds"))


### Train, test split ---------------------------------------------------

# semilla replicación
set.seed(1993)

# Elección aleatorea índices de las observaciones de entrenamiento. Se realiza de forma estratificada en función de la proporción de las clases.
train <- createDataPartition(y = datos_preprocessed$Labels, p = 0.8, list = FALSE, times = 1)
datos_train_preprocessed <- datos_preprocessed[train, ]
datos_test_preprocessed  <- datos_preprocessed[-train, ]

datos_train_preprocessed <- datos_train_preprocessed %>% dplyr::select(-Labels) %>% rename(Labels = Class)
datos_test_preprocessed <- datos_test_preprocessed %>% dplyr::select(-Labels) %>% rename(Labels = Class)
# Para guardar los objetos en formato .rds y .csv
saveRDS(datos_train_preprocessed, here("Data/Merged_dataset/Preprocessed","datos_train_preprocessed.rds"))
saveRDS(datos_test_preprocessed, here("Data/Merged_dataset/Preprocessed","datos_test_preprocessed.rds"))
export(datos_train_preprocessed, here("Data/Merged_dataset/Preprocessed","datos_train_preprocessed.csv"))
export(datos_test_preprocessed, here("Data/Merged_dataset/Preprocessed","datos_test_preprocessed.csv"))



### Tabla proporciones train vs test ------------------------------------------------------
datos_train <- readRDS(here("Data/Merged_dataset/Preprocessed","datos_train_preprocessed.rds"))
datos_test <- readRDS(here("Data/Merged_dataset/Preprocessed","datos_test_preprocessed.rds"))

distribucion_train <- prop.table(table(datos_train_preprocessed$Labels)) %>% round(3)
distribucion_test  <- prop.table(table(datos_test_preprocessed$Labels)) %>% round(3)
Tabla_distribucion_completa <- data.frame(train = distribucion_train, test = distribucion_test )

export(Tabla_distribucion_completa, here("Tables", glue("Tabla_distribucion_completa.csv")))



### Accuracy Basal ------------------------------------------------------

# Detección "Accuracy basal" a partir de la clase mayoritaria como predictor
# "Accuracy basal": porcentaje de aciertos (accuracy) que deben superar los modelos predictivos  para considerarse útiles en algún grado.
mean(datos_train_preprocessed$Labels == "Tumor")

# Guardamos la info del resultado
cat(capture.output(print(mean(datos_train_preprocessed$Labels == "Tumor"))), file = here("Info_outputs/","Accuracy basal.txt"))
