###     SECTION 9 TEST DE GENERALIZACIÓN 2    

### Descripción -------------------------------------------------------------

# 1 - Test de generalización con nuevas muestras y el mejor modelo
# 2 - Generamos matriz de confusión y un gráfico



## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 5 CLUSTERING EXPLORATORIO      
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
          "MLmetrics", # ML meterics
          "naivebayes", # NB
          "kableExtra", # tables
          "gridExtra", # tables
          "qqplotr", # qqplotr extension
          "forcats", # factor manipulation
          "RColorBrewer", # color scales
          "openxlsx",
          "fmsb",
          "ggradar"# Excel 
          
          
))


# Cargamos dataset unificado menos las muestras sampleadas anteriormente


df_unificado <- readRDS(here("Data/Merged_dataset/Preprocessed/dataset_unificado_tumor.rds"))
df_sampleado <- readRDS(here("Data/Merged_dataset/Preprocessed/df_unficado_tumor_sampled.rds"))

# Eliminar muestras sampleadas del dataframe grande
df_2_test <- anti_join(df_unificado, df_sampleado, by = "Sample")

# Cargamos parametros preprocesamiento
preproces_parameters <- readRDS(here("Data/Merged_dataset/Processed/scale_preproces_parameters.rds")) 

##Aplicamos preprocesamiento
df_2_test_scaled <- predict(preproces_parameters, newdata = df_2_test)

testdf <- readRDS(here("Data/Merged_dataset/Processed/testdf_processed.rds"))

# Cargamos datos boruta para extraer los nombres de los genes.
datos_test_boruta <- readRDS(here("Data/FS_datasets/BORUTA","datos_test_boruta.rds"))

#Guardamos nombres de los genes seleccionados por boruta
names_borutadf <- names(datos_test_boruta)
saveRDS(names_borutadf, here("Data/Merged_dataset/Processed/names_borutadf.rds")) 

# seleccionamos los genes en el df
df_test2_generalizacion <-  df_2_test_scaled %>% dplyr::select(Sample,Class, all_of(names(datos_test_boruta)))

# Cargamos modelo svm R - BORUTA
modelo_svmr_boruta <- readRDS(here("Models/Classification/svmr_boruta.rda"))


# Predicciones
pred_svmr_boruta <- predict(modelo_svmr_boruta,newdata = df_test2_generalizacion, type = "raw")
# Predicciones
probs_svmr_boruta <- predict(modelo_svmr_boruta,newdata = df_test2_generalizacion, type = "prob")


# Matriz confusión
(conf_svmr_boruta<- confusionMatrix(pred_svmr_boruta, as.factor(df_test2_generalizacion$Class)))


# Gráfico matriz de confusión
plot_conf_rf_boruta <-
  # funcion  definida en el script 8, paraplot gráfico
  confusionMatrixPlot(MatrizConfusion = conf_rf_boruta, " Support Vector Machine Radial con Boruta (Test generalización 2)")

ggsave(here("Plots", "plot_conf_svmr_boruta_test2.png"))

# Tabla predicciones

resultados <- tibble(Fecha = Sys.Date(), Paciente =df_test2_generalizacion$Sample, Diagnostico = as.character(pred_rf_boruta), Probabilidad = apply(probs_rf_boruta, 1, max) )

#resultados <- as.data.frame(resultados)
# Para extraer un valor único de probabilidad
#probs_rf_boruta %>% slice(2) %>% pivot_longer(everything(), names_to = "Tumor", values_to = "Probabilidad") %>% slice_max(Probabilidad, n=1) %>% pull(Probabilidad) %>% round(3)

# Guardamos datos en distintos formatos


# Export csv

export(resultados, here("Tables/Resultado_test_generalizacion_2.csv"))

# Export EXCEL
wb <- createWorkbook()

# Agregar una hoja al libro
addWorksheet(wb, "Diagnostico")

# Escribir la tabla en la hoja
writeDataTable(wb, sheet = "Diagnostico", x = resultados)

# Guardar el libro de Excel
saveWorkbook(wb, here("Tables/Resultado_test_generalizacion_2.xlsx"), overwrite = TRUE)

