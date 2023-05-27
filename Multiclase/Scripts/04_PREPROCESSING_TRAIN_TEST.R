###     SECTION 4 PREPROCESSING TRAIN AND TEST DATA     

### Descripción -------------------------------------------------------------

# 1 - Preprocesamiento del dataset train.
# 2 - Se realiza filtrado genes irrelevantes -(quantil < 0.25)
# 3 - Identificación Y filtrado  de predictores (genes) de varianza cercana a cero
# 3 - Normalización: Center y scale transforman los datos y asegurar que las variables tengan la misma escala y media cero.
# 4 - Aplicamos el preprocesamiento a Test en base a los parámetros del preprocesamiento de train.

## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 4 PREPROCESSING TRAIN AND TEST DATA   
#

### Instalación y carga de librerías ---------------------------------------


## Paquetes de bioconductor. Se instalan si faltan
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("TCGAbiolinks"))
  BiocManager::install("TCGAbiolinks")

if (!require("SummarizedExperiment"))
  BiocManager::install(SummarizedExperiment)

if (!require("EDASeq"))
  BiocManager::install("EDASeq")

if (!require("genefilter"))
  BiocManager::install("genefilter")


## Require carga los paquetes de CRAN y los instala si faltan

if (!require("Require")) {install.packages("Require")}

Require(c("here",  # rutas fáciles y reproducibles
          "glue", # f-strings
          "rio", # fácil importación y exportación
          "dplyr", # pipes
          "tibble", # Tibbles functionality
          "purrr", # functional programing
          "tidyr", # tidy data tools
          "ggeasy", # utils for ggplot
          "ggpubr", # Publication ready plots
          "caret", # Machine Learning
          "stringr" # string manipulation
))




### Importación data set train (bruto)----------------------------------------------

datos_train_preprocessed <- import(here("Data/Merged_dataset/Preprocessed","datos_train_preprocessed.csv"), setclass = "data.frame", rownames = FALSE)


# dimensiones dataset train
message(
  glue(
    "El dataset combinado preprocesado tiene {dim(datos_train_preprocessed)[1]} filas (Muestras) y {dim(datos_train_preprocessed)[2] - 2} columnas (genes)."
  )
)



### Filtrado genes irrelevantes -(Q < 0.25) a nivel INTERESPECIFICO -------------------------------------------------

# Las funciones de TCGA Biolinks admiten los datos con los genes como filas y las muestras como columnas.
# Reestructuramos el data frame para convertirlo con el formato deseado. Separamos por tipo de tumor, Transponemos,
# aplicamos EL filtrado y trasponemos para que los genes sean de nuevo columnas.


# Definición de la función de reestructuración y filtrado
preprocess_data <- function(data) {
  
  # Preparamiento input
  data_filtered <- data %>% as.data.frame() %>% column_to_rownames(var = "Sample")  
 
  # Extracción etiquetas
  data_labels <-  data_filtered %>%
    dplyr::select(Labels) %>% as_tibble(rownames = NA) %>% pull()
  
  # transponer la matriz de datos de expresión
  data_t <- data_filtered %>%
    select(-Labels) %>%
    t()  # %>% as.data.frame()
  
  # FILTRADO: con el paquete "geneFilter"a través de una función TCGAanalyze_Filtering().
  # para filtrar los genes irrelevantes y devolvió los genes con una intensidad media en las muestras superior a 0,25, que era la media del cuantil definido por el umbral
  # plicar  filtering
  data_filtered_qc <- data_t %>% TCGAbiolinks::TCGAanalyze_Filtering(method = "quantile", qnt.cut =  0.25) 
  
  
  # transponer la matriz de datos. Incluir etiquetas.
  data_filtered_qc_t <- data_filtered_qc %>%  t() %>% as_tibble(rownames = NA) %>% 
    rownames_to_column() %>% dplyr::rename(Sample = rowname) %>% 
    add_column(Labels = data_labels, .after = 1)
  
  # Vector genes seleccionados que pasan el filtro
  genes_filter_selected <- data_filtered_qc_t %>% select(-c(Sample, Labels)) %>% colnames()
  
  # Exportamos en csv la matriz filtrada
  export(data_filtered_qc_t, here("Data", "Merged_dataset", "Processed", "train_qcfiltered.csv"))
  
  # Para guardar el objeto en formato .rds
  saveRDS(genes_filter_selected, here("Data/Merged_dataset/Processed","genes_filter_selected.rds"))
  
  # Pista de seguimiento
  print(glue("Filtrado QC completado"))
  

}


# Aplicamos función a los datos train para preprocesarlos
preprocess_data(datos_train_preprocessed)

# Importamos datos train filtrados
datos_train_qcfiltered <- import(here("Data/Merged_dataset/Processed","train_qcfiltered.csv"), setclass = "data.frame", rownames = FALSE)


# dimensiones dataset combinado y resultado filtrado qc
cat(message(
  glue(
    "El dataset combinado preprocesado tiene {dim(datos_train_preprocessed)[1]} filas (Muestras) y {dim(datos_train_preprocessed)[2] - 2} columnas (genes). \n
    El dataset combinado preprocesado (Normalización y filtrado intraespecifico) tiene {dim(datos_train_qcfiltered)[1]} filas (Muestras) y {dim(datos_train_qcfiltered)[2] - 2} columnas (genes). \n
    Se han filtrado: {dim(datos_train_preprocessed)[2] - dim(datos_train_qcfiltered)[2] - 2}")
), file = here("Info_outputs/","info_filtrado_train_preprocesing.txt"))

# Guardamos el aviso en texto en la carpeta info
cat(
  glue(
    "El dataset combinado preprocesado tiene {dim(datos_train_preprocessed)[1]} filas (Muestras) y {dim(datos_train_preprocessed)[2] - 2} columnas (genes). \n
    El dataset combinado preprocesado (Normalización y filtrado intraespecifico) tiene {dim(datos_train_qcfiltered)[1]} filas (Muestras) y {dim(datos_train_qcfiltered)[2] - 2} columnas (genes). \n
    Se han filtrado: {dim(datos_train_preprocessed)[2] - dim(datos_train_qcfiltered)[2] - 2}"
  ),
  file = here("Info_outputs/", "info_filtradoqc_train_preprocesing.txt")
)

# Detección de valores NA. si hay alguna fila (Muestras) incompleta.

# A nivel de fila
if (any(!complete.cases(datos_train_qcfiltered))) {
  message("Se detectan valores incompletos en las filas del dataset.")
} else {
  message("No se detectan valores incompletos en las filas del dataset.")
}

#  A nivel de columna
na_por_columna <- map_dbl(.x = datos_train_qcfiltered, .f = function(x){sum(is.na(x))})
if (any(na_por_columna > 0)) {
  message("Se detectan valores incompletos en las columnas del dataset.")
} else {
  message("No se detectan valores incompletos en las columnas del dataset.")
}


### Identificación Y filtrado  de predictores (genes) de varianza cercana a cero ----------------------

# Identificamos lor predictores
nzv_preproces  <- datos_train_qcfiltered %>% dplyr::select(-c(Sample, Labels)) %>%  preProcess(method = c("nzv"))

# seleccionamos en el dataset los predictores identificados
traindf <- predict(nzv_preproces, newdata = datos_train_qcfiltered,  keep = "Sample, Labels")


### Normalización: Center y scale --------

# Normalización: Center y scale transforman los datos y asegurar que las variables tengan la misma escala y media cero.

# Guardamos los parámetros de la normalización en el traindf.
scale_preproces  <- traindf %>% dplyr::select(-c(Sample, Labels)) %>%  preProcess(method = c("center", "scale"))

# Guardamos parametros preprocesing
saveRDS(scale_preproces,here("Data/Merged_dataset/Processed/scale_preproces_parameters.rds")) 

# Aplicamos la normalización a traindf
traindf_scaled <- predict(scale_preproces, newdata = traindf,  keep = "Sample, Labels")


message(
  glue(
    "El dataset bruto de entrenamiento tiene {dim(datos_train_preprocessed)[1]} filas (Muestras) y {dim(datos_train_preprocessed)[2] - 2} columnas (genes). \n
    El dataset preprocesado por el filtrado qc tiene {dim(datos_train_qcfiltered)[1]} filas (Muestras) y {dim(datos_train_qcfiltered)[2] - 2} columnas (genes). \n
    Se han filtrado: {dim(datos_train_preprocessed)[2] - dim(datos_train_qcfiltered)[2] - 2} \n
    El dataset de entrenamiento tras el filtrado de varianza tiene {dim(traindf_scaled)[1]} filas (Muestras) y {dim(traindf_scaled)[2] - 2} columnas (genes). \n
    Se han filtrado por near zero varianza: {((dim(datos_train_qcfiltered)[2] - 2) - (dim(traindf_scaled)[2] - 2))} genes \n
    En el preprocesamiento se han filtrado en total: {(dim(datos_train_preprocessed)[2] - dim(datos_train_qcfiltered)[2] - 2) + (dim(datos_train_qcfiltered)[2] - 2 - dim(traindf_scaled)[2] - 2)} genes \n")
)

# Guardamos el aviso en texto en la carpeta info
cat(
  glue(
    "El dataset bruto de entrenamiento tiene {dim(datos_train_preprocessed)[1]} filas (Muestras) y {dim(datos_train_preprocessed)[2] - 2} columnas (genes). \n
    El dataset preprocesado por el filtrado qc tiene {dim(datos_train_qcfiltered)[1]} filas (Muestras) y {dim(datos_train_qcfiltered)[2] - 2} columnas (genes). \n
    Se han filtrado: {dim(datos_train_preprocessed)[2] - dim(datos_train_qcfiltered)[2] - 2} \n
    El dataset de entrenamiento tras el filtrado de varianza tiene {dim(traindf_scaled)[1]} filas (Muestras) y {dim(traindf_scaled)[2] - 2} columnas (genes). \n
    Se han filtrado por near zero varianza: {((dim(datos_train_qcfiltered)[2] - 2) - (dim(traindf_scaled)[2] - 2))} genes \n
    En el preprocesamiento se han filtrado en total: {(dim(datos_train_preprocessed)[2] - dim(datos_train_qcfiltered)[2] - 2) + (dim(datos_train_qcfiltered)[2] - 2 - dim(traindf_scaled)[2] - 2)} genes \n"
  ),
  file = here("Info_outputs/", "info_total_train_preprocesing.txt")
)

# Exportamos procesado para el csv  de entrenamiento
export(traindf_scaled, here("Data/Merged_dataset/Processed","traindf_processed.csv"))



# Para guardar el objeto en formato .rds
saveRDS(traindf_scaled, here("Data/Merged_dataset/Processed","traindf_processed.rds"))




# ### Test preprocesing ---------------------------------------------------

# importamos datos test
datos_test_preprocessed <- import(here("Data/Merged_dataset/Preprocessed","datos_test_preprocessed.csv"), setclass = "data.frame", rownames = FALSE)


# dimensiones dataset test
message(
  glue(
    "El dataset combinado preprocesado tiene {dim(datos_test_preprocessed)[1]} filas (Muestras) y {dim(datos_test_preprocessed)[2] - 2} columnas (genes)."
  )
)

# filtramos mismos genes que fueron filtrados en train por el filtro qc 0.25
genes_filter_selected <- colnames(traindf_scaled)

datos_test_filtered <-  datos_test_preprocessed %>% dplyr::select(c(Sample, Labels, all_of(genes_filter_selected)))


# Aplicamos a test near zero var, scale y center
testdf <- predict(nzv_preproces, newdata = datos_test_filtered,  keep = "Sample, Labels")
testdf_scaled <- predict(scale_preproces, newdata = testdf,  keep = "Sample, Labels")



message(
  glue(
    "En el preprocesamiento se han filtrado en total: {(dim(datos_test_preprocessed)[2] - dim(datos_test_filtered)[2] - 2) + (dim(datos_test_filtered)[2] - 2 - dim(testdf_scaled)[2] - 2)} genes \n
    El dataset de test tras el filtrado de varianza tiene {dim(testdf_scaled)[1]} filas (Muestras) y {dim(testdf_scaled)[2] - 2} columnas (genes) \n
    El dataset de entrenamiento tras el filtrado de varianza tiene {dim(traindf_scaled)[1]} filas (Muestras) y {dim(traindf_scaled)[2] - 2} columnas (genes). \n"
  )
)


# Guardamos el aviso en texto en la carpeta info
cat(
  glue(
    "En el preprocesamiento se han filtrado en total: {(dim(datos_test_preprocessed)[2] - dim(datos_test_filtered)[2] - 2) + (dim(datos_test_filtered)[2] - 2 - dim(testdf_scaled)[2] - 2)} genes \n
    El dataset de test tras el filtrado de varianza tiene {dim(testdf_scaled)[1]} filas (Muestras) y {dim(testdf_scaled)[2] - 2} columnas (genes) \n
    El dataset de entrenamiento tras el filtrado de varianza tiene {dim(traindf_scaled)[1]} filas (Muestras) y {dim(traindf_scaled)[2] - 2} columnas (genes). \n"
  ),
  file = here("Info_outputs/", "info_total_test_preprocesing.txt")
)


# Exportamos procesado para el csv  de entrenamiento
export(testdf_scaled, here("Data/Merged_dataset/Processed","testdf_processed.csv"))

# Para guardar el objeto en formato .rds
saveRDS(testdf_scaled, here("Data/Merged_dataset/Processed","testdf_processed.rds"))



### Utilidades extra ----------------------------------------------------

# TCGAanalyze_Preprocessing() Permite encontrar muestras con baja correlación
# que pueden ser identificadas como outliers. Aplicamos la función a nivel de tipo de Cáncer.
#  A partir del umbral  que marquemos para el coeficiente de spearman
# se filtraran las muestras si detectamos la  presencia de outliers
# No hemos detectado. Si se hubiese detectado hubiéramos filtrado la muestra en posteriormente la matrix train.

# directorio con rda
# dir_path <- here::here("D:/TFM/Data/All_datasets/rda/")
# 
# # Obtener la lista de archivos .rda en el directorio
# file_names <- list.files(path = dir_path, pattern = "\\.rda$", full.names = TRUE)
# 
# # Función para aplicar TCGAanalyze_Preprocessing a cada archivo rda
# preprocess_file <- function(file) {
#   load(file)
#   data <- data
#   
#   print(str_extract(file, "(?<=/rda/)\\w+(?=_rnaseq_SE.rda)"))
#   TCGAanalyze_Preprocessing(object = data, cor.cut = 0.6)
# }
# 
# # Aplicar la función a cada archivo y almacenar los resultados en una lista
# map(file_names, preprocess_file)



