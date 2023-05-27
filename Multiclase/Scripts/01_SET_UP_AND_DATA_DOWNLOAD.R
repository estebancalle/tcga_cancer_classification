                                                                    
###     SECTION 1 SET UP, DATA INPUT TCGA DATA, REESTRUCTURATION        

### Descripción -------------------------------------------------------------

# 1 - Preparación entorno de trabajo. 
# 2- Descarga con TCGA biolinks datos RNAseq de distintos tipos de Cáncer del repositorio TCGA.
# 3 - Reestructuración y guardado de los los datos en combinado y separado en csv y rds.
# 4 - Sampleo aleatorio y sin reemplazamiento muestras del dataset combinado


## ***Notas: 

# 1- Abre el proyecto por el archivo "TFM.Rproj". Después ejecuta los sripts.

# 2- Para ser correctamente ejecutado en windows, la carpeta del proyecto debe estar en una carpeta raíz.
# TCGAbiolinks extrae los datos de distintas muestras cuyos nombres son muy largos.
# Windows tiene una limitación de 260 carácteres para nombres de carpeta. 
# Para evitar errores por sobrepasar el limite, ejecuta por ejemplo desde en "D:/TCGA".

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 1 SET UP, DATA INPUT TCGA DATA, REESTRUCTURATION 
#


### Instalación y carga de librerías ---------------------------------------

## Paquetes de bioconductor y los instala si faltan
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("TCGAbiolinks"))
  BiocManager::install("TCGAbiolinks")

if (!require("SummarizedExperiment"))
  BiocManager::install(SummarizedExperiment)

## Require carga los paquetes de CRAN y los instala si faltan
if (!require("Require")) {install.packages("Require")}

# Llama a las librerías, sino están, las instala
Require(c("here",  # rutas fáciles y reproducibles
          "glue", # f-strings
          "rio", # fácil importación y exportación
          "dplyr", # pipes
          "tibble", # Tibbles functionality
          "purrr" # functional programing
))

### Creación directorios ----------------------------------------------------
# Creación de carpetas en el directorio de trabajo para guardar los datos descargados

#Directorio de trabajo
cat(glue("El directorio de trabajo es {here::here()}"))

# Creación de subdirectorios
if (!dir.exists(here("Data"))) {
  dir.create(here("Data", "All_datasets", "csv"), recursive = TRUE)
  dir.create(here("Data", "All_datasets", "rda"), recursive = TRUE)
  dir.create(here("Data", "Merged_dataset", "Preprocessed"), recursive = TRUE)
  dir.create(here("Data", "Merged_dataset", "Processed"), recursive = TRUE)
  dir.create(here("Data", "FS_datasets", "BORUTA"), recursive = TRUE)
  dir.create(here("Data", "FS_datasets", "PCA"), recursive = TRUE)
  dir.create(here("Data", "FS_datasets", "LASSO"), recursive = TRUE)
  dir.create(here("Data", "FS_datasets", "SPLSDA"), recursive = TRUE)

}

if (!dir.exists(here("Models"))) {
  dir.create(here("Models", "Predictions"), recursive = TRUE)
  dir.create(here("Models", "Classification"), recursive = TRUE)
  dir.create(here("Models", "conf_matrices"), recursive = TRUE)
  dir.create(here("Models", "FS"), recursive = TRUE)
}

if (!dir.exists(here("Plots"))) {
  dir.create(here("Plots"))
}
if (!dir.exists(here("Tables"))) {
  dir.create(here("Tables"))
}

if (!dir.exists(here("Info_outputs"))) {
  dir.create(here("Info_outputs"))
}

### Descarga RNASeq datasets Pancancer TCGA -----------------------------

## Lista de proyectos 32 Tipos
# projects <- c("ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", "GBM", "HNSC", "KICH",
#               "KIRC", "KIRP", "LGG", "LIHC", "LUAD", "LUSC", "MESO", "OV", "PAAD", "PCPG",
#               "PRAD", "READ", "SARC", "SKCM", "STAD", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM")



## Lista de  15 proyectos 
projects <-
  c(
    "BLCA",
    "BRCA",
    "CESC",
    "COAD",
    "HNSC",
    "KIRC",
    "KIRP",
    "LGG",
    "LIHC",
    "LUAD",
    "LUSC",
    "OV",
    "PRAD",
    "STAD",
    "THCA"
  )

## Función para bajar expresiones génicas RNA-seq de los distintos proyectos TCGA  
download_and_prepare <- function(project) {
  
  # query con los datos de interés que queremos extraer
  query <- GDCquery(project = glue("TCGA-{project}"), 
                    data.category = "Gene expression",
                    data.type = "Gene expression quantification",
                    experimental.strategy = "RNA-Seq",
                    platform = "Illumina HiSeq",
                    file.type = "results",
                    sample.type = "Primary Tumor",
                    legacy = TRUE)
  
  # Función que baja los datos en función de la query
  GDCdownload(query)
  
  #Creación SummarizedExperiment y guarda una copia en rds
  RnaseqSE <- GDCprepare(
    query,
    directory = here("GDCdata"),
    save = TRUE,
    save.filename = glue("{project}_rnaseq_SE.rda")
  )
  
  # Matriz de expresión
  Matrix <- assay(RnaseqSE, "raw_count")
  # Transponemos Matriz para que los genes sean las columnas. Incluimos columna con la etiqueta del proyecto.
  Matrixt <- Matrix %>% t() %>% as_tibble(rownames = NA) %>% 
    rownames_to_column() %>% rename(Sample = rowname) %>% 
    add_column(Labels = project, .after = 1)
  
  # Exportamos en csv la matriz transpuesta
  export(Matrixt, here("Data", "All_datasets", "csv", glue("{project}_Matrixt.csv")))
}


# Pasamos cada proyecto por la función
purrr::map(projects, download_and_prepare)

# Almacenamos en variable los archivos .rds guardados en la carpeta principal
archivos <- list.files(path = here(), pattern = "\\.rda$")

# Mover los archivos . rds a la subcarpeta "data"
for (archivo in archivos) {
  file.rename(here(archivo), here("Data/All_datasets/rda/", archivo))
}



###  Reestructuración datos. Merge dataset.-------------------------------------------------------


# Obtener la lista de todos los archivos CSV en la ruta especificada.
# Leer cada archivo CSV con map.
#Combinar todos los objetos en un solo conjunto de datos con bind rows.
ruta_csvs <- here("Data/All_datasets/csv/")

dataset_unificado <- list.files(path = ruta_csvs, pattern = "\\.csv") %>%
  map( ~ rio::import(file.path(ruta_csvs, .), setclass = "data.frame")) %>%
  bind_rows()

# Muestreo 1500 muestras aleatorias de cada factor sin reemplazamiento y convertir el resultado en un dataframe
set.seed(1993)
df_unficado_sampled <- dataset_unificado %>%
  group_by() %>%
  sample_n(size = 1500, replace = FALSE) %>%
  as.data.frame()


# Exportamos el csv  combinado
export(dataset_unificado, here("Data/Merged_dataset/Preprocessed","dataset_unificado.csv"))

# Para guardar el objeto en formato .rds
saveRDS(dataset_unificado, here("Data/Merged_dataset/Preprocessed","dataset_unificado.rds"))

# Exportamos el csv  combinado del df_sampled
export(df_unficado_sampled, here("Data/Merged_dataset/Preprocessed","df_unficado_sampled.csv"))

# Para guardar el objeto en formato .rds del df_sampled
saveRDS(df_unficado_sampled, here("Data/Merged_dataset/Preprocessed","df_unficado_sampled.rds"))

### Utilidades extra --------------------------------------------------------

# Código para devolver de vuelta el dataframe a matrix
# t3 <- dataset_unificado %>%
#   column_to_rownames(var = "Sample") %>%
#   select(-Labels) %>%
#   t()


