

###     SECTION 2 EDA PREPROCESSED MERGED DATA        

### Descripción -------------------------------------------------------------

# 1 - Tabla número de muestras por tumor. Total y sampleo. 
# 2 - Diagrama de barras número de muestras por tumor
# 3 - Detección valores ausentes
# 4 - Se guardan los archivos resultantes


## ***Notas: 

#
### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 2 EDA PREPROCESSED  MERGED DATA   
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

Require(c("here",  # rutas fáciles y reproducibles
          "glue", # f-strings
          "rio", # fácil importación y exportación
          "dplyr", # pipes
          "tibble", # Tibbles functionality
          "purrr", # functional programing
          "tidyr", # tidy data tools
          "ggeasy", # utils for ggplot
          "ggpubr" # Publication ready plots,
          
))


### Cargar datos --------------------------------------

#Directorio de trabajo
cat(glue("El directorio de trabajo es {here::here()}"))

#Cargar el objeto guardado como "datos_unificados.rds"
# datos_preprocessed <- readRDS(here("Data/Merged_dataset/Preprocessed","dataset_unificado.rds"))
df_unficado_sampled <- readRDS(here("Data/Merged_dataset/Preprocessed/df_tumor_and_normal.rds"))



#### Exploración datos ----------------------------------------------------

# dimensiones dataset combinado
message(
  glue(
    "El dataset combinado preprocesado tiene {dim(df_unficado_sampled)[1]} filas y {dim(df_unficado_sampled)[2]} columnas."
  )
)

#Cambiamos la variable Labels a factor en ambos datasets
df_unficado_sampled$Class <- as.factor(df_unficado_sampled$Class)


### Detección de datos faltantes ----------------------------------------


# Definir el nombre del archivo para guardar mensajes posteriores
sink(here("Info_outputs", "Valores_na.txt"))


# Detección si hay alguna fila (Muestras) incompleta.
if (any(!complete.cases(df_unficado_sampled))) {
  message("Se detectan valores incompletos en las filas del dataset merged.")
} else {
  message("No se detectan valores incompletos en las filas del dataset merged.")
}

# Detección si hay algun dato faltante en alguna columna
na_por_columna <-
  map_dbl(
    .x = df_unficado_sampled,
    .f = function(x) {
      sum(is.na(x))
    }
  )
if (any(na_por_columna > 0)) {
  message("Se detectan valores incompletos en las columnas del dataset merged.")
} else {
  message("No se detectan valores incompletos en las columnas del dataset merged.")
}


### Tabla número muestras por tipo de tumor -----------------------------------------

# # Tabla Número de muestras por tumor. Datos completos.
# tabla_num_tumor <-  datos_preprocessed %>% dplyr::select(Labels) %>%
#   mutate(Labels = factor(Labels)) %>%
#   group_by(Labels) %>%
#   dplyr::count()


# Tabla Número de muestras por tumor. Datos sampleados
tabla_num_tumor_sampled <-
  df_unficado_sampled %>% dplyr::select(Class) %>%
  mutate(Class = factor(Class)) %>%
  group_by(Class) %>%
  dplyr::count()

# Tabla de contingencia clase vs labels
result <- df_unficado_sampled %>%
  mutate(Class = factor(Class)) %>%
  group_by(Class) %>%
  tabyl(Class, Labels) %>%
  adorn_totals("row") %>%
  adorn_totals("col")

# print(tabla_num_tumor, n=4)


# Salvamos Tabla en formato csv
#export(tabla_num_tumor, here("Tables", glue("tabla_num_tumor.csv")))
# Salvamos Tabla en formato csv
export(tabla_num_tumor_sampled, here("Tables", glue("tabla_num_tumor_sampled.csv")))

### Barplot número muestras por tipo de tumor -----------------------------------------

# # Gráfico número de muestras por tumor. Datos completos.
# plot_num_tumor <- ggplot(tabla_num_tumor, aes(x = reorder(Labels, n), y = n, fill = Labels)) +
#   geom_col() +
#   scale_fill_discrete(name = "Tipo de tumor") +
#   coord_flip() +
#   theme_bw() +
#   labs(x = "Tipo de tumor", y = "Número de muestras",
#        title = "Número de muestras por tipo de tumor") +
#   ggeasy::easy_remove_legend()
# 
# 
# # Guardar la imagen en la carpeta "plots"
# ggsave(here("plots", "plot_num_tumor.png"))


# Gráfico número de muestras por tumor. Datos Sampleados.
plot_num_tumor <- ggplot(tabla_num_tumor_sampled, aes(x = reorder(Class, n), y = n, fill = Class)) +
  geom_col() +
  scale_fill_discrete(name = "Tipo de tumor") +
  coord_flip() +
  theme_bw() +
  labs(x = "Tipo de tumor", y = "Número de muestras",
       title = "Número de muestras Tumor vs Normal ") +
  ggeasy::easy_remove_legend()

# Guardamos el plot como plotly
plotly_num_tumor <- ggplotly(plot_num_tumor)
saveRDS(plotly_num_tumor, here("Plots/plotly_num_tumor_binario.rds"))

# Guardar la imagen en la carpeta "plots"
ggsave(here("plots", "plot_num_tumor_sampled_normal.png"))




### Utilidades extra ----------------------------------------------------

# # Creamos el dataset en formato long, para utilizarlo con librerías como ggplot
# 
# # Realizar pivot_longer
# datos_preprocessed_long <- datos_preprocessed %>%
#   pivot_longer(cols = -c(Sample, Labels), 
#                names_to = "Genes", 
#                values_to = "Expresion")
# 
# # Para guardar el objeto en formato .rds
# saveRDS(datos_preprocessed_long, here("Data/Merged_dataset/Preprocessed","datos_preprocessed_long.rds"))
# 
# # Cargamos datos long
# datos_preprocessed_long <- readRDS(here("Data/Merged_dataset/Preprocessed","datos_preprocessed_long.rds"))

