
###     SECTION 5 CLUSTERING EXPLORATORIO    

### Descripción -------------------------------------------------------------

# 1 - Clustering exploratorio datos train: PCA,T-SNE,UMAP
# 2 - Generamos un dendrograma con las muestras
# 3 - Creamos heatmaps


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


if (!require("Require")) {install.packages("Require")}

## Require carga los paquetes de CRAN y los instala si faltan
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
          "doParallel", # parallel computing
          "glmnet", #  regression models
          "tictoc", # time
          "MLmetrics", # ML metrics
          "cluster", # clustering
          "RColorBrewer", # color scales
          "factoextra", #multivariate tools
          "viridis", # color scales
          "dendextend" # dendogram tools# Complex heatmaps
          
))

## Paquetes de bioconductor. Se instalan si faltan
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("TCGAbiolinks"))
  BiocManager::install("TCGAbiolinks")

if (!require("SummarizedExperiment"))
  BiocManager::install("SummarizedExperiment")
if (!require("M3C"))
  BiocManager::install("M3C")

if (!require("ComplexHeatmap"))
BiocManager::install("ComplexHeatmap")

### Importación data set train (bruto)----------------------------------------------


traindf <- readRDS(here("Data/Merged_dataset/Processed/traindf_processed.rds")) %>% dplyr::select(-Sample)
traindf$Labels <- as.character(traindf$Labels)



### PCA,T-SNE,UMAP ----------------------------------------------------------



# Preparamos los datos. Debe ser matriz y trasponemos para tener los genes en las filas 
data_t <- traindf %>% as.data.frame() %>%
  select(-Labels) %>%
  t() %>%  as.matrix()
# str(data_t)



# tsne plot
tsne_plot <- tsne(data_t,labels=as.factor(traindf$Labels), dotsize = 3) + ggtitle("TSNE en 2 dimensiones: Muestras de entrenamiento")
ggsave(here("Plots", "tsne_plot.png"))

# para tener el gráfico en version plotly
tsne_plot <- tsne(data_t,labels=as.factor(traindf$Labels), dotsize = 2) + ggtitle("TSNE en 2 dimensiones: Muestras de entrenamiento")

tsne_plot <- ggplotly(tsne_plot)
saveRDS(tsne_plot, here("Plots","tsne_plotly.rds"))


# pca plot
pca_plot <- pca(data_t,labels=traindf$Labels,legendtextsize = 10,axistextsize = 10,dotsize=2)
ggsave(here("Plots", "pca_plot.png"))

# umap plot
umap_plot <- umap(data_t,labels=as.factor(traindf$Labels), dotsize = 3) + ggtitle("UMAP en 2 dimensiones: Muestras de entrenamiento")
ggsave(here("Plots", "umap_plot.png"))

# para tener el gráfico en version plotly
umap_plot <- umap(data_t,labels=as.factor(traindf$Labels), dotsize = 2) + ggtitle("UMAP en 2 dimensiones: Muestras de entrenamiento")

umap_plot <- ggplotly(umap_plot)
saveRDS(umap_plot, here("Plots","umap_plotly.rds"))





### DENDROGRAMA --------------------------------------------------------------

# Fragmento de código extraído y adaptado de: cienciadedatos.net [https://rpubs.com/Joaquin_AR/387758]
# Se unen de nuevo todos los datos en un único dataframe


# Reestructuramos los datos. 
# La librería factoextra emplea el nombre de las filas del dataframe para
# identificar cada observación.
datos_clustering <- traindf %>% as.data.frame()
rownames(datos_clustering) <- paste(1:nrow(datos_clustering),
                                    datos_clustering$Labels,
                                    sep = "_")

# Se emplean únicamente los genes filtrados
#datos_clustering <- datos_clustering %>% select(Labels)

# Se calculan las distancias en base a la correlación de Pearson
mat_distancias <- get_dist(datos_clustering[, -1],
                           method = "pearson",
                           stand = FALSE)


# HIERARCHICAL CLUSTERING
set.seed(1993)
hc_average <- hclust(d = mat_distancias, method = "complete")

# PERSONALIZACIÓN DENDOGRAMA
# Vector de colores para cada observación: Se juntan dos paletas para tener
# suficientes colores

colores <- c(brewer.pal(n = 8, name = "Dark2"),
             brewer.pal(n = 8, name = "Set1")) %>%
  unique()

# Se seleccionan 14 colores, uno para cada tipo de tumor
colores <- colores[1:15]

# Se asigna a cada tipo de tumor uno de los colores. Para conseguirlo de forma
# rápida, se convierte la variable tipo_tumor en factor y se emplea su codificación
# numérica interna para asignar los colores.
  colores <- colores[as.factor(datos_clustering$Labels)]

# Se reorganiza el vector de colores según el orden en que se han agrupado las
# observaciones en el clustering
colores <- colores[hc_average$order]

dendro_plot <- fviz_dend(x = hc_average, 
          label_cols = colores,
          cex = 0.6,
          lwd = 0.3,
          main = "Linkage completo",
          type = "circular")

ggsave(here("Plots", "dendro_plot.png"))





### Heatmap -------------------------------------------------------------
# Cluster para parallel procesing
cl <- makeCluster(1)
registerDoParallel(cl)

#clustering de las observaciones con método average
row_dend <- hclust(d = dist(datos_clustering[, -1], method = "euclidean"), method = "average")

# Se transpone la matriz de datos para que las observaciones estén como columnas
col_dend <- hclust(d = dist(t(datos_clustering[, -1]), method = "euclidean"), method = "average")
#Gama de colores
colores <- magma(256)


# Heatmap complejo 
heatmap_plot <- Heatmap(matrix = as.matrix(datos_clustering[, -1]), name = " Valor expresion",
        col = colores,
        row_title = "observaciones",
        column_title = "Heatmap expresión RNAseq genes (completo)",
        row_names_gp = gpar(fontsize = 7),
        column_names_gp = gpar(fontsize = 6),
        cluster_rows = color_branches(dend = row_dend, k = 4),
        cluster_columns = color_branches(dend = col_dend, k = 2))

# Guardar el gráfico como un archivo PNG
png(here("Plots", "heatmap_plot.png"))
print(heatmap_plot)
dev.off()

# heatmap simple
heatmap_plot2 <- heatmap(as.matrix(datos_clustering[, -1]), main = "Heatmap expresión RNAseq de los genes (Reducido)")

# Guardar el gráfico como un archivo PNG
png(here("Plots", "heatmap_plot2.png"))
print(heatmap_plot2)
dev.off()

# Paramos los clusters de paralelización
stopCluster(cl)