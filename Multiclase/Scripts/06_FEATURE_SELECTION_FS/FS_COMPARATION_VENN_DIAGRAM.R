###  SECTION 6.E VENN DIAGRAM FS
### Descripción -------------------------------------------------------------

# 1 - VENN DIAGRAM FS


## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 6.E VENN DIAGRAM FS

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
          "ggpubr",
          "ggVennDiagram",
          "ggeasy"# Publication ready plots

          
))



### Importación datasets

borutadf <- readRDS(here("Data/FS_datasets/BORUTA/datos_train_boruta.rds")) %>% dplyr::select(-Labels)
lassodf <- readRDS(here("Data/FS_datasets/LASSO/datos_train_lasso.rds")) %>% dplyr::select(-Labels)
splsdadf <- readRDS(here("Data/FS_datasets/SPLSDA/datos_train_SPLSDA.rds")) %>% dplyr::select(-Labels)

# Lista genes por FS
genes_fs_list <- list(
  Boruta = colnames(borutadf), 
  Lasso = colnames(lassodf), 
  Splsda = colnames(splsdadf)
)

vennplot <- ggVennDiagram(genes_fs_list) + 
   ggeasy::easy_adjust_legend(to = "right") +
  scale_fill_gradient(low="blue",high = "red")
 
ggsave(here("Plots", "FS_Venn_Diagram.png"))