
###     SECTION 8 MODELS COMPARATION AND ENSEMBLE

### Descripción -------------------------------------------------------------

# 1 - Carga de todos los modelos, predicciones y matrices de confusión
# 2 - Tablas y gráficos comparativos de distintas métricas de los modelos
# 3 - Gráficos métricas modelo por tipo de técnica Feature selection
# 3 - Ensemble  con los 4 mejores modelos. 
# 4 - Tabla con la decisión final del ensemble con la clasificación para muestras test en base a la moda.
# 5 - Gráficos métricas modelo por tipo de técnica Feature selection

## ***Notas: 

### Script metadata --------------------------------------------
#
# Author: Esteban Calle
# 
# Email:
#
# Date: 2023-04-06
#
# Script Name: SECTION 8 MODELS COMPARATION AND ENSEMBLE     
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


#


### Importación modelos -----------------------------------------------------

modelo_knn_boruta <- readRDS(here("Models/Classification/knn_boruta.rda"))
modelo_knn_lasso <- readRDS(here("Models/Classification/knn_lasso.rda"))
modelo_knn_pca <- readRDS(here("Models/Classification/knn_pca.rda"))
modelo_knn_splsda <- readRDS(here("Models/Classification/knn_splsda.rda"))
modelo_ann_boruta <- readRDS(here("Models/Classification/ann_boruta.rda"))
modelo_ann_lasso <- readRDS(here("Models/Classification/ann_lasso.rda"))
modelo_ann_pca <- readRDS(here("Models/Classification/ann_pca.rda"))
modelo_ann_splsda <- readRDS(here("Models/Classification/ann_splsda.rda"))
modelo_svml_boruta <- readRDS(here("Models/Classification/svml_boruta.rda"))
modelo_svml_lasso <- readRDS(here("Models/Classification/svml_lasso.rda"))
modelo_svml_pca <- readRDS(here("Models/Classification/svml_pca.rda"))
modelo_svml_splsda <- readRDS(here("Models/Classification/svml_splsda.rda"))
modelo_svmr_boruta <- readRDS(here("Models/Classification/svmr_boruta.rda"))
modelo_svmr_lasso <- readRDS(here("Models/Classification/svmr_lasso.rda"))
modelo_svmr_pca <- readRDS(here("Models/Classification/svmr_pca.rda"))
modelo_svmr_splsda <- readRDS(here("Models/Classification/svmr_splsda.rda"))
modelo_nb_boruta <- readRDS(here("Models/Classification/nb_boruta.rda"))
modelo_nb_lasso <- readRDS(here("Models/Classification/nb_lasso.rda"))
modelo_nb_pca <- readRDS(here("Models/Classification/nb_pca.rda"))
modelo_nb_splsda <- readRDS(here("Models/Classification/nb_splsda.rda"))
modelo_rf_boruta <- readRDS(here("Models/Classification/rf_boruta.rda"))
modelo_rf_lasso <- readRDS(here("Models/Classification/rf_lasso.rda"))
modelo_rf_pca <- readRDS(here("Models/Classification/rf_pca.rda"))
modelo_rf_splsda <- readRDS(here("Models/Classification/rf_splsda.rda"))
modelo_msgl_lasso <- readRDS(here("Models/FS/cv_lasso_lasso.rda"))



# Evaluación Error Validación ---------------------------------------------


# Agrupamos modelos entrenados con caret
# ANN Y  MSGL no los cargaremos

modelos <-
  list(
    KNN_BOR = modelo_knn_boruta,
    KNN_LASSO = modelo_knn_lasso,
    KNN_PCA = modelo_knn_pca,
    KNN_SPLSDA = modelo_knn_splsda,
    SVML_BOR = modelo_svml_boruta,
    SVML_LASSO = modelo_svml_lasso,
    SVML_PCA = modelo_svml_pca,
    SVML_SPLSDA = modelo_svml_splsda,
    SVMR_BOR = modelo_svmr_boruta,
    SVMR_LASSO = modelo_svmr_lasso,
    SVMR_PCA = modelo_svmr_pca,
    SVMR_SPLSDA = modelo_svmr_splsda,
    NB_BOR = modelo_nb_boruta,
    NB_LASSO = modelo_nb_lasso,
    NB_PCA = modelo_nb_pca,
    NB_SPLSDA = modelo_nb_splsda,
    RF_BOR = modelo_rf_boruta,
    RF_LASSO = modelo_rf_lasso,
    RF_PCA = modelo_rf_pca,
    RF_SPLSDA = modelo_rf_splsda
    # ANN_BOR = modelo_ann_boruta,
    # ANN_LASSO = modelo_ann_lasso,
    # ANN_PCA = modelo_ann_pca,
    # ANN_SPLSDA = modelo_ann_splsda,
  )

# Extraemos métricas de los modelos
resultados_resamples <- resamples(modelos)

# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor",-Resample) %>%
  separate(
    col = "modelo",
    into = c("modelo", "metrica"),
    sep = "~",
    remove = TRUE
  )

# Accuracy y Kappa promedio de cada modelo durante la validación
promedio_metricas_resamples <- metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  ungroup() %>% 
  add_row(modelo = "ANN_BOR", Accuracy = mean(modelo_ann_boruta$metrics$val_accuracy), Kappa = NA) %>%
  add_row(modelo = "ANN_LASSO", Accuracy = mean(modelo_ann_lasso$metrics$val_accuracy), Kappa = NA) %>%
  add_row(modelo = "ANN_PCA", Accuracy = mean(modelo_ann_pca$metrics$val_accuracy), Kappa = NA) %>%
  add_row(modelo = "ANN_SPLSDA", Accuracy = mean(modelo_ann_splsda$metrics$val_accuracy), Kappa = NA) %>%
  arrange(desc(Accuracy))

# vemos tabla y la guardamos en csv y pdf

print(promedio_metricas_resamples, n=24)

export(promedio_metricas_resamples, (here("Tables/promedio_metricas_resamples.csv")))

pdf(here("Tables/promedio_metricas_resamples.pdf"), width = 15, height = 20)       # Export PDF
grid.table(promedio_metricas_resamples)
dev.off()

# Accuracy basal (proporción de muestras de clase mayoritaria) (para poner en yintercept)
#traindf <- readRDS(here("Data/Merged_dataset/Processed/traindf_processed.rds"))
# mean(traindf$Labels == "BRCA")

# Gráfico Validación: Accuracy medio repeated-CV
metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ungroup() %>% 
  add_row(modelo = "ANN_BOR", media = mean(modelo_ann_boruta$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_LASSO", media = mean(modelo_ann_lasso$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_PCA", media = mean(modelo_ann_pca$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_SPLSDA", media = mean(modelo_ann_splsda$metrics$val_accuracy)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 8, color = "#00CDCD") +
  geom_text(color = "black", size = 3, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.16, linetype = "dashed", color="blue") +
  annotate(geom = "text", y = 0.27, x = 23.5, label = "Accuracy basal") +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()

# Guardamos plot
ggsave(here("Plots", "models_Validacion_Accuracy.png"), width = 7.69, height = 8.25)



# Gráfico barplots Validación: Accuracy medio repeated-CV
metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ungroup() %>% 
  add_row(modelo = "ANN_BOR", media = mean(modelo_ann_boruta$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_LASSO", media = mean(modelo_ann_lasso$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_PCA", media = mean(modelo_ann_pca$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_SPLSDA", media = mean(modelo_ann_splsda$metrics$val_accuracy)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
    geom_bar(stat = "identity", fill = "#00CDCD") +
    geom_text(color = "black", size = 3,  nudge_y = -0.03, fontface = "bold") +
    scale_y_continuous(limits = c(0, 1)) +
    # Accuracy basal
    geom_hline(yintercept = 0.16, linetype = "dashed", color="black") +
    annotate(geom = "text", y = 0.27, x = 22, label = "Accuracy basal") +
    labs(title = "Validación: Accuracy medio repeated-CV",
         subtitle = "Modelos ordenados por media",
         x = "modelo") +
    coord_flip() +
    theme_bw()

# Guardamos plot bars plot
ggsave(here("Plots", "models_Validacion_Accuracy_barplot.png"), width = 7.69, height = 8.25)


# Gráfico boxplots Validación: Accuracy medio repeated-CV
metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.16, linetype = "dashed", color="black") +
  annotate(geom = "text", y = 0.27, x = 19.5, label = "Accuracy basal") +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")

# Guardamos plot boxplots
ggsave(here("Plots", "models_Validacion_Accuracy_boxplot.png"), width = 7.69, height = 8.25)




# Evaluación Error test ---------------------------------------------


# Extraemos predicciones de los modelos. Las teníamos ya guardadas. Las cargamos.

pred_knn_boruta <- readRDS(here("Models/Predictions/pred_knn_boruta.rds"))
pred_knn_lasso <- readRDS(here("Models/Predictions/pred_knn_lasso.rds"))
pred_knn_pca <- readRDS(here("Models/Predictions/pred_knn_pca.rds"))
pred_knn_SPLSDA <- readRDS(here("Models/Predictions/pred_knn_splsda.rds"))

pred_svml_boruta <- readRDS(here("Models/Predictions/pred_svml_boruta.rds"))
pred_svml_lasso <- readRDS(here("Models/Predictions/pred_svml_lasso.rds"))
pred_svml_pca <- readRDS(here("Models/Predictions/pred_svml_pca.rds"))
pred_svml_SPLSDA <- readRDS(here("Models/Predictions/pred_svml_splsda.rds"))

pred_svmr_boruta <- readRDS(here("Models/Predictions/pred_svmr_boruta.rds"))
pred_svmr_lasso <- readRDS(here("Models/Predictions/pred_svmr_lasso.rds"))
pred_svmr_pca <- readRDS(here("Models/Predictions/pred_svmr_pca.rds"))
pred_svmr_SPLSDA <- readRDS(here("Models/Predictions/pred_svmr_splsda.rds"))

pred_nb_boruta <- readRDS(here("Models/Predictions/pred_nb_boruta.rds"))
pred_nb_lasso <- readRDS(here("Models/Predictions/pred_nb_lasso.rds"))
pred_nb_pca <- readRDS(here("Models/Predictions/pred_nb_pca.rds"))
pred_nb_SPLSDA <- readRDS(here("Models/Predictions/pred_nb_splsda.rds"))

pred_rf_boruta <- readRDS(here("Models/Predictions/pred_rf_boruta.rds"))
pred_rf_lasso <- readRDS(here("Models/Predictions/pred_rf_lasso.rds"))
pred_rf_pca <- readRDS(here("Models/Predictions/pred_rf_pca.rds"))
pred_rf_SPLSDA <- readRDS(here("Models/Predictions/pred_rf_splsda.rds"))

pred_ann_boruta <- readRDS(here("Models/Predictions/y_predf_boruta.rds"))
pred_ann_lasso <- readRDS(here("Models/Predictions/y_predf_lasso.rds"))
pred_ann_pca <- readRDS(here("Models/Predictions/y_predf_pca.rds"))
pred_ann_SPLSDA <- readRDS(here("Models/Predictions/y_predf_splsda.rds"))
pred_msgl_lasso <- readRDS(here("Models/Predictions/pred_lasso_lasso.rds"))


# Las listamos en un dataframe llamado Predicciones
predicciones <- data.frame(
  KNN_BOR = pred_knn_boruta,
  KNN_LASSO = pred_knn_lasso,
  KNN_PCA = pred_knn_pca,
  KNN_SPLSDA = pred_knn_SPLSDA,
  ANN_BOR = pred_ann_boruta,
  ANN_LASSO = pred_ann_lasso,
  ANN_PCA = pred_ann_pca,
  ANN_SPLSDA = pred_ann_SPLSDA,
  SVML_BOR = pred_svml_boruta,
  SVML_LASSO = pred_svml_lasso,
  SVML_PCA = pred_svml_pca,
  SVML_SPLSDA = pred_svml_SPLSDA,
  SVMR_BOR = pred_svmr_boruta,
  SVMR_LASSO = pred_svmr_lasso,
  SVMR_PCA = pred_svmr_pca,
  SVMR_SPLSDA = pred_svmr_SPLSDA,
  NB_BOR = pred_nb_boruta,
  NB_LASSO = pred_nb_lasso,
  NB_PCA = pred_nb_pca,
  NB_SPLSDA = pred_nb_SPLSDA,
  RF_BOR = pred_rf_boruta,
  RF_LASSO = pred_rf_lasso,
  RF_PCA = pred_rf_pca,
  RF_SPLSDA = pred_rf_SPLSDA,
  MSGL_LASSO = pred_msgl_lasso
)

# Cargamos matrices de confusión
conf_ann_boruta <- readRDS(here("Models/conf_matrices/conf_ann_boruta.rds"))
conf_ann_lasso <- readRDS(here("Models/conf_matrices/conf_ann_lasso.rds"))
conf_ann_pca <- readRDS(here("Models/conf_matrices/conf_ann_pca.rds"))
conf_ann_SPLSDA <- readRDS(here("Models/conf_matrices/conf_ann_splsda.rds"))

conf_knn_boruta <- readRDS(here("Models/conf_matrices/conf_knn_boruta.rds"))
conf_knn_lasso <- readRDS(here("Models/conf_matrices/conf_knn_lasso.rds"))
conf_knn_pca <- readRDS(here("Models/conf_matrices/conf_knn_pca.rds"))
conf_knn_SPLSDA <- readRDS(here("Models/conf_matrices/conf_knn_splsda.rds"))

conf_svml_boruta <- readRDS(here("Models/conf_matrices/conf_svml_boruta.rds"))
conf_svml_lasso <- readRDS(here("Models/conf_matrices/conf_svml_lasso.rds"))
conf_svml_pca <- readRDS(here("Models/conf_matrices/conf_svml_pca.rds"))
conf_svml_SPLSDA <- readRDS(here("Models/conf_matrices/conf_svml_splsda.rds"))

conf_svmr_boruta <- readRDS(here("Models/conf_matrices/conf_svmr_boruta.rds"))
conf_svmr_lasso <- readRDS(here("Models/conf_matrices/conf_svmr_lasso.rds"))
conf_svmr_pca <- readRDS(here("Models/conf_matrices/conf_svmr_pca.rds"))
conf_svmr_SPLSDA <- readRDS(here("Models/conf_matrices/conf_svmr_splsda.rds"))

conf_nb_boruta <- readRDS(here("Models/conf_matrices/conf_nb_boruta.rds"))
conf_nb_lasso <- readRDS(here("Models/conf_matrices/conf_nb_lasso.rds"))
conf_nb_pca <- readRDS(here("Models/conf_matrices/conf_nb_pca.rds"))
conf_nb_SPLSDA <- readRDS(here("Models/conf_matrices/conf_nb_splsda.rds"))

conf_rf_boruta <- readRDS(here("Models/conf_matrices/conf_rf_boruta.rds"))
conf_rf_lasso <- readRDS(here("Models/conf_matrices/conf_rf_lasso.rds"))
conf_rf_pca <- readRDS(here("Models/conf_matrices/conf_rf_pca.rds"))
conf_rf_SPLSDA <- readRDS(here("Models/conf_matrices/conf_rf_splsda.rds"))

conf_msgl_lasso <- readRDS(here("Models/conf_matrices/conf_lasso_lasso.rds"))



## Extraemos métricas de la matriz de confusión


# Modelo: KNN 

accur_knn_boruta    <- conf_knn_boruta[3][[1]][[1]]#accuracy
accur_knn_boruta_li <- conf_knn_boruta[3][[1]][[3]]#accuracy lower
accur_knn_boruta_ls <- conf_knn_boruta[3][[1]][[4]]#accuracy upper
kappa_knn_boruta    <- conf_knn_boruta[3][[1]][[2]]#kappa
sensit_knn_boruta   <- conf_knn_boruta[4][[1]][[1]]#sensitivity
specif_knn_boruta   <- conf_knn_boruta[4][[1]][[2]]#Specificity


accur_knn_lasso    <- conf_knn_lasso[3][[1]][[1]]#accuracy
accur_knn_lasso_li <- conf_knn_lasso[3][[1]][[3]]#accuracy lower
accur_knn_lasso_ls <- conf_knn_lasso[3][[1]][[4]]#accuracy upper
kappa_knn_lasso    <- conf_knn_lasso[3][[1]][[2]]#kappa
sensit_knn_lasso   <- conf_knn_lasso[4][[1]][[1]]#sensitivity
specif_knn_lasso   <- conf_knn_lasso[4][[1]][[2]]#Specificity


accur_knn_pca    <- conf_knn_pca[3][[1]][[1]]#accuracy
accur_knn_pca_li <- conf_knn_pca[3][[1]][[3]]#accuracy lower
accur_knn_pca_ls <- conf_knn_pca[3][[1]][[4]]#accuracy upper
kappa_knn_pca    <- conf_knn_pca[3][[1]][[2]]#kappa
sensit_knn_pca   <- conf_knn_pca[4][[1]][[1]]#sensitivity
specif_knn_pca   <- conf_knn_pca[4][[1]][[2]]#Specificity

accur_knn_SPLSDA    <- conf_knn_SPLSDA[3][[1]][[1]]#accuracy
accur_knn_SPLSDA_li <- conf_knn_SPLSDA[3][[1]][[3]]#accuracy lower
accur_knn_SPLSDA_ls <- conf_knn_SPLSDA[3][[1]][[4]]#accuracy upper
kappa_knn_SPLSDA    <- conf_knn_SPLSDA[3][[1]][[2]]#kappa
sensit_knn_SPLSDA   <- conf_knn_SPLSDA[4][[1]][[1]]#sensitivity
specif_knn_SPLSDA   <- conf_knn_SPLSDA[4][[1]][[2]]#Specificity


# Modelo: SVM  lineal 

accur_svml_boruta    <- conf_svml_boruta[3][[1]][[1]] # accuracy
accur_svml_boruta_li <- conf_svml_boruta[3][[1]][[3]] # accuracy lower
accur_svml_boruta_ls <- conf_svml_boruta[3][[1]][[4]] # accuracy upper
kappa_svml_boruta    <- conf_svml_boruta[3][[1]][[2]] # kappa
sensit_svml_boruta   <- conf_svml_boruta[4][[1]][[1]] # sensitivity
specif_svml_boruta   <- conf_svml_boruta[4][[1]][[2]] # specificity

accur_svml_lasso    <- conf_svml_lasso[3][[1]][[1]] # accuracy
accur_svml_lasso_li <- conf_svml_lasso[3][[1]][[3]] # accuracy lower
accur_svml_lasso_ls <- conf_svml_lasso[3][[1]][[4]] # accuracy upper
kappa_svml_lasso    <- conf_svml_lasso[3][[1]][[2]] # kappa
sensit_svml_lasso   <- conf_svml_lasso[4][[1]][[1]] # sensitivity
specif_svml_lasso   <- conf_svml_lasso[4][[1]][[2]] # specificity

accur_svml_pca    <- conf_svml_pca[3][[1]][[1]] # accuracy
accur_svml_pca_li <- conf_svml_pca[3][[1]][[3]] # accuracy lower
accur_svml_pca_ls <- conf_svml_pca[3][[1]][[4]] # accuracy upper
kappa_svml_pca    <- conf_svml_pca[3][[1]][[2]] # kappa
sensit_svml_pca   <- conf_svml_pca[4][[1]][[1]] # sensitivity
specif_svml_pca   <- conf_svml_pca[4][[1]][[2]] # specificity

accur_svml_SPLSDA    <- conf_svml_SPLSDA[3][[1]][[1]] # accuracy
accur_svml_SPLSDA_li <- conf_svml_SPLSDA[3][[1]][[3]] # accuracy lower
accur_svml_SPLSDA_ls <- conf_svml_SPLSDA[3][[1]][[4]] # accuracy upper
kappa_svml_SPLSDA    <- conf_svml_SPLSDA[3][[1]][[2]] # kappa
sensit_svml_SPLSDA   <- conf_svml_SPLSDA[4][[1]][[1]] # sensitivity
specif_svml_SPLSDA   <- conf_svml_SPLSDA[4][[1]][[2]] # specificity


# Modelo: SVM  Radial 
accur_svmr_boruta    <- conf_svmr_boruta[3][[1]][[1]]#accuracy
accur_svmr_boruta_li <- conf_svmr_boruta[3][[1]][[3]]#accuracy lower
accur_svmr_boruta_ls <- conf_svmr_boruta[3][[1]][[4]]#accuracy upper
kappa_svmr_boruta    <- conf_svmr_boruta[3][[1]][[2]]#kappa
sensit_svmr_boruta   <- conf_svmr_boruta[4][[1]][[1]]#sensitivity
specif_svmr_boruta   <- conf_svmr_boruta[4][[1]][[2]]#Specificity

accur_svmr_lasso    <- conf_svmr_lasso[3][[1]][[1]]#accuracy
accur_svmr_lasso_li <- conf_svmr_lasso[3][[1]][[3]]#accuracy lower
accur_svmr_lasso_ls <- conf_svmr_lasso[3][[1]][[4]]#accuracy upper
kappa_svmr_lasso    <- conf_svmr_lasso[3][[1]][[2]]#kappa
sensit_svmr_lasso   <- conf_svmr_lasso[4][[1]][[1]]#sensitivity
specif_svmr_lasso   <- conf_svmr_lasso[4][[1]][[2]]#Specificity

accur_svmr_pca    <- conf_svmr_pca[3][[1]][[1]]#accuracy
accur_svmr_pca_li <- conf_svmr_pca[3][[1]][[3]]#accuracy lower
accur_svmr_pca_ls <- conf_svmr_pca[3][[1]][[4]]#accuracy upper
kappa_svmr_pca    <- conf_svmr_pca[3][[1]][[2]]#kappa
sensit_svmr_pca   <- conf_svmr_pca[4][[1]][[1]]#sensitivity
specif_svmr_pca   <- conf_svmr_pca[4][[1]][[2]]#Specificity

accur_svmr_SPLSDA    <- conf_svmr_SPLSDA[3][[1]][[1]]#accuracy
accur_svmr_SPLSDA_li <- conf_svmr_SPLSDA[3][[1]][[3]]#accuracy lower
accur_svmr_SPLSDA_ls <- conf_svmr_SPLSDA[3][[1]][[4]]#accuracy upper
kappa_svmr_SPLSDA    <- conf_svmr_SPLSDA[3][[1]][[2]]#kappa
sensit_svmr_SPLSDA   <- conf_svmr_SPLSDA[4][[1]][[1]]#sensitivity
specif_svmr_SPLSDA   <- conf_svmr_SPLSDA[4][[1]][[2]]#Specificity


# Modelo: Naive Bayes 

accur_nb_boruta    <- conf_nb_boruta[3][[1]][[1]]#accuracy
accur_nb_boruta_li <- conf_nb_boruta[3][[1]][[3]]#accuracy lower
accur_nb_boruta_ls <- conf_nb_boruta[3][[1]][[4]]#accuracy upper
kappa_nb_boruta    <- conf_nb_boruta[3][[1]][[2]]#kappa
sensit_nb_boruta   <- conf_nb_boruta[4][[1]][[1]]#sensitivity
specif_nb_boruta   <- conf_nb_boruta[4][[1]][[2]]#Specificity


accur_nb_lasso    <- conf_nb_lasso[3][[1]][[1]]#accuracy
accur_nb_lasso_li <- conf_nb_lasso[3][[1]][[3]]#accuracy lower
accur_nb_lasso_ls <- conf_nb_lasso[3][[1]][[4]]#accuracy upper
kappa_nb_lasso    <- conf_nb_lasso[3][[1]][[2]]#kappa
sensit_nb_lasso   <- conf_nb_lasso[4][[1]][[1]]#sensitivity
specif_nb_lasso   <- conf_nb_lasso[4][[1]][[2]]#Specificity


accur_nb_pca    <- conf_nb_pca[3][[1]][[1]]#accuracy
accur_nb_pca_li <- conf_nb_pca[3][[1]][[3]]#accuracy lower
accur_nb_pca_ls <- conf_nb_pca[3][[1]][[4]]#accuracy upper
kappa_nb_pca    <- conf_nb_pca[3][[1]][[2]]#kappa
sensit_nb_pca   <- conf_nb_pca[4][[1]][[1]]#sensitivity
specif_nb_pca   <- conf_nb_pca[4][[1]][[2]]#Specificity

accur_nb_SPLSDA    <- conf_nb_SPLSDA[3][[1]][[1]]#accuracy
accur_nb_SPLSDA_li <- conf_nb_SPLSDA[3][[1]][[3]]#accuracy lower
accur_nb_SPLSDA_ls <- conf_nb_SPLSDA[3][[1]][[4]]#accuracy upper
kappa_nb_SPLSDA    <- conf_nb_SPLSDA[3][[1]][[2]]#kappa
sensit_nb_SPLSDA   <- conf_nb_SPLSDA[4][[1]][[1]]#sensitivity
specif_nb_SPLSDA   <- conf_nb_SPLSDA[4][[1]][[2]]#Specificity



# Modelo: R. Forest 

accur_rf_boruta    <- conf_rf_boruta[3][[1]][[1]]#accuracy
accur_rf_boruta_li <- conf_rf_boruta[3][[1]][[3]]#accuracy lower
accur_rf_boruta_ls <- conf_rf_boruta[3][[1]][[4]]#accuracy upper
kappa_rf_boruta    <- conf_rf_boruta[3][[1]][[2]]#kappa
sensit_rf_boruta   <- conf_rf_boruta[4][[1]][[1]]#sensitivity
specif_rf_boruta   <- conf_rf_boruta[4][[1]][[2]]#Specificity


accur_rf_lasso    <- conf_rf_lasso[3][[1]][[1]]#accuracy
accur_rf_lasso_li <- conf_rf_lasso[3][[1]][[3]]#accuracy lower
accur_rf_lasso_ls <- conf_rf_lasso[3][[1]][[4]]#accuracy upper
kappa_rf_lasso    <- conf_rf_lasso[3][[1]][[2]]#kappa
sensit_rf_lasso   <- conf_rf_lasso[4][[1]][[1]]#sensitivity
specif_rf_lasso   <- conf_rf_lasso[4][[1]][[2]]#Specificity


accur_rf_pca    <- conf_rf_pca[3][[1]][[1]]#accuracy
accur_rf_pca_li <- conf_rf_pca[3][[1]][[3]]#accuracy lower
accur_rf_pca_ls <- conf_rf_pca[3][[1]][[4]]#accuracy upper
kappa_rf_pca    <- conf_rf_pca[3][[1]][[2]]#kappa
sensit_rf_pca   <- conf_rf_pca[4][[1]][[1]]#sensitivity
specif_rf_pca   <- conf_rf_pca[4][[1]][[2]]#Specificity

accur_rf_SPLSDA    <- conf_rf_SPLSDA[3][[1]][[1]]#accuracy
accur_rf_SPLSDA_li <- conf_rf_SPLSDA[3][[1]][[3]]#accuracy lower
accur_rf_SPLSDA_ls <- conf_rf_SPLSDA[3][[1]][[4]]#accuracy upper
kappa_rf_SPLSDA    <- conf_rf_SPLSDA[3][[1]][[2]]#kappa
sensit_rf_SPLSDA   <- conf_rf_SPLSDA[4][[1]][[1]]#sensitivity
specif_rf_SPLSDA   <- conf_rf_SPLSDA[4][[1]][[2]]#Specificity



# Modelo: NNET 

accur_ann_boruta    <- conf_ann_boruta[3][[1]][[1]] # Accuracy
accur_ann_boruta_li <- conf_ann_boruta[3][[1]][[3]] # Accuracy Lower
accur_ann_boruta_ls <- conf_ann_boruta[3][[1]][[4]] # Accuracy Upper
kappa_ann_boruta    <- conf_ann_boruta[3][[1]][[2]] # Kappa
sensit_ann_boruta   <- conf_ann_boruta[4][[1]][[1]] # Sensitivity
specif_ann_boruta   <- conf_ann_boruta[4][[1]][[2]] # Specificity

accur_ann_lasso    <- conf_ann_lasso[3][[1]][[1]] # Accuracy
accur_ann_lasso_li <- conf_ann_lasso[3][[1]][[3]] # Accuracy Lower
accur_ann_lasso_ls <- conf_ann_lasso[3][[1]][[4]] # Accuracy Upper
kappa_ann_lasso    <- conf_ann_lasso[3][[1]][[2]] # Kappa
sensit_ann_lasso   <- conf_ann_lasso[4][[1]][[1]] # Sensitivity
specif_ann_lasso   <- conf_ann_lasso[4][[1]][[2]] # Specificity

accur_ann_pca    <- conf_ann_pca[3][[1]][[1]] # Accuracy
accur_ann_pca_li <- conf_ann_pca[3][[1]][[3]] # Accuracy Lower
accur_ann_pca_ls <- conf_ann_pca[3][[1]][[4]] # Accuracy Upper
kappa_ann_pca    <- conf_ann_pca[3][[1]][[2]] # Kappa
sensit_ann_pca   <- conf_ann_pca[4][[1]][[1]] # Sensitivity
specif_ann_pca   <- conf_ann_pca[4][[1]][[2]] # Specificity

accur_ann_SPLSDA    <- conf_ann_SPLSDA[3][[1]][[1]] # Accuracy
accur_ann_SPLSDA_li <- conf_ann_SPLSDA[3][[1]][[3]] # Accuracy Lower
accur_ann_SPLSDA_ls <- conf_ann_SPLSDA[3][[1]][[4]] # Accuracy Upper
kappa_ann_SPLSDA    <- conf_ann_SPLSDA[3][[1]][[2]] # Kappa
sensit_ann_SPLSDA   <- conf_ann_SPLSDA[4][[1]][[1]] # Sensitivity
specif_ann_SPLSDA   <- conf_ann_SPLSDA[4][[1]][[2]] # Specificity


# Modelo: MSGL 

accur_msgl_lasso    <- conf_msgl_lasso[3][[1]][[1]] # Accuracy
accur_msgl_lasso_li <- conf_msgl_lasso[3][[1]][[3]] # Accuracy Lower
accur_msgl_lasso_ls <- conf_msgl_lasso[3][[1]][[4]] # Accuracy Upper
kappa_msgl_lasso    <- conf_msgl_lasso[3][[1]][[2]] # Kappa
sensit_msgl_lasso   <- conf_msgl_lasso[4][[1]][[1]] # Sensitivity
specif_msgl_lasso   <- conf_msgl_lasso[4][[1]][[2]] # Specificity

#  Creamos Dataframe unificado con todas las métricas

df_compar_mod <- data.frame(
  Modelo = c(
    "KNN_BOR",
    "KNN_LASSO",
    "KNN_PCA",
    "KNN_SPLSDA",
    "ANN_BOR",
    "ANN_LASSO",
    "ANN_PCA",
    "ANN_SPLSDA",
    "SVML_BOR",
    "SVML_LASSO",
    "SVML_PCA",
    "SVML_SPLSDA",
    "SVMR_BOR",
    "SVMR_LASSO",
    "SVMR_PCA",
    "SVMR_SPLSDA",
    "NB_BOR",
    "NB_LASSO",
    "NB_PCA",
    "NB_SPLSDA",
    "RF_BOR",
    "RF_LASSO",
    "RF_PCA",
    "RF_SPLSDA",
    "MSGL_LASSO"
  ),
  Accuracy = c(
    accur_knn_boruta,
    accur_knn_lasso,
    accur_knn_pca,
    accur_knn_SPLSDA,
    accur_ann_boruta,
    accur_ann_lasso,
    accur_ann_pca,
    accur_ann_SPLSDA,
    accur_svml_boruta,
    accur_svml_lasso,
    accur_svml_pca,
    accur_svml_SPLSDA,
    accur_svmr_boruta,
    accur_svmr_lasso,
    accur_svmr_pca,
    accur_svmr_SPLSDA,
    accur_nb_boruta,
    accur_nb_lasso,
    accur_nb_pca,
    accur_nb_SPLSDA,
    accur_rf_boruta,
    accur_rf_lasso,
    accur_rf_pca,
    accur_rf_SPLSDA,
    accur_msgl_lasso
  ),
  Lower_Ac = c(
    accur_knn_boruta_li,
    accur_knn_lasso_li,
    accur_knn_pca_li,
    accur_knn_SPLSDA_li,
    accur_ann_boruta_li,
    accur_ann_lasso_li,
    accur_ann_pca_li,
    accur_ann_SPLSDA_li,
    accur_svml_boruta_li,
    accur_svml_lasso_li,
    accur_svml_pca_li,
    accur_svml_SPLSDA_li,
    accur_svmr_boruta_li,
    accur_svmr_lasso_li,
    accur_svmr_pca_li,
    accur_svmr_SPLSDA_li,
    accur_nb_boruta_li,
    accur_nb_lasso_li,
    accur_nb_pca_li,
    accur_nb_SPLSDA_li,
    accur_rf_boruta_li,
    accur_rf_lasso_li,
    accur_rf_pca_li,
    accur_rf_SPLSDA_li,
    accur_msgl_lasso_li
  ),
  Upper_Ac = c(
    accur_knn_boruta_ls,
    accur_knn_lasso_ls,
    accur_knn_pca_ls,
    accur_knn_SPLSDA_ls,
    accur_ann_boruta_ls,
    accur_ann_lasso_ls,
    accur_ann_pca_ls,
    accur_ann_SPLSDA_ls,
    accur_svml_boruta_ls,
    accur_svml_lasso_ls,
    accur_svml_pca_ls,
    accur_svml_SPLSDA_ls,
    accur_svmr_boruta_ls,
    accur_svmr_lasso_ls,
    accur_svmr_pca_ls,
    accur_svmr_SPLSDA_ls,
    accur_nb_boruta_ls,
    accur_nb_lasso_ls,
    accur_nb_pca_ls,
    accur_nb_SPLSDA_ls,
    accur_rf_boruta_ls,
    accur_rf_lasso_ls,
    accur_rf_pca_ls,
    accur_rf_SPLSDA_ls,
    accur_msgl_lasso_ls
  ),
  Kappa = c(
    kappa_knn_boruta,
    kappa_knn_lasso,
    kappa_knn_pca,
    kappa_knn_SPLSDA,
    kappa_ann_boruta,
    kappa_ann_lasso,
    kappa_ann_pca,
    kappa_ann_SPLSDA,
    kappa_svml_boruta,
    kappa_svml_lasso,
    kappa_svml_pca,
    kappa_svml_SPLSDA,
    kappa_svmr_boruta,
    kappa_svmr_lasso,
    kappa_svmr_pca,
    kappa_svmr_SPLSDA,
    kappa_nb_boruta,
    kappa_nb_lasso,
    kappa_nb_pca,
    kappa_nb_SPLSDA,
    kappa_rf_boruta,
    kappa_rf_lasso,
    kappa_rf_pca,
    kappa_rf_SPLSDA,
    kappa_msgl_lasso
  ),
  Especifi = c(
    specif_knn_boruta,
    specif_knn_lasso,
    specif_knn_pca,
    specif_knn_SPLSDA,
    specif_ann_boruta,
    specif_ann_lasso,
    specif_ann_pca,
    specif_ann_SPLSDA,
    specif_svml_boruta,
    specif_svml_lasso,
    specif_svml_pca,
    specif_svml_SPLSDA,
    specif_svmr_boruta,
    specif_svmr_lasso,
    specif_svmr_pca,
    specif_svmr_SPLSDA,
    specif_nb_boruta,
    specif_nb_lasso,
    specif_nb_pca,
    specif_nb_SPLSDA,
    specif_rf_boruta,
    specif_rf_lasso,
    specif_rf_pca,
    specif_rf_SPLSDA,
    sensit_msgl_lasso
  ),
  Sensitiv = c(
    sensit_knn_boruta,
    sensit_knn_lasso,
    sensit_knn_pca,
    sensit_knn_SPLSDA,
    sensit_ann_boruta,
    sensit_ann_lasso,
    sensit_ann_pca,
    sensit_ann_SPLSDA,
    sensit_svml_boruta,
    sensit_svml_lasso,
    sensit_svml_pca,
    sensit_svml_SPLSDA,
    sensit_svmr_boruta,
    sensit_svmr_lasso,
    sensit_svmr_pca,
    sensit_svmr_SPLSDA,
    sensit_nb_boruta,
    sensit_nb_lasso,
    sensit_nb_pca,
    sensit_nb_SPLSDA,
    sensit_rf_boruta,
    sensit_rf_lasso,
    sensit_rf_pca,
    sensit_rf_SPLSDA,
    specif_msgl_lasso
  )
)
  

# Ordenamos el datraframe por accuracy en orden descendente
df <- df_compar_mod %>%
  arrange(desc(Accuracy)) 
#%>%
# kable(caption = "Tabla comparativa de las distintas métricas de modelos con los datos tests") %>%
# kable_styling(
#   bootstrap_options = c("striped", "hover"),
#   full_width = F
# )


# Convertir el DataFrame en una tabla con kable()
tabla <- kable(df, caption = "Tabla comparativa de las distintas métricas de modelos con los datos tests")

# Personalizar la tabla:
tabla <- tabla %>% 
  kable_styling(bootstrap_options = "striped")

# Personalizar la tabla para archivo PDF con la función kableExtra::kableExtra()
tabla <- kableExtra::kable_styling(tabla, latex_options = c("striped"))
  

# Export PDF y a csv
pdf(here("Tables/compatativa_modelos.pdf"), width = 15, height = 20)       #
grid.table(df)
dev.off()
export(df, (here("Tables/compatativa_modelos.csv")))



# Gráficos comparativa error test modelos ---------------------------------


# Paleta de colores para poder diferenciar los 25 modelos
colores <- c(
  brewer.pal(n = 8, name = "Accent"),
  brewer.pal(n = 9, name = "Set1"),
  brewer.pal(n = 9, name = "Spectral")
) %>%
  unique()


# subset dataset con los modelos y el accuracy test en orden descendente
accuracy_test <-data.frame(modelo = df$Modelo, accuracy_test = df$Accuracy) %>%
  arrange(desc(accuracy_test))


# Tabla con los modelos y los accuracys de validación y test
print(metricas_predicciones <- metricas_resamples %>% 
        group_by(modelo, metrica) %>% 
        summarise(media = mean(valor)) %>%
        spread(key = metrica, value = media) %>%
        dplyr::select(accuracy_validacion = Accuracy) %>%
        full_join(accuracy_test, by = "modelo") %>%
        ungroup() %>% 
        arrange(desc(accuracy_test)), n=25)


# Export PDF y a csv
pdf(here("Tables/accuracy_val_test_modelos.pdf"), width = 15, height = 20)       #
grid.table(metricas_predicciones)
dev.off()
export(metricas_predicciones, (here("Tables/accuracy_val_test_modelos.csv")))



# VISUALIZACION modelos y su accuracy para validación y test

# Gráfico con los accuracy entrenamiento y test
metricas_predicciones %>%
  gather(key = "datos", value = "accuracy", -modelo) %>%
  ggplot(aes(
    x = fct_reorder(modelo, accuracy),
    y = accuracy,
    color = datos,
    label = round(accuracy, 2)
  )) +
  geom_point(size = 8) +
  scale_color_manual(values = c("red3", "turquoise3")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.16, linetype = "dashed", color="blue") +
  annotate(geom = "text", y = 0.27, x = 23.5, label = "Accuracy basal") + 
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test",
       x = "Modelo") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(here("Plots", "accuracy_train_test_plot.png"), width = 7.69, height = 8.25)


# gráfico con los accuracy test por modelo
metricas_predicciones %>%
  dplyr::select(-accuracy_validacion) %>% 
  ggplot(aes(x = reorder(modelo, accuracy_test), y = accuracy_test, label = round(accuracy_test, 2))) +
  geom_segment(aes(x = reorder(modelo, accuracy_test), y = 0,
                   xend = modelo, yend = accuracy_test),
               color = "grey50") +
  geom_point(size = 8, color = "firebrick") +
  geom_text(color = "white", size = 3, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.16, linetype = "dashed", color="blue") +
  annotate(geom = "text", y = 0.27, x = 23.55, label = "Accuracy basal") +
  labs(title = "Validación: Accuracy test",
       subtitle = "Modelos ordenados Accuracy",
       x = "modelo") +
  coord_flip() +
  theme_bw()

# Guardamos plot
ggsave(here("Plots", "models_Test_Accuracy.png"), width = 7.69, height = 8.25)


# gráfico barplot con los accuracy test por modelo
metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ungroup() %>% 
  add_row(modelo = "ANN_BOR", media = mean(modelo_ann_boruta$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_LASSO", media = mean(modelo_ann_lasso$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_PCA", media = mean(modelo_ann_pca$metrics$val_accuracy)) %>%
  add_row(modelo = "ANN_SPLSDA", media = mean(modelo_ann_splsda$metrics$val_accuracy)) %>%
  add_row(modelo = "MSGL_LASSO", media = accur_msgl_lasso) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_bar(stat = "identity", fill = "firebrick") +
  geom_text(color = "white", size = 3,  nudge_y = -0.03, fontface = "bold") +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.16, linetype = "dashed", color="black") +
  annotate(geom = "text", y = 0.28, x = 22, label = "Accuracy basal") +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()

# Guardamos plot bars plot
ggsave(here("Plots", "models_Test_Accuracy_barplot.png"), width = 7.69, height = 8.25)


# Gráfico Parallel coordinate plot comparación métricas modelos 
df_compar_mod %>% 
  dplyr::select(-c(Lower_Ac, Upper_Ac)) %>% 
  gather(key = "medida", value = "valor", -Modelo) %>% 
  ggplot(data = ., aes(x = medida, y = valor, color = Modelo)) +
  geom_point(size = 4) +
  geom_line(aes(group = Modelo)) +
  scale_color_manual(values = colores) +
  labs(title = "Comparación de métricas de modelos",
       subtitle = "Testing") +
  theme_test() +
  theme(legend.position = "bottom")

# Gráfico comparación métricas modelos
ggsave(here("Plots", "Model_metrics_plot.png") , width = 7.69, height = 8.25)


# Gráficos metricas por modelo y tipo de Feature selection ----------------



# vector con los modelos
key_modelos_names <- c("KNN", "ANN", "SVML", "SVMR", "NB", "RF") 

# Función para crear un gráfico Parallel coordinate plot para cada modelo
metric_plot <- function(key_modelo) {
 
# detectamos en el dataframe long, cada subconjunto de modelos según FS y graficamos.   
df_compar_mod %>% 
  dplyr::select(-c(Lower_Ac, Upper_Ac)) %>% 
  gather(key = "medida", value = "valor", -Modelo) %>% 
  filter(str_detect(Modelo,key_modelo)) %>% 
  ggplot(data = ., aes(x = medida, y = valor, color = Modelo)) +
  geom_point(size = 4) +
  geom_line(aes(group = Modelo)) +
  scale_color_manual(values = brewer.pal(n = 9, name = "Set1")) +
  labs(title = glue("Comparación de métricas del modelo {key_modelo}"),
       subtitle = "Para las distintas técnicas de Feature Selection") +
  theme_test() +
  theme(legend.position = "bottom")

# Guardamos el plot 
ggsave(here("Plots", glue("{key_modelo}_Model_metrics_by_fs_plot.png")), width = 7.69, height = 8.25)
}

# función para pasar cada indice del vector de modelos por la funcion metric plot
map(key_modelos_names,metric_plot)


# Gráfico Parallel coordinate plot comparación métricas modelos
# entre los 4 mejores modelos

df_compar_mod %>%
  dplyr::select(-c(Lower_Ac, Upper_Ac)) %>%
  gather(key = "medida", value = "valor",-Modelo) %>%
  filter(
    str_detect(Modelo, "RF_BOR") |
      str_detect(Modelo, "RF_LASSO") |
      str_detect(Modelo, "ANN_BOR") |
      str_detect(Modelo, "ANN_LASSO")
  ) %>%
  ggplot(data = ., aes(x = medida, y = valor, color = Modelo)) +
  geom_point(size = 4) +
  geom_line(aes(group = Modelo)) +
  scale_color_manual(values = brewer.pal(n = 9, name = "Set1")) +
  labs(title = "Comparación de métricas de los modelos Ensemble (RF Y ANN)",
       subtitle = "Para las distintas técnicas de Feature Selection") +
  theme_test() +
  theme(legend.position = "bottom")

ggsave(here("Plots", glue(
  "{RF_ANN}_Model_metrics_by_fs_plot.png"
)), width = 7.69, height = 8.25)




# Plot matriz confusión mejores modelos --------------------------------------

# Función para graficar matrices de confusión.
# Extraida y adaptada de:
# https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot

confusionMatrixPlot <- function(MatrizConfusion, NombreAlgoritmo) {
  # tabla con la matriz de confusión
  table <- data.frame(MatrizConfusion$table)
  
  # tabla para el ggplot con las nuevas variables resultado y proporción
  plotTable <- table %>%
    mutate(Resultado = ifelse(table$Prediction == table$Reference, "Acierto", "Fallo")) %>%
    group_by(Reference) %>%
    mutate(prop = Freq / sum(Freq))
  
  # función ggplot para crear la matriz de confusión en base a la tabla anterior
  # llenar alfa en relación con la sensibilidad/especificidad por resultados
  # proporcionales dentro de los grupos de referencia
  ggplot(
    data = plotTable,
    mapping = aes(
      x = Reference,
      y = Prediction,
      fill = Resultado,
      alpha = prop
    )
  ) +
    geom_tile() +
    geom_text(
      aes(label = Freq),
      vjust = .5,
      fontface  = "bold",
      alpha = 1
    ) +
    scale_fill_manual(values = c(Acierto = "green", Fallo = "red")) +
    theme_bw() +
    theme(
      title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 9, face = "bold")
    ) +
    xlim(levels(table$Reference)) +
    ylim(rev(levels(table$Reference))) +
    scale_x_discrete(position = "top") +
    labs(
      title = glue("Algoritmo:{NombreAlgoritmo}"),
      x = "Actual",
      y = "Predicción",
      alpha = "Proporción"
    )
}


# plotsMatriz confusión 
plot_conf_rf_boruta <-
    confusionMatrixPlot(MatrizConfusion = conf_rf_boruta, " Random forest con Boruta")

ggsave(here("Plots", "plot_conf_rf_boruta.png"))

plot_conf_rf_lasso <-
  confusionMatrixPlot(MatrizConfusion = conf_rf_lasso, " Random forest con Lasso")

ggsave(here("Plots", "plot_conf_rf_lasso.png"))

plot_conf_ann_boruta <-
  confusionMatrixPlot(MatrizConfusion = conf_ann_boruta, " Neural network con Boruta")

ggsave(here("Plots", "plot_conf_ann_boruta.png"))


# Model ensembling (stacking) ---------------------------------------------

# Función para calcular la moda y el ensemble stacking
# extraído y adaptado de: Cienciadedatos.net
# https://www.cienciadedatos.net/documentos/42_clasificacion_de_tumores_con_machine_learning

moda <- function(x, indice_mejor_modelo){
  tabla_freq <- table(x)
  freq_maxima <- max(tabla_freq)
  if(sum(tabla_freq == freq_maxima) > 1) {
    # En caso de empate, se devuelve la predicción que
    # ocupa el índice del mejor modelo
    return(x[indice_mejor_modelo])
  }
  return(names(which.max(table(x))))
}


nombres_modelos <- predicciones  %>% dplyr::select(RF_BOR, RF_LASSO, ANN_BOR, ANN_LASSO) %>% colnames()

predicciones_ensemble <- predicciones %>%
  dplyr::select(all_of(nombres_modelos)) %>%
  mutate(Clasificacion = apply(
    X = dplyr::select(.data = predicciones,
               all_of(nombres_modelos)),
    MARGIN = 1,
    FUN = moda,
    indice_mejor_modelo = 1
  ))

# Vemos cabecera
predicciones_ensemble %>% head()

# Export PDF
pdf(here("Tables/Resultado_clasificacion_models.pdf"), width = 30, height = 24)       
grid.table(predicciones_ensemble)
dev.off()

# Export csv

export(predicciones_ensemble, (here("Tables/Resultado_clasificacion_models.csv")))

# Export EXCEL
wb <- createWorkbook()

# Agregar una hoja al libro
addWorksheet(wb, "Predicciones")

# Escribir la tabla en la hoja
writeDataTable(wb, sheet = "Predicciones", x = predicciones_ensemble)

# Guardar el libro de Excel
saveWorkbook(wb, here("Tables/Resultado_clasificacion_models.xlsx"), overwrite = TRUE)




#  Matriz confusión ensemble 

predicciones_ensemble$Clasificacion

testdfBoruta <- readRDS(here("Data/FS_datasets/BORUTA/datos_test_boruta.rds"))
conf_ensemble<- confusionMatrix(as.factor(predicciones_ensemble$Clasificacion), testdfBoruta$Labels)

plot_conf_ensemble <-
  confusionMatrixPlot(MatrizConfusion = conf_ensemble, " Stacking Ensemble")

ggsave(here("Plots", "plot_conf_ensemble.png"))
