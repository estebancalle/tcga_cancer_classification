
Este proyecto utiliza secuencias de RNA-Seq, para clasificar tipos de cáncer utilizando Machine Learning Learning.

Todos los datos utilizados se descargaron del proyecto TCGA (https://portal.gdc.cancer.gov/), utilizando la biblioteca R `TCGAbiolinks`.

Se plantean dos pipelines distintos, como modos de clasificación:

- **Binaria**: Tumor o Normal.
- **Multiclase**: entre 15 tipos de tumores.

Ambos pipelines siguen los mismos pasos y procesos, con ligeras modificaciones para ajustar y atender cada caso.

Cada pipeline está dividido en 9 pasos (scripts). Con el objetivo final de evaluar el desempeño de los distintos algoritmos y su capacidad clasificadora y de generalización.


----------------------
## Instrucciones de ejecución:

1 - Todos los scripts están en código R. Cada uno, instalará las bibiliotecas necesarias para su ejecución
2 - Escoge el pipeline deseado y abre el archivo `.Rproj` para establecer el directorio de trabajo.
3- Selecciona el script a ejecutar. El script 1 debe haber sido ejecutado al menos una vez antes.

----------------------

##   SCRIPT 1 SET UP, DATA INPUT TCGA DATA, REESTRUCTURATION        

### Descripción

1 - Preparación del entorno de trabajo y subdirectorios 
2- Descarga con TCGA biolinks datos RNAseq de distintos tipos de Cáncer del repositorio TCGA.
3 - Reestructuración y guardado de los los datos en combinado y separado en csv y rds.
4 - Sampleo aleatorio y sin reemplazamiento muestras del dataset combinado
5 - Guarda el dataset sampleado y el 

En modo multiclase se bajan muestras tumorales de 15 proyectos distintos: BLCA, BRCA, CESC, COAD, HNSC, KIRC, KIRP, LGG, LIHC, LUAD, LUSC, PRAD,
  OV, STAD, PRAD
En modo binario se bajan muestras normales y tumorales de 13 proyectos distintos:BLCA, BRCA, CESC, COAD,
  HNSC, KIRC, KIRP, LIHC, LUAD, LUSC, PRAD, STAD, THCA

### ***Notas: 

- Para poder ejecutar el script en windows, la carpeta del proyecto debe estar en una carpeta raíz.
TCGAbiolinks extrae los datos de distintas muestras produciendo nombres de carpetas extremadamente largos.
Windows tiene una limitación de 260 carácteres para nombres de carpeta. 
Para evitar errores por sobrepasar el limite, ejecuta por ejemplo desde "D:/pipeline/multiclase".
- Revisa la documentación de TCGAbiolinks más reciente: los parámetros de la query necesarios y la versión de bioconductor funcional, suelen cambiar y actualizarse.

----------------------

##     SCRIPT 2 EDA PREPROCESSED MERGED DATA        

### Descripción

1 - Tabla número de muestras por tumor. Total y sampleo. 
2 - Diagrama de barras número de muestras por tumor
3 - Detección valores ausentes
4 - Se guardan los gráficos y tablas resultantes

----------------------  

##     SCRIPT 3 TRAIN AND TEST SPLIT       

### Descripción

1 - Partición estratificada de datos en 80% datos train, 20 % datos test. 
2 - Guardar datos train y test.
3 - Tabla de proporciones.
4 - Detección "Accuracy basal" clase mayoritaria como predictor

----------------------  
##     SCRIPT 4 PREPROCESSING TRAIN AND TEST DATA     

### Descripción 

1 - Preprocesamiento del dataset train.
2 - Se realiza filtrado genes irrelevantes -(quantil < 0.25)
3 - Identificación Y filtrado  de predictores (genes) de varianza cercana a cero
3 - Normalización: Center y scale transforman los datos y asegurar que las variables tengan la misma escala y media cero.
4 - Aplicamos el preprocesamiento a Test en base a los parámetros del preprocesamiento de train.

----------------------  

##     SCRIPT 5 CLUSTERING EXPLORATORIO      

### Descripción 

1 - Clustering exploratorio datos train: PCA,T-SNE,UMAP
2 - Generamos un dendrograma con las muestras
3 - Creamos heatmaps

----------------------  

##    SCRIPTS 6 FEATURE SELECTION AND FEATURE EXTRACTION

### Descripción 

1 - Selección de predictores (genes) con diversos métodos: Boruta, msgl: Multinomial Sparse Group Lasso, sPLS-DA, PCA.
2 - Guardamos datasets train y test con los predictores seleccionados
3- Diagrama de Venn

----------------------  

##     SCRIPTS 7 CLASSIFICATION MODELS

### Descripción 

1 - Entrenamiento de los distintos modelos en base a las distintas selecciones FS (SECTION 6).
2 - Generamos predicciones y gráficos para evaluar.
3 - Se utilizan los siguientes modelos: ANN, NB, RF, SVM RADIAL, SVM LINEAL

### ***Notas: 
Para el entrenamiento de la ANN, utilizamos KERAS y Tensorflow. Debes crear un enviroment en miniconda sencillo con la instalación de python, Tensorflow y keras. Llama a dicho environment "tf". El script lo importará si lo llamas así.
https://docs.anaconda.com/free/anaconda/applications/tensorflow/


----------------------  

##    SCRIPT 8 MODELS COMPARATION AND ENSEMBLE

### Descripción 

1 - Carga de todos los modelos, predicciones y matrices de confusión
2 - Tablas y gráficos comparativos de distintas métricas de los modelos
3 - Gráficos métricas modelo por tipo de técnica Feature selection
4 - Stacking Ensemble con una selección de los 4 mejores modelos y sus métricas de desempeño 
5 - Tabla con la decisión final del ensemble con la clasificación para muestras test en base a la moda.
6 - Gráficos de las métricas de cada modelo por tipo de técnica Feature selection


##     SCRIPT 9 TEST DE GENERALIZACIÓN 2    

### Descripción 

1 - Test de generalización con nuevas muestras (Las que no fueron seleccionados en el sampleo del paso 1) y el mejor modelo
2 - Generamos matriz de confusión y un gráfico
