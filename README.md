
### RNA-Seq Cancer Classification with Machine Learning

Welcome to the RNA-Seq cancer classification project! This repository features a detailed pipeline for classifying cancer types using machine learning techniques. The project spans from data extraction to preprocessing, model training, and evaluation, showcasing practical applications of data science in genomics. Additionally, feature selection algorithms have been implemented to address the high dimensionality of gene data, and exploratory analysis using unsupervised algorithms like t-SNE and UMAP has been conducted to understand the dataset's structure. The resulting model, Random Forest with Boruta, is integrated into the [CanceRClassif Webapp for Cancer Classification](https://github.com/estebancalle/Cancerclassif), a web application designed to classify and detect tumors in RNA-Seq tissue samples.

---

### Project Overview

This project leverages RNA-Seq data from The Cancer Genome Atlas (TCGA) to classify cancer types using machine learning. It includes two classification pipelines:

- **Binary Classification**: Distinguishes between tumor and normal samples.
- **Multiclass Classification**: Identifies 15 different types of tumors.

Both pipelines follow similar steps with minor adjustments tailored to each classification task.

---

### Pipelines and Key Steps

Each pipeline is divided into nine distinct steps, implemented as R scripts. Below is a summary of each step:

1. **Setup and Data Extraction**
   - Environment setup and subdirectory creation.
   - RNA-Seq data download from TCGA using TCGAbiolinks.
   - Data restructuring and saving in CSV and RDS formats.
   - Random sampling of datasets.
   - Handling specific cancer types for binary and multiclass tasks.

2. **Exploratory Data Analysis (EDA)**
   - Summary tables and bar charts of sample counts per tumor type.
   - Missing value detection and visualization.

3. **Train-Test Split**
   - Stratified partitioning of data (80% train, 20% test).
   - Calculation of baseline accuracy using the majority class predictor.

4. **Data Preprocessing**
   - Filtering of irrelevant genes (quantile < 0.25) and low-variance predictors.
   - Normalization (centering and scaling).
   - Applying preprocessing steps to test data.

5. **Exploratory Clustering**
   - Clustering analysis using PCA, t-SNE, and UMAP.
   - Dendrogram and heatmap generation.

6. **Feature Selection and Extraction**
   - Selection of predictors using methods like Boruta, msgl, sPLS-DA, and PCA.
   - Venn diagram to visualize selected genes across methods.
   - ![Venn Diagram](link-to-venn-diagram-image)

7. **Model Training**
   - Training various models: ANN, NB, RF, SVM (radial and linear).
   - Evaluation using predictions and graphical analysis.
   - Notes on setting up environments for ANN training using Keras and TensorFlow.

8. **Model Comparison and Ensemble**
   - Loading models and generating predictions.
   - Comparative analysis of models using various metrics.
   - Stacking ensemble of top-performing models.

9. **Generalization Testing**
   - Testing the best model on new samples.
   - Generating confusion matrices and performance graphs.

---

### Visual Insights

The project includes several visual aids to enhance understanding:

- **Bar Chart**: Shows the proportion of samples per tumor type, highlighting dataset imbalance:
   - ![Bar Chart](https://github.com/estebancalle/tcga_cancer_classification/blob/master/Result_plots/plot_num_tumor_sampled.png)
- **t-SNE Plot**: Displays the data structure, indicating overlap among some tumor types.
   - ![t-SNE plot](https://github.com/estebancalle/tcga_cancer_classification/blob/master/Result_plots/tsne_plot.png)
- **Venn Diagram**: Illustrates gene selection differences across algorithms (Boruta, Lasso, sPLS-DA, PCA).
  - ![Venn Diagram](https://github.com/estebancalle/tcga_cancer_classification/blob/master/Result_plots/FS_Venn_Diagram.png)

### Selected Model and Future Integration

The Random Forest model with Boruta feature selection was identified as the best-performing model based on Kappa and Accuracy metrics. The resulting model, Random Forest with Boruta, is integrated into the [CanceRClassif Webapp for Cancer Classification](https://github.com/estebancalle/Cancerclassif), a web application designed to classify and detect tumors in RNA-Seq tissue samples.

---

### Execution Instructions

To run the pipelines:

1. Ensure all R scripts are in place. Each script will install necessary libraries.
2. Open the `.Rproj` file to set the working directory.
3. Execute the scripts in sequence, starting with Script 1.

**Windows Users**: Place the project folder in a root directory to avoid path length issues (e.g., `D:/pipeline/multiclass`).

**Note**: 
- Keep TCGAbiolinks documentation handy for updated parameters and Bioconductor versions.
- For training the ANN, we use KERAS and TensorFlow. You need to create a simple environment in Miniconda with Python, TensorFlow, and Keras installed. Name this environment "tf". The script will import it if it is named this way. [Installation instructions](https://docs.anaconda.com/free/anaconda/applications/tensorflow/).

---

### Conclusion

This project showcases a complete workflow for RNA-Seq data classification using machine learning, highlighting practical skills in data science and genomics. The integration of the best-performing model into an application further demonstrates the project's real-world applicability and user-centric approach. Explore the repository to see the detailed work and potential impact.

---
