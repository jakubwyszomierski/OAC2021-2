# OAC-2021/2

Creation of the **2021/2 Output Area Classification**

### Scripts

- `Metadata.R` - creating reference tables and glossaries for Census data. 
- `Downloading_data.R` – downloading 2011 and 2021 Census data from NOMIS using `nomisr` package.
- `Comparing_Censuses.R` - data cleaning and amalgamation.
- `Transforming_Census_data.R` – data manipulation and transformation for the classification.
- `NI.R` – modelling 2021 data for Northern Ireland.
- `Correlation.R` - testing correlation between the variables.
- `Pre-clustering.R` - preparing data for clustering.
- `Clustering.R` - clustering of the data.
- `Post-clustering.R` - creating maps and plots of the cluster solution.
- `Testing_clustering.R`
- `Clustergrams.ipynb` – creating Clustergrams in Python. (Credits: Prof Alex Singleton)
- `Industry.R` - loading Industry data
- `Industry_classification.R` - creating geodemographic classification with Industry variables
- `Graph_comparisons.R` - comparing data with graphs

### Data
List of folders (subfolders & files) in the project:

- **API** - Census data downloaded and saved with use of `nomisr` package.
- **Clean** – amalgamated data ready for the analysis. Downloadable via [this link](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/Eqd1EV2WgOFJmZ7kLx-oDYMBdxqNe9IJmli6M8S-e91F0g?e=M9wh5j).
  - Raw_counts - datasets with raw counts
  - Percentages - datasets transformed into percentages
  - Transformed - datasets transformed with IHS (analysis-ready)
  - Final_variables - datasets with OAC variables only
  - All_data_clustering - results of the clustering for all investigated datasets.
- **Clustering** - datasets with cluster assignment for the UK and centroids. 
- **Lookups** - reference tables for 2011 and 2021 Census variables.
- **NISRA 2021** - 2021 Census data at LGD level for Northern Ireland.
- **Objects** - R objects created and stored to ensure consistency of the results or load big files
- **SIR** – contingency tables on disability counts by age, utilised for calculation of Standardised Illness Ratio.
- **shapefiles** - folder containing shapefiles used for some of the calculations/ Downloadable via [this link](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/Eu4Z-haZY2dLvYvdna1PBlEB__kLPk2wHGGYJkNonhsA2w?e=VpX95K).

### Plots

Downloadable via [this link](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/Egu6pw70wcVOs9zMgOTruTEBxPuS5AuZAppQNDAqklQrGw?e=iIZ26F).

- Bar_plots - Comparison of clusters to the UK (as well as Supergroup and Group averages)
- Clustergrams – plots used to establish number of clusters at each classification level. 

### Maps

Downloadable via [this link](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/EsJBp69rZBJAjPrURNU9sWABeO2g42xOu04XZYhY7jIGAw?e=OAI0E7).
