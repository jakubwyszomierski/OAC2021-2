# OAC-2021

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
- `Clustergram.ipynb` – creating Clustergrams in Python. (Credits: Prof Alex Singleton)

### Data
List of folders (subfolders & files) in the project:
Downloadable via [this link](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/EsUvZLiQsa1At6ORGJgMTIkBUzQslS52lsgZe_MjhEw-CQ?e=2DkvGy).

- **API** - Census data downloaded and saved with use of `nomisr` package.
- **Clean** – amalgamated data ready for the analysis. 
  - Raw_counts - datasets with raw counts
  - Percentages - datasets transformed into percentages
  - Transformed - datasets transformed with IHS (analysis-ready)
  - Final_variables - datasets with OAC variables only
  - All_data_clustering - results of the clustering for all investigated datasets.
- **Clustering** - datasets with cluster assignment for the UK and centroids. 
- **Lookups** - reference tables for 2011 and 2021 Census variables.
- **NISRA 2021** - 2021 Census data at LGD level for Northern Ireland. 
- **SIR** – contingency tables on disability counts by age, utilised for calculation of Standardised Illness Ratio. 

### Plots

Downloadable via [this link](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/Ejr-KcCj7qVIv4K6kTsH_90BlTw3kQvLsiP8P9k4Qjid5w?e=wEtlDz).

- Bar_plots - Comparison of clusters to the UK (as well as Supergroup and Group averages)
- Clustergrams – plots used to establish number of clusters at each classification level. 

### Maps

Downloadable via [this link](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/El562C0yOhlBlPTIKM8qx88BUfxrJM8rd3k2KlYCUeTA4g?e=UF0tZi).
