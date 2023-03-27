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
- `Clustergram.ipynb` – creating Clustergrams in Python. 

### Data
List of of folders (subfolders & files) in the project:

- **API** - Census data downloadded and saved with use of `nomisr` package.
- **Clean** – amalgamated data ready for the analysis.
  - Raw_counts - datasets with raw counts
  - Percentages - datasets transformed into percentages
  - Transformed - datasets transformed with IHS (analysis-ready)
  - Final_variables - datasets with OAC variables only
  - Clustering - results of the clustering algorithms

- **Clustering** - folder containing cluster assignments
- **Lookups** - reference tables for 2011 and 2021 Census variables [Download here](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/EopoRyd87XpCjTWeVkQky-QBIIXWU4omlWYrTy9CA2a68g?e=aHy5I0)
- **NISRA 2021** - 2021 Census data at LGD level for Northern Ireland [Download here] (https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/EsNU1WxSz0pGkxG0OejUTD8Bz0uB7OOPWnjsLTmH-uiTjg?e=Hho4X7)
- **SIR** – contingency tables on disability counts by age, utilised for calculation of Standardised Illness Ratio. [Download here] (https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/EreqlG5R7-5PmVmDTdlAQ1YBwN1qZb-43FyTwPFksw2F7w?e=lzYp1B)


