# OAC-2021

Creation of the **2021/2 Output Area Classification**

### Scripts

- `Metadata.R` - creating reference tables and glossaries for Census data. 
- `Downloading_data.R` – downloading 2011 and 2021 Census data from NOMIS using `nomisr` package.
- `Comparing_Censuses.R` - data cleaning and amalgamation.
- `Transforming_Census_data.R` – data manipulation and transformation for the classification.
- `NI.R` – modelling 2021 data for Northern Ireland.
- `Correlation.R` - testing correlation between the variables
- `Pre-clustering.R` - 
- `Clustering.R`
- `Post-clustering.R`
- `Correlation.R`

- `Clustergram.ipynb` – creating Clustergrams in Python. 

### Data

List of of folders (subfolders & files) in the project:

- **API**
- **Clean** – amalgamated data ready for the analysis.
  - Raw_counts - datasets with raw counts
  - Percentages - datasets transformed into percentages
  - Transformed - datasets transformed with IHS (analysis-ready)
  - Final_variables - datasets with OAC variables only
  - Clustering - results of the clustering algorithms
- **Lookups** - reference tables for 2011 and 2021 Census variables [Link here](https://liveuclac-my.sharepoint.com/:f:/g/personal/zcfajwy_ucl_ac_uk/EopoRyd87XpCjTWeVkQky-QBIIXWU4omlWYrTy9CA2a68g?e=aHy5I0)
- **NISRA 2021** - 2021 Census data at LGD level for Northern Ireland
- **Objects** - `R` objects saved in the process of the analysis. 
  - `Aged_Scotland_8_class_2k.RDS`
- **SIR** – contingency tables on disability counts by age, utilised for calculation of Standardised Illness Ratio.


2021 Census tables are stored in specific subfolders for different administrative levels.

- `Census2021_wide.csv` – All Census tables from the selected administrative level merged 
- `Tables_metadata.csv` – Metadata for all Census 2021 tables created by the ONS
- `Topic_summaries_metadata.xlsx` – Metadata for Topic summaries provided by ONS (includes population bases and reference to 2011 Census) 
- `Variables_metadata.csv` – Metadata for all variables created from the Census 2021


`API` folder contains data at different administrative levels downloaded and saved with the use of `nomisr` package.

