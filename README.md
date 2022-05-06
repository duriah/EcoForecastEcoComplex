# EcoForecastEcoComplex
This is the repository to the article "Forecasting in the face of ecological complexity: number and strength of species interactions determine forecast skill in ecological communities".


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6323239.svg)](https://doi.org/10.5281/zenodo.6323239)

## Repository structure

```
EcoForecastEcoComplex
|-- README.md
|-- LICENSE.txt   
|-- EcoForecastEcoComplex.Rproj
|-- Data
    |-- CCM_analysis_data
    |-- Final_dataset_all_sources
    |-- MARSS_analysis_data
    |-- Multiview_forecast_data
    |-- Smap_interaction_data
|-- R
    |-- Functions
    |-- StatisticalDataAnalysis
```

### Files and directories

- **README.md**: this file
- **LICENSE.txt**: document containing the license conditions
- **EcoForecastEcoComplex.Rproj**: R project file
- **Data**: directory containing all the data used in the study
  - **Final_dataset_all_sources**: directory containing the actual data
  - The other directories contain the results of the various analyses. They are **not** needed to reproduce the study, but considering that some analyses can take days or weeks, we provide their results as .RData files as well.
- **R**: directory containg all the code needed to reproduce the study
  - **Functions**: directory containing several R scripts in which several functions are implemented
  - **StatisticalDataAnalysis**: directory containing the R script used in the study, i.e. to analyse the data.
