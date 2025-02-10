# SixSigma-Health-Bibliometrics

This repository contains the R scripts used for the bibliometric analysis of **Six Sigma in healthcare**, categorized by **World Bank income groups**, and their associations with **health, research, and expenditure indicators from the World Bank and WHO Global Observatory on Health R&D**.

## Purpose
This repository provides a reproducible workflow for analyzing research on Six Sigma in healthcare, integrating bibliometric metrics (publications, H-index) with global health indicators. The analyses aim to:

- Explore the relationship between **bibliometric metrics** and **health, research, and expenditure indicators**.
- Evaluate income group differences in research output related to Six Sigma.
- Perform **linear regression models** for each World Bank income group.
- Conduct **meta-analysis** to estimate pooled effects across income groups.
- Visualize associations using **hierarchical clustering** and **heatmaps**.

## Required R packages
The following R packages are necessary to execute the analyses:
- **dplyr**
- **tidyr**
- **readxl**
- **pheatmap**
- **ggplot2**
- **ggsci**
- **metafor**
- **wbstats**

## Analyses included
This script performs the following analyses:

1. **Data loading and cleaning**  
   - Reading bibliometric data and health indicator datasets (stored in `/data/`).  
   - Standardizing indicator formats and income group classifications.

2. **Data summarization**  
   - Aggregating total **publications and H-index** by income group and year.  
   - Computing **average values** for each health indicator per income group-year.

3. **Linear regression models**  
   - Evaluating associations between **health indicators** and **bibliometric metrics**.
   - Constructing separate **income group models** for each World Bank category.  
   - Reporting only significant models while making all results available.

4. **Hierarchical clustering and heatmaps**  
   - Creating a **regression coefficient matrix** with Z-score normalization.  
   - Performing **clustering analysis** to group related indicators.  
   - Generating **heatmaps** with significance annotations.

5. **Meta-analysis**  
   - Applying **random-effects meta-analysis** to summarize associations across income groups.  
   - Estimating pooled regression coefficients with the **Restricted Maximum Likelihood (REML)** method.  

6. **Result extraction and reporting**  
   - Extracting **significant models** based on p-values.  
   - Formatting results for **publication and supplementary materials**.  
   - Storing output tables in structured CSV files.

## Data availability
This repository includes the following preprocessed health indicators from the WHO Global Observatory on Health R&D:
- **Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country**
- **Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)**
- **Health researchers (in full-time equivalent), as a proportion of all researchers**

All other indicators are retrieved dynamically using the **World Bank API** via the `wbstats` R package.

**Bibliometric data on Six Sigma research articles is available upon reasonable request.**

## Folder structure
The repository is organized as follows:

```
/SixSigma-Health-Bibliometrics
│── main_analysis.R         # Main R script for all analyses
│── /indicators/                  # Preprocessed health indicators from WHO
│── README.md               # Documentation
```

## License
This repository is licensed under the **MIT License**, allowing free use, modification, and distribution with attribution. See the `LICENSE` file for more details.
