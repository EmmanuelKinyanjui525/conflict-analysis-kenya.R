# Kenya Conflict Data Analysis

This repository contains an in-depth **exploratory data analysis, visualization, and statistical modeling of conflict events in Kenya** using R. The project demonstrates data cleaning, analysis, mapping, and forecasting of conflict trends over time.

## Tools and Packages Used

- **R** programming language  
- **tidyverse** (dplyr, ggplot2, etc.) for data manipulation and visualization  
- **dplyr** for data wrangling  
- **ggplot2** for plotting  
- **GGally** for correlation analysis  
- **tseries**, **forecast**, **astsa** for time series analysis  
- **caret**, **e1071** for prediction and machine learning  

## Dataset

The dataset contains records of conflict events in Kenya, including:

- Event date, type, and sub-type  
- Actors involved  
- Location (county, sub-county, latitude, longitude)  
- Civilian targeting  
- Number of fatalities  

> **Note:** Make sure the CSV file `kenya_conflict_data.csv` is in the same folder as the R script to run the code.

## Features / Analyses

- Data cleaning and formatting  
- Frequency tables and bar charts for event types, sub-event types, and actors  
- Mapping conflict zones in Kenya by type of event and disorder  
- Statistical analysis of fatalities (per year, per event type, per disorder type)  
- Linear regression modeling to understand factors affecting fatalities  
- Time series analysis and forecasting of conflict fatalities  
- Correlation analysis between variables  

## How to Run

1. Clone or download this repository.  
2. Ensure the dataset CSV file is in the same directory.  
3. Open `kenya-conflict-analysis.R` in R or RStudio.  
4. Run the script line by line, or source the file to execute the full analysis.  

```r
# Example:
source("kenya-conflict-analysis.R")
