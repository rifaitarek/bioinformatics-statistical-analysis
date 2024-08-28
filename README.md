# Dental Growth Analysis in R

This repository contains an R script that performs statistical analysis on growth data over time. The analysis focuses on exploring the relationship between age, sex, and growth measurements using various statistical methods. The dataset includes measurements taken at different ages and across different sexes.

## Table of Contents
- [Introduction](#introduction)
- [Data](#data)
- [Analysis](#analysis)
- [Results](#results)
- [Requirements](#requirements)
- [Usage](#usage)
- [License](#license)

## Introduction
The goal of this analysis is to understand how growth measurements vary with age and sex using R. The analysis includes descriptive statistics, data visualization, linear regression, and mixed-effects models.

## Data
The dataset used for this analysis is `growth_txt.txt`, which includes the following variables:
- `IDNR`: Identifier for each participant.
- `AGE`: Age in months at the time of measurement.
- `SEX`: Sex of the participant (1 for male, 2 for female).
- `MEASURE`: Growth measurement value.

## Analysis
The analysis performed includes:
- **Spaghetti Plots**: Visual representation of individual growth trajectories over time.
- **Descriptive Statistics**: Calculation of mean, standard deviation, variance, and frequency of measurements by age and sex.
- **Boxplots**: Visualization of the distribution of growth measurements across ages and sexes.
- **Correlation Analysis**: Correlation between growth measurements at different ages.
- **Linear Regression**: Regression analysis to determine the relationship between age and growth measurement.
- **Mixed-Effects Models**: Advanced statistical models to account for both fixed and random effects in the data.

## Results
- Visualizations showing individual growth trajectories and mean growth trends by sex.
- Descriptive statistics summarizing the growth measurements.
- Linear regression coefficients for each participant.
- Mixed-effects models indicating the significance of age and sex on growth.

## Requirements
To run the analysis, you'll need the following R packages:
- `lme4`
- `lattice`
- `reshape2`

Install them with:
```R
install.packages(c("lme4", "lattice", "reshape2"))
