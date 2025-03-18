# Evaluating RMSE of Linear Models Under Varying Conditions

This project uses R to systematically analyze the Root Mean Squared Error (RMSE) of linear regression models under different scenarios. Specifically, it investigates how dataset size, predictor correlation, and multicollinearity influence model accuracy.

## Project Overview

- **RMSE Stability Analysis**: Examines how the variability of RMSE changes with increasing dataset sizes.
- **Correlation Impact**: Studies how increasing the correlation between predictor and response variables affects RMSE.
- **Multicollinearity Analysis**: Explores how using correlated vs. independent predictors influences the predictive performance of linear models.

## Key Findings

- **Larger Dataset Stability**: RMSE becomes increasingly stable (lower variability) with larger datasets.
- **Correlation and Accuracy**: Higher correlation between predictors and outcomes significantly improves model accuracy, reducing RMSE.
- **Multicollinearity Effects**: Independent predictors improve accuracy substantially, whereas highly correlated predictors do not notably enhance performance, potentially causing multicollinearity issues.

## Technologies Used

- R programming language
- tidyverse package for data manipulation
- caret package for data partitioning
- MASS package for generating multivariate normal data

## Getting Started

To replicate this analysis:

1. Clone this repository.
2. Ensure R and the required packages (`tidyverse`, `caret`, `MASS`) are installed.
3. Run the provided R scripts.

## Author

- Jasmine Zhang
