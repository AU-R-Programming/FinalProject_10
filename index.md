
---
title: "Binary Classification with the Logistic Regression Package"
author: "Destanie Pitt, Hayden Billmann, Ashton Wise"
date: "`r Sys.Date()`"
output: 
  rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Binary Classification with Logistic Regression}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# Introduction
This vignette demonstrates the usage of the logistic regression package with the provided `adult` dataset. The binary classification problem involves predicting whether a person earns `<=50K` or `>50K` based on various demographic and work-related variables.

# Installation

To install the package from GitHub, use:

```{r, eval=FALSE}
# Install devtools if not already installed
install.packages("devtools")

# Install the package
devtools::install_github("AU-R-Programming/FinalProject_10")
```

# Workflow
The workflow consists of the following steps:
1. Load the dataset.
2. Preprocess the data.
3. Fit a logistic regression model using selected predictors.
4. Compute confidence intervals for the coefficients.
5. Evaluate the model using performance metrics.

## Load and Preprocess the Dataset
We load the dataset and preprocess it by encoding the target variable (`NA`) as a binary numeric variable (`0` for `<=50K`, `1` for `>50K`).

```{r, eval=FALSE}
# Load necessary functions
library(Estimater)

# Load the dataset
data <- read.csv("adult(1).csv", sep = ";", quote = '"')

# Preprocess the dataset
data$Income <- as.numeric(data$"NA" == " >50K")  # Encode the target variable as binary (0/1)
```

## Fit the Logistic Regression Model
We select a subset of predictors (`age`, `education.num`, `hours-per-week`, and `capital-gain`) and fit a logistic regression model.

```{r, eval=FALSE}
# Select predictors and response variable
X <- as.matrix(data[, c("age", "education.num", "hours.per.week", "capital.gain")])
X <- cbind(1, X)  # Add intercept column
y <- data$Income

# Validate predictors and response
if (any(is.na(X))) stop("Error: X contains NA values.")
if (length(unique(y)) != 2) stop("Error: Response variable must be binary.")

# Fit the model
beta_hat <- fit_model(X, y)
print("Optimized Coefficients:")
print(beta_hat)
```

## Bootstrap Confidence Intervals
We compute confidence intervals for the estimated coefficients using bootstrapping.

```{r, eval=FALSE}
# Bootstrap confidence intervals
B <- 50
alpha <- 0.05
confidence_intervals <- bootstrap(X, y, B, alpha)
print("Bootstrap Confidence Intervals:")
print(confidence_intervals)
```

## Evaluate Model Performance
We compute the confusion matrix and key performance metrics using the `compute_metrics` function.

```{r, eval=FALSE}
# Compute metrics
metrics <- compute_metrics(X, beta_hat, y)
print("Model Performance Metrics:")
print(metrics)
```

## Complete Logistic Regression Pipeline
We run the full logistic regression pipeline.

```{r, eval=FALSE}
# Run logistic regression pipeline
results <- logistic_regression_pipeline(
  n = nrow(X),
  lambda = 1,
  beta_true = rep(0, ncol(X)),
  B = 50,
  alpha = 0.05
)

```

# Conclusion
This vignette illustrates how to use the package with a real dataset to perform logistic regression, compute confidence intervals, and evaluate performance metrics. For more details on individual functions, refer to the function documentation.
