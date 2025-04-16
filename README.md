
<!-- README.md is generated from README.Rmd. Please edit that file -->

# torch-for-R-gibbons

<!-- badges: start -->
<!-- badges: end -->

This repository provides a comprehensive pipeline for training,
evaluating, and deploying convolutional neural networks (CNNs) for
gibbon call detection using the `torch` and `gibbonNetR` packages in R.
It includes data preprocessing, model training, evaluation, and
deployment across a range of configurations and test conditions.

## Repository Structure

### R Scripts Overview

- **Part 1a. Variability Benchmarking Results.R**  
  Processes model performance metrics across different configurations to
  assess variability.

- **Part 1b. Evaluate Variability Benchmarking Results.R**  
  Analyzes and visualizes variability in model performance.

- **Part 2a. Train CNNs Over Multiple Epochs.R**  
  Trains CNN models over multiple epochs for performance benchmarking.

- **Part 2b. Train CNNs Over Multiple Epochs.R**  
  Continuation or alternative run of multi-epoch training using
  different parameters or models.

- **Part 3a. Data Augmentation.R**  
  Performs data augmentation on spectrograms to enrich the training
  dataset.

- **Part 3b. Evaluation Data Augmentation.R**  
  Evaluates how data augmentation impacts model performance.

- **Part 3c. Evaluation Data Augmentation on Different Test Set.R**  
  Tests the augmented models on a novel test set to assess
  generalization.

- **Part 4a. MultiSpecies Gibbon Comparison BirdNET CLI**  
  Runs BirdNET CLI to generate multispecies comparison results for
  Crested and Grey Gibbons.

- **Part 4b. Comparison with BirdNET.R**  
  Compares internal CNN model outputs with BirdNET predictions.

- **Part 5. Final Model Performance.R**  
  Extracts and summarizes final model performance statistics including
  F1, AUC, and threshold selection.

- **Part 6. Deploy Model Over PAM Data.R**  
  Applies the trained models to passive acoustic monitoring (PAM)
  datasets.

- **Part 7. Call Density Plots.R**  
  Generates visualizations of call densities across sites and times.

## Getting Started

### Prerequisites

Ensure the following packages are installed: install.packages(c(
“torch”, “dplyr”, “tidyr”, “ggplot2”, “stringr”, “viridis”, “flextable”,
“readr” ))

For detailed usage instructions and examples, refer to the gibbonNetR
documentation (<https://denajgibbon.github.io/gibbonNetR/>).

Current citation: Clink, Dena J., et al. “Automated detection of gibbon
calls from passive acoustic monitoring data using convolutional neural
networks in the” torch for R” ecosystem.” arXiv preprint
arXiv:2407.09976 (2024).
