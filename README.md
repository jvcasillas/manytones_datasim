# ManyTones preregistered analyses

![](https://img.shields.io/badge/R-v4.3%2B-blue)
![](https://img.shields.io/badge/Status-Active-success)
![](https://img.shields.io/badge/License-MIT-lightgrey)

Welcome to the data and modeling repository for the [ManyTones](https://chenchenzi.github.io/manytones/) project. 
This project hosts a structured pipeline to demonstrate the planned analytical pipeline for the ManyTones project, demonstrated on pilot data (n = 40).

## 📁 Repository Architecture

To ensure computational reproducibility and structural integrity, this work space is organized into the following modules:

```text
.
├── data/
│   ├── raw/                  # Immutable trial recordings and raw CSV manifests
│   ├── derived/              # Derived data frames including, cleaned data set, sample size simulation, participant mapping etc.
│   └── archived              # Previous now outdated data files, pilots etc.
├── metadata/                 
│   └──code_book_raw.txt      # Codebook for the raw data file
├── models/
│   ├── ...                   # Stored models for sample size justification
│   └── archived              # Previous stored models
├── plots/                    # High-resolution graphics derived in scripts
├── resources/                # Containing original ManyTones proposal
└── scripts/
    ├── 00_data_cleaning.qmd               # Data preparation, grouping filters, and distinct-row structuring
    ├── 01_descriptive_exploration.qmd     # Descriptive exploration and visualization of cleaned data
    ├── 02_inferential_assessment.qmd      # Model to estimate effect magnitude for sample size justification
    ├── 03_sample_size_estimation.qmd      # Simulation to estimate sample size
    ├── 04_model_comparison.qmd            # Comparing models for model assumption assessment
    ├── 05_random_forest.qmd               # Proof of concept of random forest analysis
    ├── hombert_estimates.qmd              # Extraction and projection of original descriptive values from Hombert (1975)
    └── archived                           # Previous scripts during development