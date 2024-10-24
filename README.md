# ACS_PUMS

This repository contains code and scripts to analyze American Community Survey (ACS) Public Use Microdata Sample (PUMS) data. The goal of this analysis is to measure consolidation between race and education using PUMA-level and tract-level metrics across various years of ACS data. As more data beyond 2022 becomes available, this repository will support efficient computation of social and demographic patterns at multiple geographic levels.

## Overview
The core script in this repository computes consolidation measures (Cramér’s V) to determine the association between race/ethnicity and education levels within geographic areas, specifically:

* PUMA (Public Use Microdata Areas) – large geographic areas containing approximately 100,000 people.
* Synthetic tracts – placeholder geographic units used for finer-grained analysis until real tract-level microdata becomes available.

The analysis leverages person weights from the ACS PUMS dataset to ensure population-level estimates are accurate. These consolidation measures help identify areas with educational stratification or social diversity, providing valuable insights for policy analysis and academic research.

## Repository Structure
1. Data Processing and Analysis
* `ACS_PUMS_2022.ipynb`:
  * This Jupyter notebook contains the primary workflow to compute PUMA and tract-level consolidation measures.
  * It includes the following key components:
      * PUMA-level Consolidation: Uses weighted contingency tables to calculate Cramér’s V for PUMAs.
      * Synthetic Tract Generation: Randomly assigns individuals to synthetic tracts to simulate tract-level analysis.
      * Tract-level Consolidation: Builds on synthetic tracts to compute consolidation measures across smaller geographic units.

## Function Overview
Key Functions in ACS_PUMS_2022.ipynb
1. `compute_consolidation_by_puma`
  * Groups data by PUMA and state to compute Cramér’s V for each PUMA.
  * Outputs a DataFrame containing PUMA identifiers, states, and consolidation scores.
2. `generate_synthetic_tracts`

  * Shuffles individuals within each PUMA and assigns them to synthetic tracts for simulation.
  * Ensures random and unbiased tract-level assignments for analysis.
3. `compute_tract_level_consolidation`

  * Groups data by synthetic tract, PUMA, and state to calculate tract-level consolidation scores.
  * Uses the same Cramér’s V method as the PUMA-level analysis.
4. `build_weighted_contingency_table`

  * Creates a sparse matrix to efficiently store weighted counts of race and education pairs.
5. `calculate_cramers_v`

  * Computes Cramér’s V from contingency tables to measure the association strength between race and education.

## How to Use the Code
1. Prepare the Data:

  * Load ACS PUMS data, ensuring that relevant columns like RAC1P, SCHL, PWGTP, PUMA, and state are available

2. Run PUMA-Level Consolidation:

`puma_consolidation = compute_consolidation_by_puma(df)`
`print(puma_consolidation)`

3. Generate Synthetic Tracts:

`df = generate_synthetic_tracts(df, puma_col='PUMA20', num_tracts=5)`

5. Run Tract-Level Consolidation:

`tract_consolidation = compute_tract_level_consolidation(df)`
`print(tract_consolidation)`

## Dependencies
This project requires the following Python libraries:

NumPy: For numerical operations.
Pandas: For data manipulation and analysis.
SciPy: For building sparse matrices and calculating contingency statistics.
