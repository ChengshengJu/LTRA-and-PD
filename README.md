README: Analytical Code for "Effect of leukotriene receptor antagonist use on the future risk of Parkinson’s disease in older patients with asthma"

Overview
This repository contains SAS code used to conduct the primary, secondary, and sensitivity analyses for the study titled:

"Effect of leukotriene receptor antagonist use on the future risk of Parkinson’s disease in older patients with asthma"

The study emulates a target trial using real-world data to assess the association between LTRA use and subsequent risk of Parkinson’s disease (PD) among patients with asthma.

Contents
The repository includes the following SAS scripts:
Analysis_combined.sas

Required Data
The analyses were conducted using data from CPRD Aurum and GOLD To replicate the results, access to this dataset is required.

Data inputs expected:
Patient-level data with demographics, LTRA prescription records, covariates, and outcome events

Please note: Due to data governance restrictions, the dataset itself is not shared.

Software Requirements
SAS version: SAS 9.4 or later

How to Run
Update the library name in each script to match your local environment.
Ensure all macros in the macros/ folder are compiled before running the analysis scripts.
Run the scripts in the order specified above (starting from primary analysis).


