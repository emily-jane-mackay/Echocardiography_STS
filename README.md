# This folder contains code necessary to reproduce results in the article
#### Please note: the data required for this analysis is owned by the Society of Thoracic Surgeons and is not publicly available

## Directory: STS/Data_cleaning contains the STATA code the loads the original dataset, cleans the data, defines covariates (independent, dependent, and outcomes), and finalizes the cohort

## Directory: STS_code/code contains R code that preprocesses the raw data, makes the Table One, and conducts exploratory data analyses. It also contains code that fits a propensity score model using a standard generalized linear model with ridge penalty.

## Directory STS_code/within_equivocal_surgeon_match contains R code necessary to reproduce the statistical match, balance table, and outcome analyses for the within-equivocal-surgeons match. 

## Directory STS_code/within_surgeon_match contains R code necessary to reproduce the statistical match, balance table, and outcome analyses for the within-all-surgeons match. 

## Directory STS/all_data_match contains R code necessary to reproduce the statistical match, balance table, and outcome analyses for the all-patient match. 

#### Please email any questions to bozhan@wharton.upenn.edu or emily.mackay@pennmedicine.upenn.edu.