# WiDS-Datathon-2021

This repository contains the group R scripts for the Women in Data Science Datathon 2021.
More information about the Datathon can be found on Kaggle: [widsdatathon2021](https://www.kaggle.com/c/widsdatathon2021/).
Datasets were downloaded from [widsdatathon2021/data](https://www.kaggle.com/c/widsdatathon2021/data)

# Description of scripts
The repository includes two workstreams, one for data cleaning,  explorative data analsyis and predictions (1 to 6) and one for data description, and more automated processing workflow using the [caret package](https://cran.r-project.org/web/packages/caret/vignettes/caret.html) in R.
Both workstreams use functions from the functions.R script and assume data is available from a download_dat folder within the repository (not version controlled).

## Data cleaning, EDA and predictions
- `01- dataProcessing.R`
- `02 - FeaturingSelection.R`
- `03 - Creatingmodel.R`
- `04-ScoreModel.R`
- `05 - Evaluating model.R`
- `06 - Predicting with unlabel dataset.R`

## Caret workflow
- `data_processing.R`
- `target_predictions.R`



