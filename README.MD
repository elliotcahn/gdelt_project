# GDELT Data Analysis (2018-2022)

This project analyzes the relationship between global event intensity (**GoldsteinScale**) and media sentiment (**AvgTone**) using 5 years of GDELT data. We used HTCondor to parallelize the processing of thousands of daily log files.

## File Description

### 1. Data Processing
These files are used to download and clean data on the CHTC cluster.
* **`job_list.txt`**: A list of 60 tasks (one for each month from 2018-01 to 2022-12).
* **`gdelt_etl.sub`**: The submission file that sends these 60 jobs to the cluster.
* **`gdelt_wrapper.sh`**: Sets up the Docker environment (R + Tidyverse) on the worker nodes.
* **`process_month.R`**: The main script. It downloads one month of daily data, filters out bad years, and calculates monthly averages.

### 2. Analysis & Modeling
These files run locally to merge results and test hypotheses.
* **`analyze.R`**: Merges the 60 separate monthly files into one final dataset.
* **`threemodels.R`**: Runs three regression models (Correlation, Time-series, Difference) on the final data.
* **`gdelt_final_2018_2022.csv`**: The final output file containing monthly data for analysis.

## Results Summary

1.  **Correlation**: There is a significant positive correlation ($\beta \approx 0.59$) between event stability and media tone.
2.  **Conflict Impact**: Physical conflicts ($\beta \approx -11.8$) lower media sentiment much more than verbal conflicts ($\beta \approx -8.3$).
3.  **Prediction**: A spike in conflict events strongly predicts a drop in media sentiment ($R^2 \approx 0.63$).

## Quick Start
1.  Submit jobs: `condor_submit gdelt_etl.sub`
2.  Merge results: `Rscript analyze.R`
3.  Run models: `Rscript threemodels.R`