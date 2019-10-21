tourism_prediction
====================
*ML competition at [Signate](https://signate.jp/competitions/13)*



1st Big Data Analysis Contest (Tourism Prediction in Japan)


---
Title: "Tourism Prediction Contest sponsored by METI"
Author: "Aakansh Gupta"
First Created: "January 25, 2015"

---

This repo contains code for [The 1st Big Data Analysis Contest] (https://datasciencelab.jp/compe/13) which was themed on "Tourism Prediction"
for 14 cities of Japan. Data was provided by METI ministry, Japan.

**OS version, Software, modules**

1. OS version: Ubuntu 14.03.3 LTS on AWS r3.8xlarge EC2 instance.
2. Language: R 3.2.2 with Rstudio server.
3. R packages used: data.table 1.9.6, fields 8.3-6, caret 6.0-64, reshape 0.8.5, plyr 1.8.3, forecast 6.2, xgboost 0.4-2, Metrics 0.1.1, ggplot2 2.0.0.


**Repository Structure**

1. /code/ contains R code files
2. /raw_data/ contains contest data  + open data
3. kanazawa_gtrends.xlsx contains explanation of kanazawa google trends keywords, categories, filenames etc. This is a reference to column names for google trends data of Kanazawa inside /raw_data/open_data/kanazawa/
4. Run RunMe.R to reproduce the model.

**HOW TO RUN CODE:**

1. Download the repository. (It contains code and raw data).
2. Install the required R packages as mentioned in RunMe.R
3. Inside RunMe.R modify the main_path variable to the path of this repository.
4. Run RunMe.R
6. A submission_ensemble.csv is generated containing final predictions for Toyama City (C6) and Kanazawa (C7)

*Details about implementation in English are mentioned in report_02_ENGLISH.pdf*

*Details about implementation in Japanese are mentioned in report_02_日本語.pdf*

