
<!-- README.md is generated from README.Rmd. Please edit that file -->
# StreetEasy

*SDS 410 Capstone in Statistical & Data Sciences, Spring 2021, Smith College*

Semester-long project in partnership with StreetEasy investigating predicting New York real estate prices using natural language processing and machine learning algorithms

-   **Team:** Lauren Low, Dayana Meza, Emma Scott, Xian (Elaine) Ye, Yanwan Zhu
-   **Project Partner:** Yipeng Lai @ StreetEasy
-   **Faculty Mentor:** Prof. Ben Baumer

### Files

-   **data** - Create this folder within working directory to store .csv files of data
-   **paper\_MDPI** - Folder containing final paper **paper\_MDPI.Rmd** and related files like **figures.Rmd** and **mybibfile.bib**
-   **pre-processing.R** - Data cleaning script to prepare sale\_listings for modeling: filter out unreasonable values, join with `zipcodeR` data, remove duplicate listings, impute NA values, split data into training and test sets
-   **random\_forest.Rmd** - Random forest machine learning model using existing variables and text-based variables created from `listing_description`; also includes visualizations of model error
-   **text\_processing.R** - Natural language processing script that creates binary keyword variables and performs `AFINN` sentiment analysis

### Instructions

1.  In working directory, create **data** folder containing **amenities.csv**, **documentation - amenities.csv**, **documentation - sale\_listings.csv**, **sale\_listings.csv**
2.  Run **pre-processing.R** to load script functions into global environment
3.  Run **text\_preprocessing.R** to load script functions into global environment
4.  Run **random\_forest.Rmd** to generate random forest model and corresponding model error visualizations
