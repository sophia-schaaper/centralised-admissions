README
=======================

# centralised-admissions

This repository contains all data processing scripts and relevant
datasets for my capstone dissertation project on the impact of
centralized high school admissions systems on high school segregation in
major U.S. cities.

## Data Source 

All raw data come from the **NCES Common Core of Data (CCD)**, accessed via ELSi and the CCD files portal. File structure varies by period:

- 1992-1997: Retrieved via ELSi multi-year CSV exports at the school level. (Used here as pre-treatment context; our export did not include grade-by-race counts, so the pipeline harmonizes pre-1998 years accordingly).

- 1998-2006: Single 'School Universe' file per year, split into three state groups (with states names starting with a-i, k-n, and o-w).

- 2007-2013: State groups were removed, one single national school universe file per year, with school directory and membership information within the same file.

- 2014-2015: Transition to component files per year (continued until today):
  - `029`: Directory file.
  - `052`: Membership file.
  - `033`: Lunch Program Eligibility file.

The raw data used in this project is processed and cleaned in `code/process_data` which produces a clean school-grade-level race count dataset for each year, which can be found in `data/clean`, and then aggregates them into our main dataset `data/school_count_data` which is used to run our analyses.
The raw data, however, is not provided in this repo due to size constraints. You can access it here: 

**Source**: National Center for Education Statistics (NCES). *Common Core of Data (CCD), Public Elementary/Secondary School Universe (Nonfiscal) Data Files, 1992–2023*. U.S. Department of Education, Institute of Education Sciences. [Retrieved via ELSi and CCD Files.](https://nces.ed.gov/ccd/files.asp#Fiscal:2,LevelId:7,Page:1)

## Repository Structure

### `/main`

This folder holds all the data necessary to replicate the tables and plots used in the main body of text. 

### `/appendix`

This folder holds all the data necessary to replicate the tables and plots used in the appendix.

It is organised in accordance with the text's appendix: 

- `/A1`

- `/A2`

- `/A3`

  - `/simple`: Contains the plots and tables of the simple version of the regression.

- `/A4`

  - `/simple`
    - `/plot`: Contains the plots for the simple version of the regression.
    - `/table`: Contains all the data necessary to replicate the table for the simple version of the regression.
  - `/ps`: Contains the plots of the post-stratification of the regression.

### `/data`

This folder holds all of the processed data used to run the analyses.

- `/clean`: Contains the cleaned, school-level, race count data used to calculate the outcome measures of segregation, generated from the raw data through the `code/process_data` script.
  - `syYY_mem`: There is one dataset per school year (sy), all named with this format. replace YY with the first calendar year of the school year. 

- `/covs`: This holds the raw data, directly exported from [ELSi](https://nces.ed.gov/ccd/elsi/tablegenerator.aspx) with all the covariates necessary for the regressions.
  - `/district`: district-level covariates for district-level regressions.
  - `/school`: school-level covariates for school-level regressions.

- `/pre`: This holds the data used for `matching`, i.e., for narrowing the control group to comparable districts. It also holds the RDS file with the list of `LEAID`s to keep to use for filtering when processing the data.

- `school_count_data`: The output of running `/code/process_data` which aggregates the race counts for each high school in each year for all the districts in `leaid_keep`.
- `school_panel_cov`: The output of running `/code/school/school_analysis`, which adds the school-level covariates to the `school_count_data` for each school in each year, after calculating the district-level segregation measures.
- `theil_seg_measures_with_dis_cov`: The output of running `/code/district/district_analysis`, which calculates the district-level segregation measures and adds the district-level covariates for each district in each year to `school_count_data`.

### `/code`

This folder holds all the code used to process the raw data and run the analyses, except for the code used to generate the plots and figures in the text submission, as that is available in Appendix A5 of the document.

- `/district`

  - `district_analysis`: Takes `school_count_data` and calculates the district-level segregation metrics ($M$, $H$, $D$, and $P*$). It then adds the covariates from `data/covs/district` to each district in each year, copying the values into each grade breakdown. Creating our `theil_seg_measures_with_dis_cov` dataset. 
  - `district_reg`: Uses the previously created, in `district_analysis` script, `theil_seg_measures_with_dis_cov`, to run the regressions for the district-level analysis, producing the results in `main` and `appendix` folders.

- `/school`

  - `school_analysis`: Takes `school_count_data` and calculates the school-level segregation metrics ($H_{diversity}$, $ls$, $white\_share$, and $rep\_black$, $rep\_hisp$, $rep\_nonwhite$). It then adds the covariates from `data/covs/school` to each school in each year, copying the values into each grade breakdown. This creates our `school_panel_cov` dataset.
  - `school_reg`: Uses the previously created, in `school_analysis` script, `school_panel_cov`, to run the regressions for the school-level analysis, producing the results in `main` and `appendix` folders.

- `matching`: Uses the data in `/data/pre` to filter districts on various covariates and minimum, pre-treatment, thresholds, to keep only a comparable group of large and regular urban school districts.

- `process_data`: From the raw data, which is not provided in this repo, it generates, for each year, a cleaned dataset with counts of how many students were in that grade (ranging 9 through 12, for high school) for each category of race (`white`, `black`, `hisp`, `asian`, and `other`). It then aggregates these to produce `school_count_data` which will then be used to calculate the segregation measures at both the district and school level, subsequently used to run the regressions in both the main body of text and the appendix.


