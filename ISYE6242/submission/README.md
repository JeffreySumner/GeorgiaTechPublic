Data Visualization 6242: Diabetes Risk Assessment Project
================

## Disclaimer

The goal of this project was to analyze the BRFSS data to raise
awareness to risk factors that increase the likelihood of an individual
having diabetes. We are NOT medical professionals and in no way claim to
be. Any models and interpretations are our own and medical advice should
be sought from a medical professional. Our goal is to merely raise
awareness of potential diabetes risk factors. If you believe that there
is something medically wrong with you, please contact a healthcare
provider.

## Github File Structure

If you plan to use this repository ensure that you have the following
directories stored in the same directory as your .Rproj file or please
set your working directory to the submission sub-directory. These
directories are required to ensure each script runs. Make sure to
include each of the .csv and/or .rds data files.

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    ##                                         levelName
    ## 1  .                                             
    ## 2   ¦--app_data                                  
    ## 3   ¦   ¦--variable_list_app_models.xlsx         
    ## 4   ¦   °--variable_list_app_models_adjusted.xlsx
    ## 5   ¦--data                                      
    ## 6   ¦   ¦--BRFSS_Analysis_Data_2022.csv          
    ## 7   ¦   ¦--BRFSS_Variable_List.xlsx              
    ## 8   ¦   ¦--codebook22_llcp-v2-508.HTML           
    ## 9   ¦   ¦--LLCP2022.XPT                          
    ## 10  ¦   ¦--LLCP2022XPT.ZIP                       
    ## 11  ¦   °--SurveyData2022.csv                    
    ## 12  ¦--models                                    
    ## 13  ¦   ¦--model_data                            
    ## 14  ¦   ¦   ¦--enhanced_model_data.RDS           
    ## 15  ¦   ¦   ¦--enhanced_model_testing_data.RDS   
    ## 16  ¦   ¦   °--enhanced_model_training_data.RDS  
    ## 17  ¦   ¦--enhanced_log_workflow.RDS             
    ## 18  ¦   ¦--enhanced_recipe.RDS                   
    ## 19  ¦   °--enhanced_xgboost_workflow.RDS         
    ## 20  ¦--rsconnect                                 
    ## 21  ¦   °--shinyapps.io                          
    ## 22  ¦       °--npfp0p-jeffrey0sumner             
    ## 23  ¦           °--DiabetesAwarenessTool.dcf     
    ## 24  ¦--scripts                                   
    ## 25  ¦   ¦--clean_data                            
    ## 26  ¦   ¦   ¦--clean_brfss_2022.R                
    ## 27  ¦   ¦   °--clean_variable_names_html.R       
    ## 28  ¦   ¦--helpers                               
    ## 29  ¦   ¦   ¦--app_variable_list.R               
    ## 30  ¦   ¦   ¦--draw_confusion_matrix.R           
    ## 31  ¦   ¦   ¦--helpers.R                         
    ## 32  ¦   ¦   °--required_packages.R               
    ## 33  ¦   ¦--model                                 
    ## 34  ¦   ¦   °--create_models.R                   
    ## 35  ¦   °--pull_data                             
    ## 36  ¦       °--gather_brfss.R                    
    ## 37  ¦--www                                       
    ## 38  ¦   ¦--georgia_tech_theme.css                
    ## 39  ¦   ¦--GeorgiaTech_Navy.png                  
    ## 40  ¦   ¦--GeorgiaTech_RGB.png                   
    ## 41  ¦   ¦--GeorgiaTech_TechGold.png              
    ## 42  ¦   ¦--GeorgiaTech_TechGoldandWhite.png      
    ## 43  ¦   ¦--GeorgiaTech_White.png                 
    ## 44  ¦   °--GeorgiaTech_White.svg                 
    ## 45  ¦--app.R                                     
    ## 46  ¦--README.md                                 
    ## 47  °--README.Rmd

## Data Gathering & Cleaning

The data was pulled from the CDC BRFSS website. You can find the
information about the survey, data and questionnaires at the following
url: <https://www.cdc.gov/brfss/index.html>

For our project we pulled the 2022 dataset and utilized the 2022 BRFSS
Codebook to correctly label stored survey responses and enrich variable
meaningfulness. The 2022 BRFSS data and 2022 BRFSS Codebook can be
downloaded from the following link:
<https://www.cdc.gov/brfss/annual_data/annual_2022.html>

The variable list will also need to be downloaded or use the file within
the `/data` directory:

<https://data.cdc.gov/Behavioral-Risk-Factors/Behavioral-Risk-Factor-Surveillance-System-BRFSS-H/iuq5-y9ct>

Below are a few examples of our data gathering and cleaning process.

### Loading Packages

Most scripts will have packages listed at the top and should install or
load depending on your situation. If has to be installed, you will need
to run the line once more when the install completes in order to load
the package. Refer to the `scripts/helpers/required_packages.R` if there
are any concerns.

``` r
if (!require('devtools')) install.packages('devtools')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('tictoc')) install.packages('tictoc')
if (!require('glue')) install.packages('glue')
if (!require('httr')) install.packages('httr')
if (!require('rvest')) install.packages('rvest')
if (!require('here')) install.packages('here')
if (!require('ggmap')) install.packages('ggmap')
if (!require('glmnet')) install.packages('glmnet')
if (!require('vip')) install.packages('vip')
if (!require('caret')) install.packages('caret')
if (!require('xgboost')) install.packages('xgboost')
if (!require('ggcorrplot')) install.packages('ggcorrplot')
if (!require('zoo')) install.packages('zoo')
if (!require('lubridate')) install.packages('lubridate')
if (!require('ggthemes')) install.packages('ggthemes')
if (!require('forcats')) install.packages('forcats')
if(!require('bookdown')) install.packages('bookdown')
if(!require('kableExtra')) install.packages('kableExtra')
if(!require('scales')) install.packages('scales')
if(!require('stringr')) install.packages('stringr')
```

### Pull BRFSS Data

If you choose to not manually download the data file listed above, you
can use the `scripts/pull_data/gather_brfss.R` in order to pull the
data. Below is a sample function that is contained in the script as well
as a simple use-case.

``` r
# This can be a single integer or vector of integers
selected_years <- 2022

# this will download the XPT dataset
download_brfss_xpt <- function(year){
  print(year)
  # brfss survey data
  url <- glue::glue("https://www.cdc.gov/brfss/annual_data/{year}/files/LLCP{year}XPT.ZIP")
  storage_location <- glue::glue("data/LLCP{year}XPT.ZIP")
  download.file(url, storage_location, mode="wb")
  unzip(storage_location, exdir = "data/")
}

# Run the function to pull the XPT file
download_brfss_xpt(year = selected_years)

# Turn the XPT file into CSV
convert_xpt_to_csv <- function(year){
  print(year)
  temp_data <- haven::read_xpt(glue::glue("data/LLCP{year}.XPT"))
  readr::write_csv(temp_data, glue::glue("data/SurveyData{year}.csv"))
}

selected_years <- 2022

convert_xpt_to_csv(year = selected_years)
```

The above code will pull the XPT file and convert it to a csv labeled
SurveyData{Year}.csv. This survey dataset will be used later to create a
sanitized dataset for analysis.

### Clean variable names

Assuming the link to the codebook was followed and downloaded, extract
and place the `codebook22_llcp-v2-508.HTML` file in the `/data`
directory. With that HTML we will extract the required variable
information that is not listed within the downloaded variable names list
from the CDC website. For whatever reason there are fields missing
between the two. Running the `/scripts/clean_variable_names_html.R` code
will create a comprehensive list of fields and their respective values
for use within our application.

### Clean BRFSS

With the variable names cleaned up and the survey data ready, we should
be able to apply the code within the `/scripts/clean_brfss_2022.R`
script. This script reads in the survey data and cleaned up field list
to select only columns that are necessary for analysis. This list was
curated after extensive research.

## Model & Visualization

This `/model` directory contains 1 script:

1.  create_models.R

Within this script we build and store baseline models and enhanced
models. In addition, we will visualize variable importance plots as well
as confusion matrices that assist in evaluating model performance.

### Model Building

We will not cover the models in detail but the general concept here is
to read in the data created in the previous steps then perform various
modeling techniques ranging from LASSO regression, Logistic Regression,
and Decision Trees. Please see the tidymodels documentation to find out
more about our approach and see examples. You can find this
documentation at <https://www.tidymodels.org/>

## Shiny Application

We have an additional script called `app.R` located in the submission
directory. This script contains a modularized shinydashboard application
that incorporate the models created from our `create_models.R` script.
The `app_data` directory contains important variable information that
our application uses to correctly display survey questions that have
been modified to fit the needs of our tool.

The `/www` directory contains styling information for our application to
make it look more stylish.

The application itself is hosted via <https://shinyapps.io> and can be
found at the following url
<https://rpy-ai.shinyapps.io/DiabetesAwarenessTool/>.

## Wrap up

Below is the session information that was used to create this report.
The packages and their versions are listed. R 4.3.1 is required.

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22621)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/Chicago
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] kableExtra_1.3.4   bookdown_0.36      ggthemes_5.0.0     zoo_1.8-12        
    ##  [5] ggcorrplot_0.1.4.1 xgboost_1.7.5.1    caret_6.0-94       lattice_0.21-8    
    ##  [9] vip_0.4.1          glmnet_4.1-8       Matrix_1.6-1       ggmap_3.0.2       
    ## [13] here_1.0.1         rvest_1.0.3        httr_1.4.7         glue_1.6.2        
    ## [17] tictoc_1.2         yardstick_1.2.0    workflowsets_1.0.1 workflows_1.1.3   
    ## [21] tune_1.1.2         rsample_1.2.0      recipes_1.0.8      parsnip_1.1.1     
    ## [25] modeldata_1.2.0    infer_1.0.5        dials_1.2.0        scales_1.2.1      
    ## [29] broom_1.0.5        tidymodels_1.1.1   devtools_2.4.5     usethis_2.2.2     
    ## [33] plyr_1.8.8         data.tree_1.1.0    lubridate_1.9.2    forcats_1.0.0     
    ## [37] stringr_1.5.0      dplyr_1.1.2        purrr_1.0.2        readr_2.1.4       
    ## [41] tidyr_1.3.0        tibble_3.2.1       ggplot2_3.4.4      tidyverse_2.0.0   
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] jsonlite_1.8.7       rstudioapi_0.15.0    shape_1.4.6         
    ##   [4] magrittr_2.0.3       rmarkdown_2.24       fs_1.6.3            
    ##   [7] vctrs_0.6.3          memoise_2.0.1        webshot_0.5.5       
    ##  [10] htmltools_0.5.6      pROC_1.18.5          parallelly_1.36.0   
    ##  [13] htmlwidgets_1.6.2    cachem_1.0.8         mime_0.12           
    ##  [16] lifecycle_1.0.4      iterators_1.0.14     pkgconfig_2.0.3     
    ##  [19] R6_2.5.1             fastmap_1.1.1        future_1.33.0       
    ##  [22] shiny_1.7.5.1        digest_0.6.33        colorspace_2.1-0    
    ##  [25] furrr_0.3.1          ps_1.7.5             rprojroot_2.0.3     
    ##  [28] pkgload_1.3.2.1      fansi_1.0.4          timechange_0.2.0    
    ##  [31] compiler_4.3.1       remotes_2.4.2.1      withr_2.5.2         
    ##  [34] backports_1.4.1      RgoogleMaps_1.4.5.3  pkgbuild_1.4.2      
    ##  [37] MASS_7.3-60          lava_1.7.2.1         sessioninfo_1.2.2   
    ##  [40] ModelMetrics_1.2.2.2 tools_4.3.1          httpuv_1.6.11       
    ##  [43] future.apply_1.11.0  nnet_7.3-19          callr_3.7.3         
    ##  [46] nlme_3.1-162         promises_1.2.1       grid_4.3.1          
    ##  [49] reshape2_1.4.4       generics_0.1.3       gtable_0.3.4        
    ##  [52] tzdb_0.4.0           class_7.3-22         data.table_1.14.8   
    ##  [55] hms_1.1.3            sp_2.0-0             xml2_1.3.5          
    ##  [58] utf8_1.2.3           foreach_1.5.2        pillar_1.9.0        
    ##  [61] later_1.3.1          splines_4.3.1        lhs_1.1.6           
    ##  [64] survival_3.5-5       tidyselect_1.2.0     miniUI_0.1.1.1      
    ##  [67] knitr_1.43           svglite_2.1.2        stats4_4.3.1        
    ##  [70] xfun_0.40            hardhat_1.3.0        timeDate_4022.108   
    ##  [73] stringi_1.7.12       DiceDesign_1.9       yaml_2.3.7          
    ##  [76] evaluate_0.21        codetools_0.2-19     cli_3.6.1           
    ##  [79] rpart_4.1.19         systemfonts_1.0.4    xtable_1.8-4        
    ##  [82] munsell_0.5.0        processx_3.8.2       Rcpp_1.0.11         
    ##  [85] globals_0.16.2       png_0.1-8            parallel_4.3.1      
    ##  [88] ellipsis_0.3.2       gower_1.0.1          prettyunits_1.2.0   
    ##  [91] jpeg_0.1-10          profvis_0.3.8        urlchecker_1.0.1    
    ##  [94] bitops_1.0-7         GPfit_1.0-8          listenv_0.9.0       
    ##  [97] viridisLite_0.4.2    ipred_0.9-14         prodlim_2023.08.28  
    ## [100] crayon_1.5.2         rlang_1.1.1
