# This is used to create a list for our application
library(tidyverse)
library(readxl)
library(writexl)
variable_list_app <- readxl::read_xlsx("data/variable_breakdown_2022.xlsx", sheet = 1) %>%
  filter(`SAS Variable Name` %in%
           c("_STATE"
             , "_RFHLTH" # GENHLTH group
             , "_AGE80" # Age groups from 18-24 through 80 or older
             , "_BMI5CAT"
             , "DRNKANY6" # Any alcohol last 30 days - just to indicate if drinks or not
             , "CHECKUP1"
             , "CHCKDNY2" # Kidney disease check; https://www.kidney.org/news/newsroom/fsindex#:~:text=People%20with%20kidney%20disease%20are,to%20or%20worsen%20the%20other.
             , "EMPLOY1"
             , "CVDINFR4"
             , "SEXVAR"
             , "_INCOMG1"
             , "CVDCRHD4" # Angina or coronary heart disease
             , "_EDUCAG"
             , "EXERANY2"
             , "PERSDOC3"
             , "CVDSTRK3" # Ever diagnosed with a stroke; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5298897/#:~:text=Diabetes%20is%20a%20well%2Destablished,stroke%20with%20uncontrolled%20glucose%20levels.
             , "_CHLDCNT"
             , "COVIDPOS"
             , "HAVARTH4"
             )
  )


# This file is then manually cleaned to appropriately account for application information that our team needed
# please see the /app_data/variable_list_app_models_adjusted.xlsx for more information
writexl::write_xlsx(variable_list_app,"results/app_data/variable_list_app_models.xlsx")
