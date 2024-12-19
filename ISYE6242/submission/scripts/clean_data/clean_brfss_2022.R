library(tidyverse)
library(readxl)
library(data.table)

n = 400000
final_data <- readxl::read_xlsx("data/variable_breakdown_2022.xlsx", sheet = 1)
survey_data_2022 <- data.table::fread("data/SurveyData2022.csv") %>%
  select_if(~sum(is.na(.)) <= n) %>%
  filter(DISPCODE == 1100)

survey_data_long <- survey_data_2022%>% 
  pivot_longer(
    c(-`_STATE`, -FMONTH, -IDATE, -IMONTH, -IDAY, -IYEAR, -DISPCODE, -SEQNO, -`_PSU`)
    , names_to = "metric"
    , values_to = "value"
    , values_transform = as.character
  )

condensed_data <- survey_data_long %>%
  mutate(ToKeep = case_when(
    metric %in% (final_data %>%
                   filter(str_detect(`Section Name`,c("Health|Exercise|Demographics|Alcohol|Diabetes|Occupation|COVID")
                   )
                   ) %>% pull(`SAS Variable Name`) %>% unique()) ~ TRUE
    , metric %in% c("WTKG3","_BMI5","_BMI5CAT","_RFBMI5","HTIN4","HTM4","_INCOMG1","_EDUCAG","_AGE5YR"
                    ,"_AGE65YR","_AGE80","_AGE_G", "MARITAL","CHOLCHK3", "TOLDHI3", "CHOLMED3"
                    , "ALCDAY5","AVEDRNK3","DRNK3GE5","MAXDRNKS", "VEGETAB2", "FRUIT2"
                    , "FRUTDA2_","_FRTLT1A", "_VEGLT1A", "FVGREEN1","FRENCHF1"
                    , "SEXVAR", "BPHIGH6", "HAVARTH5","_CHLDCNT", "_RFHLTH", "CVDINFR4","DRNKANY6") ~ TRUE
  )) %>%
  mutate(ToKeep = case_when(
    
    str_detect(metric,"ASBI|NUMHHOL4|MAXDRNKS|PDIABTS1|LSATISFY|FOODSTMP|EMTSUPRT|DRNK3GE5|COVIDFS1|COVIDNU1|COVIDSE1|COVIDSMP|COVIDVA1|POORHLTH|PREDIAB2|PREGNANT|SDH|ASTHNOW|AVEDRNK3|LASTDEN4|RMVTETH4") ~ FALSE
    , TRUE ~ ToKeep
  )) %>%
  filter(ToKeep) %>%
  select(-ToKeep) %>%
  arrange(metric) %>%
  pivot_wider(names_from = metric, values_from = value)

data.table::fwrite(condensed_data, "data/BRFSS_Analysis_Data_2022.csv")
