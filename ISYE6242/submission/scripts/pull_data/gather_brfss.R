library(tidyverse)
library(glue)
library(rvest)
# test edit
# Download Initial Data ----
## Selected Years ----
# select the years you want to download data for
selected_years <- 2022
## download_brfss_xpt ----
# this will download the XPT dataset
# category labels are truncated in the header so we need to fix this with the format files
download_brfss_xpt <- function(year){
  print(year)
  # brfss survey data
  url <- glue::glue("https://www.cdc.gov/brfss/annual_data/{year}/files/LLCP{year}XPT.ZIP")
  storage_location <- glue::glue("data/LLCP{year}XPT.ZIP")
  download.file(url, storage_location, mode="wb")
  unzip(storage_location, exdir = "data/")
  
}
lapply(selected_years, download_brfss_xpt)
## convert_xpt_to_csv ----
# Turn the XPT file into CSV
# Columns will need to be cleaned up with another function using the labels
convert_xpt_to_csv <- function(year){
  print(year)
  temp_data <- haven::read_xpt(glue::glue("data/LLCP{year}.XPT"))
  
  readr::write_csv(temp_data, glue::glue("data/SurveyData{year}.csv"))
  # readr::write_rds(temp_data, glue::glue("data/SurveyData{year}.rds"))
}

selected_years <- 2022

lapply(selected_years,convert_xpt_to_csv)

## parse_variable_list ----
variable_list <- readr::read_csv("data/BRFSS_Variable_List.csv")


# Deprecated ----
## download_brfss_format ----
# # first format file to understand what each input means
# # must be parsed
# download_brfss_format <- function(year){
#   print(year)
#   # brfss survey data
#   url <- glue::glue("https://www.cdc.gov/brfss/annual_data/{year}/files/FORMAT{substr(year,3,4)}.sas")
#   storage_location <- glue::glue("data/FORMAT{substr(year,3,4)}.txt")
#   download.file(url, storage_location, mode="wb")
#   unzip(storage_location, exdir = "data/")
# 
# }
# lapply(selected_years,download_brfss_format)
# 
## download_brfss_sasout ----
# # second format file to understand column meanings
# # must be parsed
# download_brfss_sasout <- function(year){
#   print(year)
#   # brfss survey data
#   url <- glue::glue("https://www.cdc.gov/brfss/annual_data/{year}/files/SASOUT{substr(year,3,4)}_LLCP.sas")
#   storage_location <- glue::glue("data/SASOUT{substr(year,3,4)}_LLCP.txt")
#   download.file(url, storage_location, mode="wb")
#   unzip(storage_location, exdir = "data/")
#   
# }
# lapply(selected_years,download_brfss_sasout)
# 
## download_brfss_formas ----
# # second format file to understand column meanings
# # must be parsed
# download_brfss_formas <- function(year){
#   print(year)
#   # brfss survey data
#   url <- glue::glue("https://www.cdc.gov/brfss/annual_data/{year}/files/FORMAS{substr(year,3,4)}.sas")
#   storage_location <- glue::glue("data/FORMAS{substr(year,3,4)}.txt")
#   download.file(url, storage_location, mode="wb")
#   unzip(storage_location, exdir = "data/")
#   
# }
# lapply(selected_years,download_brfss_formas)
# 
# 





