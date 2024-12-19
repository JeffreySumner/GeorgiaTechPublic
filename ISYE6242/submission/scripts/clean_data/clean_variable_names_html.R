library(rvest)
library(tidyverse)
# Get the HTML Tables ----
html_content <- read_html("data/codebook22_llcp-v2-508.HTML", encoding = "UTF-8")
table <- html_content %>% html_node("table") %>% html_table()
details <- html_content %>%
  html_node(xpath = "//td[contains(@class, 'linecontent')]") %>%
  html_text() %>%
  strsplit(split = "<br>") %>%
  unlist()

table <- html_content %>% html_node("table:nth-of-type(1)") %>% html_table()
tables <- html_content %>% html_nodes("table") %>% html_table(fill = TRUE)
print(tables[[500]]) 

condensed_tables <- tables[seq(4,968,3)]

condensed_tables[1] %>% pluck(1)
str_replace_all(condensed_tables[322][[1]][1,]$X2,"Label:|Section\\sName:|Module\\sNumber:|Question\\sNumber:|Column:|Type\\sof\\sVariable:|SAS\\sVariable\\sName:|Question\\sPrologue:|Question:","|") %>% str_split("\\|") %>% unlist()

x <- condensed_tables[2]
make_clean_data_happen <- function(x){
  print(x)
  temp <- x
  print(temp)
  info <- str_replace_all(temp[1,]$X1,"Label:|Section\\sName:|Module\\sNumber:|Question\\sNumber:|Column:|Type\\sof\\sVariable:|SAS\\sVariable\\sName:|Question\\sPrologue:|Question:","|") %>%
    str_split("\\|") %>%
    unlist() %>%
    str_replace("\\s","")
  
  print(info)
  if(length(info) == 10){
    info <- str_replace_all(temp[1,]$X1,"Label:|Section\\sName:|Section\\sNumber:|Question\\sNumber:|Column:|Type\\sof\\sVariable:|SAS\\sVariable\\sName:|Question\\sPrologue:|Question:","|") %>%
      str_split("\\|") %>%
      unlist() %>%
      str_replace("\\s","")
  }
  print(temp[1,]$X1)
  names <- c("Value", "Value Label", "Frequency", "Percentage", "Wighted Percentage")
  
  temp <- temp %>%
    filter(row_number() > 2)
  
  names(temp) <- names
  
  temp <- temp %>%
    mutate(Label = info[2]
           , `Section Name` = info[3]
           , `Module Number` = info[4]
           , `Question Number` = info[5]
           , `Type of Variable` = info[6]
           , `SAS Variable Name` = info[7]
           , `Question Prologue` = info[8]
           , Question = info[9])
  
  return(temp)
  
}


final_data <- bind_rows(lapply(condensed_tables,make_clean_data_happen))

library(writexl)

write_xlsx(
  list(
    full_breakdown = final_data
    , variable_list = final_data %>% select(Question, `Question Prologue`, `SAS Variable Name`, `Section Name`, `Label`) %>%
      mutate(ToKeep = FALSE) %>%
      distinct()
  ), "data/variable_breakdown_2022.xlsx")