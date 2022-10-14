# install.packages("officer")
# install.packages("dplyr")
# install.packages("glue")
library(officer)
library(dplyr)
library(glue)

# Download the syllabus
# Put Syllabus in project folder

sample_doc <- read_docx("Intro Analytics Modeling - ISYE-6501/OMSA_ISYE6501Syllabus-combined-Fall_2022.docx")
content <- docx_summary(sample_doc)
# filter to appropriate word doc data - index 96 for 6501 syllabus - will change for different courses

table_cells <- content %>% filter(content_type == "table cell",doc_index %in% 96)

table_data <- table_cells %>% filter(!is_header) %>% select(row_id, cell_id, text)

# split data into individual columns
splits <- split(table_data, table_data$cell_id)
splits <- lapply(splits, function(x) x$text)

# combine columns back together in wide format
table_result <- bind_cols(splits)

# get table headers
cols <- table_cells %>% filter(is_header)
names(table_result) <- cols$text

# if the above table headers don't work for whatever reason, they are Week, Topic, Date ranges

names(table_result) <- c("Weeks","Course Topics","Dates")


# create your directory list
directories <- glue::glue("Intro Analytics Modeling - ISYE-6501/{table_result$Weeks} - {table_result$`Course Topics`}")

# loop and create directories
for(i in directories){
  if (file.exists(i)) {
    
    cat("The folder already exists")
    
  } else {
    
    dir.create(i)
    
  }
}
