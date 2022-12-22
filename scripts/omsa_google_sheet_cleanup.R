# OMSA WIKI Data Extraction -----

# This script is meant to be used in addition to the OMSA WIKI Google Sheet
# The goal is to help students make decisions on which courses to take

# Required Packages & Initial Setup -----

library(googlesheets4)
library(tidyverse)

# google sheet location
google_sheet_name <- "https://docs.google.com/spreadsheets/d/1pErp_kO_PYDKP-htezzb-NqYoZefPh4nHRQ4mXge0tE/edit?pli=1#gid=98519517"

# sheets within the google sheet
omsa_wiki_sheets <- sheet_names(google_sheet_name)

# Reading the OMSA WIKI Google Sheet for Courses -----

# read in the course data
courses <- googlesheets4::read_sheet(google_sheet_name
                                     , sheet = omsa_wiki_sheets %>% str_subset("Courses")
                                     )

# cleaning up the course data
courses_tbl <- courses %>%
  filter(!is.na(AKA)
         , !Code %in% "NULL"
         , !AKA %in% c("Main","r/OMSA", "Prep")
         ) %>%
  mutate(Code = as.character(Code))

write_rds(courses_tbl,"data/courses_tbl.rds")

writexl::write_xlsx(courses_tbl,"data/courses_tbl.xlsx")


# Reading the OMSA WIKI Google Sheet for Matrix -----

# read in the pain matrix
matrix <- googlesheets4::read_sheet(google_sheet_name
                                    , sheet = omsa_wiki_sheets %>% str_subset("Matrix")
                                    , skip = 3
                                    )

# clean up pain matrix
matrix_tbl <- matrix %>%
  pivot_longer(cols = c(-1,-2)
               , names_to = "course2"
               , values_to = "time"
               ) %>%
  rename(course1_code = 1
         , course1 = 2
         ) %>%
  filter(!is.na(time)) %>%
  left_join(courses_tbl %>%
              select(AKA
                     , course1_name = `Course Name`
                     )
            , by = c("course1" = "AKA")
            ) %>%
  left_join(courses_tbl %>%
              select(AKA
                     , course2_name = `Course Name`
                     , course2_code = Code
              )
            , by = c("course2" = "AKA")
  ) %>%
  select(course1
         , course1_code
         , course1_name
         , course2
         , course2_code
         , course2_name
         , time
         )

write_rds(matrix_tbl,"data/pain_matrix.rds")
writexl::write_xlsx(matrix_tbl,"data/pain_matrix.xlsx")



# Reading the OMSA WIKI Google Sheet for Syllabuses -----

# read in the pain matrix
syllabus <- googlesheets4::read_sheet(google_sheet_name
                                    , sheet = omsa_wiki_sheets %>% str_subset("Syllabuses")
                                    , skip = 3
)

# clean up pain matrix
syllabus_tbl <- syllabus %>%
  select(course_code = 1
         , course = 2
         , course_desc = 3
         , difficulty = 4
         , workload = 5
         , rating = 6
         , pre_req = 8
         , remarks = 9
         , language = 10
         , software = 11
         , temp1 = 13
         , temp2 = 14
         , temp3 = 15
         , temp4 = 16
         , temp5 = 17
         , temp6 = 18
         ) %>%
  mutate(course_code = as.character(course_code)) %>%
  filter(!course %in% c(NA, "Pract", "Startup", "r/OMSA")
         ) %>%
  pivot_longer(cols = contains("temp")
               , names_to = "type"
               , values_to = "semester") %>%
  filter(!(semester %in% c(NA,"🚫") & !str_detect(remarks,"Brand New"))
         , !course_code %in% c(
           "ISYE 6404"
           , "ISYE 6413"
           , "ISYE 6650"
           , "ISYE 7750"
           , "ISYE 6739"
           , "CS 1301"
           , "CS 1331"
           , "CS 1332"
           , "CS 6640"
           , "CS 6476"
           , "CS 7641"
           #, "MGT 8803"
           , "CSE 6220"
           , "NULL"
         )) %>%
  group_by(course) %>%
  mutate(all_semesters = str_c(semester, collapse = ", ")
         , previously_offered_summer = str_detect(all_semesters,"Summer")
         , previously_offered_spring = str_detect(all_semesters,"Spring")
         , previously_offered_fall = str_detect(all_semesters,"Fall")
  ) %>%
  filter(
         ) %>%
  select(-type, - semester) %>%
  distinct() %>%
  mutate(course_type1 = case_when(course_code %in% c("ISYE 6501","MGT 8803", "CSE 6040", "MGT 6203", "CSE 6242") ~ "Core"
                                 , course_code %in% c("ISYE 6644", "ISYE 6669") ~ "Operations Elective"
                                 , str_detect(course_code,"ISYE") ~ "Statistics Elective"
                                 , str_detect(course_code, "MGT") ~ "Business Elective"
                                 , str_detect(course_code, "CS") ~ "CS Elective"
                                 , TRUE ~ NA_character_
                                 )
         )
  
write_rds(syllabus_tbl,"data/syllabus_tbl.rds")
writexl::write_xlsx(syllabus_tbl,"data/syllabus_tbl.xlsx")

