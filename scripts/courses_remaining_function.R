# Google Sheet Uses -----

# Read in stored sheet data -----

matrix_tbl <- readr::read_rds("data/pain_matrix.rds")
courses_tbl <- readr::read_rds("data/courses_tbl.rds")
syllabus_tbl <- readr::read_rds("data/syllabus_tbl.rds")


# Syllabus Rules

required_core <- syllabus_tbl %>%
  filter(course_type1 %in% "Core") %>%
  pull(course_code)

stats_electives <- syllabus_tbl %>%
  filter(course_type1 %in% "Statistics Elective") %>%
  pull(course_code)

ops_electives <- syllabus_tbl %>%
  filter(course_type1 %in% "Operations Elective") %>%
  pull(course_code)

b_track <- syllabus_tbl %>%
  filter(course_type1 %in% "Business Elective") %>%
  pull(course_code)

c_track <- syllabus_tbl %>%
  filter(course_type1 %in% "CS Elective") %>%
  pull(course_code) 

c_track_req <- syllabus_tbl %>%
  filter(course_code %in% "ISYE 6740") %>%
  pull(course_code) 

calculate_remaining_courses <- function(current_courses
                                        , track = c("A", "B", "C")
){
  
  core_remaining <- syllabus_tbl %>%
    filter(course_code %in% required_core) %>%
    filter(!course_code %in% current_courses)
    # str_subset(required_core, current_courses, negate = TRUE)
  
  if(track == "A"){
    
    
    
    
    
  } else if(track == "B"){
    
    
    
    
    
  } else{
    
    
    
    
    
  }
  
  
  
  
  
}