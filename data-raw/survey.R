library(dplyr)

survey <- readr::read_csv("Neal_Smith_Survey_Q3Q15.csv",
                          na = "9") 

devtools::use_data(survey, overwrite = TRUE)
