library(dplyr)

survey_responses <- readr::read_csv("Neal_Smith_Survey_Data19May14.dat",
                          na = c("8","9")) %>%                 # This is likely not correct for columns other than Q3 and Q15
  dplyr::select(c(1:8,40:101,107:124))                         # Remove columns for a variety of reasons
                         

names(survey_responses) <- gsub("\\.(.*)", "", names(survey_responses))

devtools::use_data(survey_responses, overwrite = TRUE)

# Simplify the presentation by using just using the question number for column
# names and then providing another data set that has the actual question.

# survey_questions <- data_frame(tmp = names(survey)[-1]) %>%
#   tidyr::separate(tmp, c("number","question"), sep=" ", extra="merge")

survey_questions <- data.frame(tmp = names(survey)[-1]) %>%
  tidyr::separate(tmp, c("question","question_text"), extra="merge") %>%
  filter(question != "Number")

devtools::use_data(survey_questions, overwrite = TRUE)
