library(dplyr)

# This is used for both survey_responses and survey_questions
tmp <- readr::read_csv("Neal_Smith_Survey_Data19May14.dat",
                          na = c("8","9")) %>%                 # This is likely not correct for columns other than Q3 and Q15
  dplyr::select(c(1:8,40:101,107:124))                         # Remove columns for a variety of reasons
   

# The idea is to make the analysis easier by making easy to type
# column names, e.g. q1a for question 1a and then have another data.frame
# that contains the actual text for the question. The former is in 
# survey_responses while the later is in survey_questions.


####################################################
# survey_responses                       
survey_responses <- tmp

names(survey_responses) <- paste0("q",gsub("\\.(.*)", "", names(survey_responses)))
names(survey_responses)[1] <- "CaseID"

# Columns 16a 16c and 16h are duplicates due to reverse coding
survey_responses <- survey_responses[, !duplicated(colnames(survey_responses))]


devtools::use_data(survey_responses, overwrite = TRUE)



##################################################
# survey_questions

# Simplify the presentation by using just using the question number for column
# names and then providing another data set that has the actual question.

# survey_questions <- data_frame(tmp = names(survey)[-1]) %>%
#   tidyr::separate(tmp, c("number","question"), sep=" ", extra="merge")

survey_questions <- data.frame(tmp = names(tmp)[-1]) %>%
  tidyr::separate(tmp, c("question","question_text"), extra="merge") %>%
  filter(question != "Number")

devtools::use_data(survey_questions, overwrite = TRUE)
