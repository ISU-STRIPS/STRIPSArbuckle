#' STRIPS Survey data
#'
#' STRIPS Survey data for all questions. At the moment, this data set does not 
#' read in NA's properly and therefore caution should be used when looking at 
#' questions other than Q3 and Q15
#'
#' @format A data.frame with a lot of variables the first of which is 
#' \code{CaseID} and the remaining 123 columns are survey questions. 
#'
#' @author J. Gordon Arbuckle, \email{arbuckle@iastate.edu}
#'
#' @seealso \link{\code{survey_questions}}
#'
"survey_responses"


#' STRIPS Survey questions
#'
#' To aid analysis, survey data are recorded using the question number and this
#' data.frame provides the associated wording for the question. 
#'
#' @format A data.frame with a 2 variables: \code{question} and 
#' \code{question_text}.
#'
#' @author J. Gordon Arbuckle, \email{arbuckle@iastate.edu}
#'
#' @seealso \link{\code{survey_responses}}
#'
"survey_questions"


