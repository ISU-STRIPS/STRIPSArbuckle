## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----load_data, message=FALSE--------------------------------------------
library(dplyr)

d <- STRIPSArbuckle::survey_responses %>% 
  dplyr::select(CaseID,
         starts_with("q3"), 
         starts_with("q15"),
         -starts_with("q15p")) %>%
  tidyr::gather(question, response, -CaseID, -q3) %>%
  rename(farmer = q3)

## ----summarize_data------------------------------------------------------
ordered_responses = c("No response",
                      "No Priority",
                      "Slight Priority",
                      "Moderate Priority",
                      "High Priority",
                      "Very High Priority")

s = d %>%
  filter(!is.na(farmer)) %>%
  mutate(question = substr(question, 1,4),
         response = ifelse(is.na(response), "No response", response)) %>%
  group_by(farmer, question, response) %>%
  summarize(n = length(CaseID)) %>%
  ungroup() %>%
  mutate(response = factor(response, levels=ordered_responses))

## ----t_test--------------------------------------------------------------
# Run t test to get t statistic and p value
make_data <- function(n) {
  data.frame(y=rep(0:4,n)) # Converts 
}

my_t.test <- function(d) {
  test = t.test(make_data(d$No),
                make_data(d$Yes),
                var.equal = TRUE)
  data.frame(t = formatC(test$statistic, 2, format='f'),
             p = formatC(test$p.value, 3, format='f'))
}

test = s %>%
  tidyr::spread(farmer,n) %>%
  arrange(question, response) %>%
  filter(response != "No response") %>%
  group_by(question) %>%
  do(my_t.test(.))

## ------------------------------------------------------------------------
# Create data.frame with nice question formatting
pretty_names = 
  c("Increase recreation opportunities",
    "Beautification",
    "Increase crop production",
    "Increase livestock production",
    "Improve flood control",
    "Drinking water quality",
    "Swimming water quality",
    "Aquatic life water quality",
    "Restore native prairie",
    "Restore wetlands",
    "Reduce greenhouse gases",
    "Increase rural job opportunities",
    "Increase tourism",
    "Improve game habitat",
    "Improve non-game habitat")

names(pretty_names) <- paste0("q15",letters[1:15])

## ----print_results, results='asis'---------------------------------------
ordered_colnames = paste(rep(c("No","Yes"), each=length(ordered_responses)), 
                         ordered_responses,
                         sep="_")

tmp_for_table = s %>%
  mutate(column_name = factor(paste(farmer,response,sep="_"), 
                              levels = ordered_colnames)) %>%
  select(question, column_name, n) 

for_table <- tmp_for_table %>%
  tidyr::spread(column_name, n) %>%
  left_join(test, by="question") %>%
  mutate(question = plyr::revalue(question, pretty_names))

names(for_table) <- gsub("No_", "", names(for_table))
names(for_table) <- gsub("Yes_","", names(for_table))


htmlTable::htmlTable(for_table,
                     rnames = FALSE,
                     cgroup = c("","Non-farmer","Farmer","",""),
                     n.cgroup = c(1,6,6,1,1),
                     align = "l|rrrrrr|rrrrrr|rr")

