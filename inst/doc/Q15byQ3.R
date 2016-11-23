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
  data.frame(y=rep(0:4,n)) # Converts counts by category to numeric vector
}

my_t.test <- function(d) {
  test = t.test(make_data(d$No),
                make_data(d$Yes),
                var.equal = TRUE)
  data.frame(t   = test$statistic,
             p   = test$p.value,
             No  = test$estimate[1],
             Yes = test$estimate[2])
}

test = s %>%
  tidyr::spread(farmer,n) %>%
  arrange(question, response) %>%
  filter(response != "No response") %>%
  group_by(question) %>%
  do(my_t.test(.)) %>%
  ungroup() 

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

test_formatted <- test %>%
  dplyr::select(-No, -Yes) %>%
  mutate(t = formatC(t, 2, format='f'),
         p = formatC(p, 3, format='f'))

for_table <- tmp_for_table %>%
  tidyr::spread(column_name, n) %>%
  left_join(test_formatted, by="question") %>%
  mutate(question = plyr::revalue(question, pretty_names))

names(for_table) <- gsub("No_", "", names(for_table))
names(for_table) <- gsub("Yes_","", names(for_table))


htmlTable::htmlTable(for_table,
                     rnames = FALSE,
                     cgroup = c("","Non-farmer","Farmer","",""),
                     n.cgroup = c(1,6,6,1,1),
                     align = "l|rrrrrr|rrrrrr|rr")

#for_table %>% readr::write_csv(path="social_data_table.csv")

## ------------------------------------------------------------------------
g_data <- test %>% 
  mutate(significant = p<0.05,
         farmer_higher = Yes > No,
         farmer_signif = paste(ifelse(farmer_higher,
                                      "Farmer higher", 
                                      "Non-farmer higher"),
                               ifelse(significant, 
                                      "(p<0.05)", 
                                      "(p>0.05)")),
         farmer_signif = factor(farmer_signif, 
                                levels = c("Farmer higher (p<0.05)",
                                           "Farmer higher (p>0.05)",
                                           "Non-farmer higher (p>0.05)",
                                           "Non-farmer higher (p<0.05)")),
         question = plyr::revalue(question, pretty_names),
         question = factor(question, levels = question[order(Yes+No, 
                                                             decreasing = TRUE)]))
  
library(ggplot2)
# cp2 = rgb(red = c(0,241,200)/256, 
#          green = c(107,190,0)/256,
#          blue = c(166,72,0)/256)
cp2 = rgb(red = c(241,241,200)/256, 
         green = c(190,190,0)/256,
         blue = c(72,72,0)/256,
         alpha = c(1,.5,.5))
names(cp2) = c("Non-farmer higher (p<0.05)",
               "Non-farmer higher (p>0.05)",
               "Farmer higher (p>0.05)")

# Determine x-axis ordering in the plot


g_survey = ggplot(g_data, aes(x     = question, 
                              xend  = question, 
                              y     = No, 
                              yend  = Yes, 
                              color = farmer_signif)) + 
  theme_bw() +
  scale_y_continuous(limit=c(0,4), 
                     labels = c("None",
                                "Slight",
                                "Moderate",
                                "High",
                                "Very high")) +
  scale_alpha_discrete(range=c(0.5,1)) +
  geom_segment(size=4) + 
  theme(legend.position = c(0,0),
       legend.justification = c(0, 0), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.box.just=0) + 
  labs(x="", y="Priority", title="", color="") +
  scale_color_manual(values = cp2)

g_survey

#ggsave("survey_fig.pdf")

