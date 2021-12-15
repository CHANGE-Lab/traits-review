########## 
##########
# This script matches the DOIs to Journal names for the papers included  
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2021-12-12
##########
##########

# set-up =======================================================================

library(tidyverse)
library(here)
library(knitr)
library(kableExtra)
library(magick)
library(webshot)
install.packages("magick")
install.packages("webshot")
webshot::install_phantomjs()

unique_data = read_csv(here('./data/unprocessed-data/unique_papers.csv')) %>% 
  select(Year, Journal, DOI, `Relevant to Study`) %>% 
  rename(Relevance = `Relevant to Study`) 
unique_data$Relevance = as.factor(unique_data$Relevance)
unique(unique_data$Relevance)
unique_data = unique_data %>% 
  filter(Relevance %in% c("Y", "y")) %>% 
  select(-c(Year, Relevance))

new_data = read_csv(
  here("./data/unprocessed-data/study_classification_FuncBiogeog_to_append.csv")) %>% 
  select(PY, SO, DOI, Relevance) %>% 
  rename(Journal = SO, Year = PY) %>% 
  filter(Relevance == 1) %>% 
  select(-c(Year, Relevance))

# join data ====================================================================

all_data = rbind(new_data, unique_data)

all_data = mutate_all(all_data, funs(toupper))
unique_journals = all_data %>% 
  group_by(Journal) %>% 
  mutate(across(where(is.character), toupper)) %>% 
  summarize(`Number of Relevant Papers` = n())

table = unique_journals[order(
  unique_journals$`Number of Relevant Papers`, decreasing = TRUE),]

write_csv(table, here("./output-tables/journals_table.csv"))

write.table(table, here("./output-tables/journals_table.txt"))


