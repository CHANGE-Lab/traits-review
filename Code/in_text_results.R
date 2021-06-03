########## 
##########
# This code contains some small calculations for in-text results presented 
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-01-28
##########
##########

# set-up =======================================================================

library(tidyverse)
library(here)
`%notin%` = Negate(`%in%`)

# read in all the data
categorical_data = read_csv(here('./data/processed-data/categorical_data.csv'))
categorical_data_nondum = 
  read_csv(here(paste0('./data/processed-data',
                       '/categorical_data_nondummy.csv')))
orig_dummy = read_csv(here('./data/processed-data/original_traits_dummy.csv'))
trait_levels_orig = 
  read_csv(here('./data/processed-data/trait_levels_orig_prim_sec.csv'))
trait_levels_clean = 
  read_csv(here('./data/unprocessed-data/trait_levels_clean.csv'))
trait_levels_func_bio = 
  read_csv(here(paste0('./data/unprocessed-data/',
                       'traits_classification_FuncBiogeog',
                       '_to_append_revised.csv')))

# data prepping ================================================================

categorical_data_nondum$Taxonomic = 
  str_to_title(categorical_data_nondum$Taxonomic)
unique(categorical_data_nondum$Taxonomic)
categorical_data_nondum$Taxonomic = 
  as.factor(categorical_data_nondum$Taxonomic)
categorical_data_nondum$Ecosystem = 
  as.factor(categorical_data_nondum$Ecosystem)

levels(categorical_data_nondum$Taxonomic)[
  levels(categorical_data_nondum$Taxonomic)=="Herps"] = 
  "Herpetofauna"
levels(categorical_data_nondum$Taxonomic)[
  levels(categorical_data_nondum$Taxonomic)=="Broad"] = 
  "Multiple"
levels(categorical_data_nondum$Taxonomic)[
  levels(categorical_data_nondum$Taxonomic)=="Bacteria"] = 
  "Other"
levels(categorical_data_nondum$Taxonomic)[
  levels(categorical_data_nondum$Taxonomic)=="Phytoplankton"] = 
  "Plankton"
levels(categorical_data_nondum$Taxonomic)[
  levels(categorical_data_nondum$Taxonomic)=="Fungi"] = 
  "Other"
levels(categorical_data_nondum$Taxonomic)[
  levels(categorical_data_nondum$Taxonomic)=="Invertebrates"] = 
  "Other"
levels(categorical_data_nondum$Ecosystem)[
  levels(categorical_data_nondum$Ecosystem)=="Broad"] =
  "Multiple"
levels(categorical_data_nondum$Ecosystem)[
  levels(categorical_data_nondum$Ecosystem)=="freshwater"] = 
  "Freshwater"
levels(categorical_data_nondum$Ecosystem)[
  levels(categorical_data_nondum$Ecosystem)=="terrestrial"] = 
  "Terrestrial"
levels(categorical_data_nondum$Ecosystem)[
  levels(categorical_data_nondum$Ecosystem)=="marine"] = 
  "Marine"

categorical_data_nondum$`Global Change Driver` = 
  as.factor(categorical_data_nondum$`Global Change Driver`)
levels(categorical_data_nondum$`Global Change Driver`)[
  levels(categorical_data_nondum$
           `Global Change Driver`)=="climate change"] = 
  "Climate Change"
levels(categorical_data_nondum$`Global Change Driver`)[
  levels(categorical_data_nondum$
           `Global Change Driver`)=="habitat degredation"] = 
  "Habitat Degredation"
levels(categorical_data_nondum$`Global Change Driver`)[
  levels(categorical_data_nondum$`Global Change Driver`)=="invasion"] = 
  "Invasion"
levels(categorical_data_nondum$`Global Change Driver`)[
  levels(categorical_data_nondum$
           `Global Change Driver`)=="Global Change Multiple"] = 
  "Global Change Broad"

# make the objects to be printed ===============================================

number_of_environmental_filters = categorical_data_nondum %>% 
  select(DOI, Fundamental, Physical, Ecological, Trophic) %>% 
  group_by(DOI) %>% 
  summarize(n = sum(Fundamental, Physical, Ecological, Trophic))
number_of_Traits = categorical_data_nondum %>% 
  select(DOI, Morphological, NEWPhysiological, `Life History`, Behavioural) %>% 
  group_by(DOI) %>% 
  summarize(n = sum(Morphological, NEWPhysiological, 
                    `Life History`, Behavioural))

one_filter = table(number_of_environmental_filters$n)[1]/
  sum(table(number_of_environmental_filters))
more_than_two_filter = sum(table(number_of_environmental_filters$n)[3:4])/
  sum(table(number_of_environmental_filters))*100
more_than_one_filter = sum(table(number_of_environmental_filters$n)[2:4])*100

more_than_one_trait = sum(table(number_of_Traits$n)[2:4])
more_than_two_trait = sum(table(number_of_Traits$n)[3:4])
four_trait = sum(table(number_of_Traits$n)[4])

number_of_plant_studies = 
  nrow(categorical_data_nondum %>%
         filter(Taxonomic == 'Plants'))/
  nrow(categorical_data_nondum) *100
number_of_plant_morphology_studies = 
  nrow(categorical_data_nondum %>% 
         filter(Taxonomic == 'Plants') %>%
         filter(Morphological == 1))/
  nrow(categorical_data_nondum) *100
number_of_aquatic_studies = 
  nrow(categorical_data_nondum %>%
         filter(Ecosystem %in% c('Marine', 'Freshwater')))/
  nrow(categorical_data_nondum) *100

orig_size = trait_levels_clean %>% 
  filter(Secondary_classification %in% 
           c('age/size at maturity', 'biomass',
             # counted 21 things that weren't size in 
             # 'age/size at maturity' so subtract 21
             'bone length/shape', 'brain size', 
             'cell biovolume', 'offspring size', 
             'size', 'stomach size')) %>% 
  select(Trait_spell_corrected, Secondary_classification) %>% 
  distinct()
new_size = trait_levels_func_bio %>% 
  filter(Secondary_classification %in% 
           c('age/size at maturity', 'biomass',
             'bone length/shape', 'brain size', 
             'cell biovolume', 'offspring size', 
             'size', 'stomach size')) %>% 
  select(Trait_spell_corrected, Secondary_classification) %>% 
  distinct()

number_of_measures_of_size = nrow(rbind(orig_size, new_size) %>% 
                                    distinct()) - 21

number_of_SLA_studies = nrow(orig_dummy %>% 
                               select(DOI, SLA) %>% 
                               filter(SLA == 1) %>% 
                               distinct())

number_of_body_size_studies = nrow(orig_dummy %>% 
                                     select(DOI, `body size`) %>% 
                                     filter(`body size` == 1) %>% 
                                     distinct())

number_of_unique_traits = n_distinct(trait_levels_orig$Trait)
percent_of_unique_traits_that_are_size = 
  (number_of_measures_of_size/number_of_unique_traits)*100

number_of_size_studies_df = trait_levels_orig %>% 
  filter(Secondary_classification %in% 
           c('age/size at maturity', 'biomass',
             # counted 21 things that weren't size 
             # in 'age/size at maturity' so subtract 21
             'bone length/shape', 'brain size', 
             'cell biovolume', 'offspring size', 
             'size', 'stomach size')) %>% 
  select(DOI, Trait, Secondary_classification) %>% 
  distinct()
number_of_size_studies = n_distinct(number_of_size_studies_df$DOI)

number_of_size_studies_avg_use_df = number_of_size_studies_df %>% 
  group_by(Trait) %>% 
  summarize(n = n())
number_of_size_studies_avg_use = mean(number_of_size_studies_avg_use_df$n)

number_of_nonsize_studies_df = trait_levels_orig %>% 
  select(DOI, Trait) %>% 
  filter(Trait %notin% number_of_size_studies_df$Trait) %>% 
  group_by(Trait) %>% 
  summarize(n = n())
number_of_nonsize_studies_avg_use = mean(number_of_nonsize_studies_df$n)

number_of_secondary_traits = 
  n_distinct(trait_levels_orig$Secondary_classification)
number_of_primary_traits = 
  n_distinct(trait_levels_orig$Primary_classification)

number_of_times_secondary_studies_used_df = trait_levels_orig %>% 
  select(Secondary_classification, Trait) %>% 
  unique() %>% 
  group_by(Secondary_classification) %>% 
  summarize(n = n())
percentage_of_secondary_traits_used_once = 
  nrow(number_of_times_secondary_studies_used_df %>% 
         filter(n == 1))/nrow(number_of_times_secondary_studies_used_df)*100
percentage_of_secondary_traits_used_more_than_10_times = 
  nrow(number_of_times_secondary_studies_used_df %>% 
         filter(n >= 10))/nrow(number_of_times_secondary_studies_used_df)*100


number_of_physiological_studies_df = 
  rbind(trait_levels_orig %>% 
          filter(grepl("tolerance", Secondary_classification)),
        trait_levels_orig %>% 
          filter(Primary_classification == 'physiology'))
number_of_physiological_studies = nrow(number_of_physiological_studies_df)

number_of_times_primary_studies_used_df = trait_levels_orig %>% 
  filter(Trait %notin% number_of_physiological_studies_df$Trait) %>% 
  select(Primary_classification, Trait) %>% 
  unique() %>% 
  group_by(Primary_classification) %>% 
  summarize(n = n())

number_of_morphology_studies_df = number_of_times_primary_studies_used_df %>% 
  filter(Primary_classification %in% c('morphology', 'composition'))
number_of_morphology_studies = sum(number_of_morphology_studies_df$n)
number_of_behaviour_studies_df = number_of_times_primary_studies_used_df %>% 
  filter(Primary_classification %in% c('behaviour', 'activity', 'habitat',
                                       'resource acquisition'))
number_of_behaviour_studies = sum(number_of_behaviour_studies_df$n)
number_of_lifehistory_studies_df = number_of_times_primary_studies_used_df %>% 
  filter(Primary_classification %in% c('growth', 'abundance', 'survival', 
                                       'life history'))
number_of_lifehistory_studies = sum(number_of_lifehistory_studies_df$n)

percentage_of_global_change_studies = 
  nrow(categorical_data_nondum %>% 
         filter(`Global Change Driver` != 0))/
  nrow(categorical_data_nondum)*100
percentage_of_forecasting_studies = 
  nrow(categorical_data_nondum %>% 
         filter(`Forecasting/Predictive` == 1))/
  nrow(categorical_data_nondum)*100

percentage_of_global_change_drivers = 
  table(categorical_data_nondum$`Global Change Driver`)/
  nrow(categorical_data_nondum)*100

predictive_papers_df = categorical_data_nondum %>% 
  filter(`Forecasting/Predictive` == 1) 
percentage_of_predictive_papers_since_2011 =
  nrow(predictive_papers_df %>% 
         filter(Year >= 2011))/nrow(predictive_papers_df)*100
percentage_of_predictive_papers_since_2015 =
  nrow(predictive_papers_df %>% 
         filter(Year >= 2015))/nrow(predictive_papers_df)*100

predictive_papers_focused_on_plants = 
  nrow(predictive_papers_df %>% 
         filter(Taxonomic == 'Plants'))/
  nrow(predictive_papers_df)*100

percentage_of_predictive_global_change_papers = 
  nrow(predictive_papers_df %>% 
         filter(`Global Change Driver` != 0))/
  nrow(categorical_data_nondum)*100
number_of_predictive_global_change_papers = 
  nrow(predictive_papers_df %>% 
         filter(`Global Change Driver` != 0))

predictive_types_of_global_change = 
  predictive_papers_df %>% 
  filter(`Global Change Driver` != 0)
predictive_types_of_global_change = 
  table(predictive_types_of_global_change$
          `Global Change Driver`)

trait_type_studies = categorical_data_nondum %>% 
  rowwise() %>% 
  mutate(sum = sum(Morphological, NEWPhysiological, `Life History`, 
                   Behavioural)) %>% 
  select(DOI, sum)

number_of_single_trait_type_studies = nrow(trait_type_studies %>% 
                                             filter(sum == 1))
number_of_three_or_more_trait_type_studies = nrow(trait_type_studies %>% 
                                                    filter(sum > 2))

# print all the results ========================================================

print(paste0("we identified ",
             n_distinct(categorical_data$DOI), 
             " studies relevant to traits-based..."))
print(paste0("most studies (", 
             one_filter, 
             ") focused on one level of filtering"))
print(paste0("pecentage of studies (", 
             more_than_two_filter, 
             ") focused on more than two levels of filtering"))
print(paste0("pecentage of studies (", 
             more_than_one_filter, 
             ") focused on more than one level of filtering"))
print(paste0("number of studies (", 
             more_than_one_trait, 
             ") focused on more than one trait"))
print(paste0("number of studies (", 
             more_than_two_trait, 
             ") focused on more than two traits"))
print(paste0("number of studies (", 
             four_trait, 
             ") focused on all four traits"))
print(paste0("pecentage of studies (",
             number_of_plant_studies, 
             ") focused on plants"))
print(paste0("pecentage of studies (", 
             number_of_plant_morphology_studies, 
             ") focused on plant morphology"))
print(paste0("pecentage of studies (", 
             number_of_aquatic_studies, 
             ") focused on aquatic systems"))
print(paste0("number of metrics (", 
             number_of_measures_of_size, 
             ") of size across studies"))
print(paste0("number of studies (", 
             number_of_SLA_studies, 
             ") that used SLA"))
print(paste0("number of studies (", 
             number_of_body_size_studies, 
             ") that used body size"))
print(paste0("number of unique (", 
             number_of_unique_traits, 
             ") traits"))
print(paste0("percentage of traits (",
             percent_of_unique_traits_that_are_size,
             "%) that were about size"))
print(paste0("number of studies (", 
             number_of_size_studies,
             ") that used a size-based trait"))
print(paste0("average number of times (", 
             number_of_size_studies_avg_use,
             "%) that a particular size-based trait was used"))
print(paste0("average number of times (", 
             number_of_nonsize_studies_avg_use,
             "%) that a particular non-size-based trait was used"))
print(paste0("number of secondary trait (", 
             number_of_secondary_traits,
             ") classifications, and ",
             number_of_primary_traits, "primary trait classifications"))
print(paste0("percentage of secondary traits (", 
             percentage_of_secondary_traits_used_once,
             "%) that only appeared in one paper"))
print(paste0("percentage of secondary traits (", 
             percentage_of_secondary_traits_used_more_than_10_times,
             "%) that appeared in 10 or more papers"))
print(paste0("number of physiological (", 
             number_of_physiological_studies,") traits"))
print(paste0("number of morphological (", 
             number_of_morphology_studies,") traits"))
print(paste0("number of behaviour (", 
             number_of_behaviour_studies,") traits"))
print(paste0("number of life history (", 
             number_of_lifehistory_studies,") traits"))
print(paste0("percentage of global change (", 
             percentage_of_global_change_studies,") studies"))
print(paste0("percentage of forecasting (", 
             percentage_of_forecasting_studies,") studies"))
print(paste0("percentage of papers about (", 
             percentage_of_global_change_drivers['Climate Change'],
             ") Climate Change"))
print(paste0("percentage of papers about (", 
             percentage_of_global_change_drivers['Invasion'],
             ") Invasion"))
print(paste0("percentage of papers about (", 
             percentage_of_global_change_drivers['Habitat Degredation'],
             ") Habitat Degredation"))
print(paste0("percentage of papers about (", 
             percentage_of_global_change_drivers['Global Change Broad'],
             ") Global Change Broad"))
print(paste0("percentage of papers about (", 
             percentage_of_global_change_drivers['Exploitation'],
             ") Exploitation"))
print(paste0("percentage of predictive papers (", 
             percentage_of_predictive_papers_since_2011,
             ") published since 2011"))
print(paste0("percentage of predictive papers (", 
             percentage_of_predictive_papers_since_2015,
             ") published since 2015"))
print(paste0("percentage of predictive papers focused on applying plant", 
             " traits to predict the outcomes of abiotic ",
             "environmental(", predictive_papers_focused_on_plants,
             ") filtering in terrestrial ecosystems"))
print(paste0("number of predictive papers that look at ",
             "global change (", number_of_predictive_global_change_papers, 
             ", (", percentage_of_predictive_global_change_papers, "))"))
print(paste0("percentage of predictive papers about (", 
             predictive_types_of_global_change['Climate Change'],
             ") Climate Change"))
print(paste0("percentage of predictive papers about (", 
             predictive_types_of_global_change['Invasion'],
             ") Invasion"))
print(paste0("percentage of predictive papers about (", 
             predictive_types_of_global_change['Habitat Degredation'],
             ") Habitat Degredation"))
print(paste0("percentage of predictive papers about (", 
             predictive_types_of_global_change['Global Change Broad'],
             ") Global Change Broad"))
print(paste0("percentage of predictive papers about (", 
             predictive_types_of_global_change['Exploitation'],
             ") Exploitation"))
print(paste0("number of single trait (", 
             number_of_three_or_more_trait_type_studies,
             ") studies"))
print(paste0("number of studies (", 
             number_of_single_trait_type_studies,
             ") with 3 or more traits"))



