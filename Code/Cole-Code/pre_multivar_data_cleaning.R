########## 
##########
# This code contains the data cleaning component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-15
##########
##########

library(tidyverse)
library(here)
`%notin%` = Negate(`%in%`)

primary_traits_abund = read_csv(here('./data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_abundance.csv'))
secondary_traits_abund = read_csv(here('./data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_abundance.csv'))
secondary_traits_pa = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_empty_dummy.csv'))
primary_traits_pa = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy.csv'))



#clean up the dataframes and make sure they're formatted properly - start with the abundance ones
primary_traits_abund_clean = primary_traits_abund %>%
  select(-`X1`) %>% 
  group_by(DOI) %>%
  sample_n(1) %>%
  dplyr::rename(Predictive = Forecasting,
                Filter = filter) %>%
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"), 
         PredictiveCat=if_else(Predictive>0, "yes", "no"), 
         TaxonomicGroup =case_when( 
           #Note can use case_when() or if_ele() BUT you need to write one for each value and not try to combine them as strings using c()
           Taxonomic == "Mammals" ~ "Vertebrate",
           Taxonomic == "Birds" ~ "Vertebrate",
           Taxonomic == "Fish" ~ "Vertebrate",
           Taxonomic == "Herpetofauna" ~ "Vertebrate",
           Taxonomic == "Plankton" ~ "Invertebrate",
           Taxonomic == "Insects" ~ "Invertebrate",
           Taxonomic == "Multiple" ~ "Multiple",
           Taxonomic == "Other" ~ "Other",
           Taxonomic == "Plants" ~ "Plants")
  ) %>%
  select(DOI:GlobalChange,GlobalChangeCat, PredictiveCat,
         Predictive:survival) %>%
  filter(TOS != "NA")
write_csv(primary_traits_abund_clean, 
          here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_abundance_models.csv'))

secondary_traits_abund_clean = secondary_traits_abund %>%
  group_by(DOI) %>%
  sample_n(1) %>%
  dplyr::rename(Predictive = Forecasting, 
                Filter = filter) %>% #Need to rename Predictive values
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"), #Add binary column for presence/absence of Global Change Driver 
         PredictiveCat=if_else(Predictive>0, "yes", "no"), ##Add binary column for presence/absence of predictive work 
         TaxonomicGroup=case_when( #Note can use case_when() or if_ele() BUT you need to write one for each value and not try to combine them as strings using c()
           Taxonomic == "Mammals" ~ "Vertebrate",
           Taxonomic == "Birds" ~ "Vertebrate",
           Taxonomic == "Fish" ~ "Vertebrate",
           Taxonomic == "Herpetofauna" ~ "Vertebrate",
           Taxonomic == "Plankton" ~ "Invertebrate",
           Taxonomic == "Insects" ~ "Invertebrate",
           Taxonomic == "Multiple" ~ "Multiple",
           Taxonomic == "Other" ~ "Other",
           Taxonomic == "Plants" ~ "Plants")
  ) %>%
  select(DOI:GlobalChange,GlobalChangeCat, PredictiveCat, 
         Predictive:zinc) 
write_csv(secondary_traits_abund_clean, 
          here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_abundance_models.csv'))

#clean the non-abundance ones too
primary_traits_pa_clean = primary_traits_pa %>%
  select(-`X1`) %>% 
  group_by(DOI) %>%
  sample_n(1) %>%
  dplyr::rename(Predictive = Forecasting,
                Filter = filter) %>%
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"), 
         PredictiveCat=if_else(Predictive>0, "yes", "no"), 
         TaxonomicGroup =case_when( 
           #Note can use case_when() or if_ele() BUT you need to write one for each value and not try to combine them as strings using c()
           Taxonomic == "Mammals" ~ "Vertebrate",
           Taxonomic == "Birds" ~ "Vertebrate",
           Taxonomic == "Fish" ~ "Vertebrate",
           Taxonomic == "Herpetofauna" ~ "Vertebrate",
           Taxonomic == "Plankton" ~ "Invertebrate",
           Taxonomic == "Insects" ~ "Invertebrate",
           Taxonomic == "Multiple" ~ "Multiple",
           Taxonomic == "Other" ~ "Other",
           Taxonomic == "Plants" ~ "Plants")
  ) %>%
  select(DOI:GlobalChange,GlobalChangeCat:PredictiveCat,
         Predictive:survival) 
write_csv(primary_traits_pa_clean, 
          here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_pa_models.csv'))

secondary_traits_pa_clean = secondary_traits_pa %>%
  select(-`X1`) %>% 
  group_by(DOI) %>%
  sample_n(1) %>%
  dplyr::rename(Predictive = Forecasting,
                Filter = filter) %>% #Need to rename Predictive values
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"), #Add binary column for presence/absence of Global Change Driver 
         PredictiveCat=if_else(Predictive>0, "yes", "no"), ##Add binary column for presence/absence of predictive work 
         TaxonomicGroup=case_when( #Note can use case_when() or if_ele() BUT you need to write one for each value and not try to combine them as strings using c()
           Taxonomic == "Mammals" ~ "Vertebrate",
           Taxonomic == "Birds" ~ "Vertebrate",
           Taxonomic == "Fish" ~ "Vertebrate",
           Taxonomic == "Herpetofauna" ~ "Vertebrate",
           Taxonomic == "Plankton" ~ "Invertebrate",
           Taxonomic == "Insects" ~ "Invertebrate",
           Taxonomic == "Multiple" ~ "Multiple",
           Taxonomic == "Other" ~ "Other",
           Taxonomic == "Plants" ~ "Plants")
  ) %>%
  select(DOI:GlobalChange,GlobalChangeCat, PredictiveCat, 
         Predictive:zinc) 
write_csv(secondary_traits_abund_clean, 
          here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_pa_models.csv'))
