########## 
##########
# This code contains the data cleaning component of the analysis presented in
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-15
##########
##########

# set-up =======================================================================

library(tidyverse)
library(here)
`%notin%` = Negate(`%in%`)

primary_traits_abund = 
  read_csv(here(paste0('./data/processed-data',
                       '/primary_traits_dummy_abundance.csv')))
primary_traits_pa = 
  read_csv(here(paste0('./data/processed-data',
                       '/primary_traits_dummy.csv')))
secondary_traits_abund = 
  read_csv(here(paste0('./data/processed-data',
                       '/secondary_traits_dummy_abundance.csv')))
secondary_traits_pa = 
  read_csv(here(paste0('./data/processed-data',
                       '/secondary_traits_empty_dummy.csv')))

# clean primary abundance dataset ==============================================

primary_traits_abund_clean = primary_traits_abund %>%
  select(-`X1`) %>% 
  group_by(DOI) %>%
  sample_n(1) %>%
  dplyr::rename(Predictive = Forecasting,
                Filter = filter) %>%
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"), 
         PredictiveCat=if_else(Predictive>0, "yes", "no"), 
         TaxonomicGroup =case_when( 
           ###### begin NOTE ###################################################
           # Note can use case_when() or if_else() BUT you need to write one for 
           # each value and not try to combine them as strings using c()
           ###### end NOTE #####################################################
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
          here(paste0('./data/processed-data',
                      '/primary_traits_dummy_abundance_models.csv')))

# clean secondary abundance dataset ============================================

secondary_traits_abund_clean = secondary_traits_abund %>%
  group_by(DOI) %>%
  sample_n(1) %>%
  dplyr::rename(Predictive = Forecasting, 
                Filter = filter) %>% #Need to rename Predictive values
  # Add binary column for presence/absence of Global Change Driver 
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"), 
         # Add binary column for presence/absence of predictive work
         PredictiveCat=if_else(Predictive>0, "yes", "no"),  
         TaxonomicGroup=case_when( 
           ###### begin NOTE ###################################################
           # Note can use case_when() or if_else() BUT you need to write one for 
           # each value and not try to combine them as strings using c()
           ###### end NOTE #####################################################
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
          here(paste0('./data/processed-data',
                      '/secondary_traits_dummy_abundance_models.csv')))

# clean primary PA dataset =====================================================

primary_traits_pa_clean = primary_traits_pa %>%
  select(-`X1`) %>% 
  group_by(DOI) %>%
  sample_n(1) %>%
  dplyr::rename(Predictive = Forecasting,
                Filter = filter) %>%
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"), 
         PredictiveCat=if_else(Predictive>0, "yes", "no"), 
         TaxonomicGroup =case_when( 
           ###### begin NOTE ###################################################
           # Note can use case_when() or if_else() BUT you need to write one for 
           # each value and not try to combine them as strings using c()
           ###### end NOTE #####################################################
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
          here(paste0('./data/processed-data',
                      '/primary_traits_dummy_pa_models.csv')))

# clean secondary PA dataset =====================================================

secondary_traits_pa_clean = secondary_traits_pa %>%
  select(-`X1`) %>% 
  group_by(DOI) %>%
  sample_n(size = 1) %>%
  dplyr::rename(Predictive = Forecasting,
                Filter = filter) %>% #Need to rename Predictive values
  # Add binary column for presence/absence of Global Change Driver 
  mutate(GlobalChangeCat=if_else(GlobalChange>0, "yes", "no"),
         # Add binary column for presence/absence of predictive work
         PredictiveCat=if_else(Predictive>0, "yes", "no"),  
         TaxonomicGroup=case_when( 
           ###### begin NOTE ###################################################
           # Note can use case_when() or if_else() BUT you need to write one for 
           # each value and not try to combine them as strings using c()
           ###### end NOTE #####################################################
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
write_csv(secondary_traits_pa_clean, 
          here(paste0('./data/processed-data',
                      '/secondary_traits_dummy_pa_models.csv')))

