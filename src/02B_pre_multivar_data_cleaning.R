########## 
##########
# This code contains the data cleaning component of the analysis presented in
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Natasha A. HArdy
# DATE OF CREATION: 2022-03-21
##########
##########

# set-up =======================================================================

library(tidyverse)
library(readr)
library(here)
`%notin%` = Negate(`%in%`)

review_traits_0 <- read_csv(here(paste0('./data/unprocessed-data',
                                        '/traits_dummy_old.csv')))

# clean trait variables in dataset =============================================

summary(as.factor(review_traits_0$`Taxonomic`))

review_traits_0 = review_traits_0 %>%
  dplyr::rename(Predictive = `Forecasting/Predictive`, yield = `yeild`) %>%
  mutate(`Global Change`=if_else(review_traits_0$`Global Change Driver`>0, "yes", "no"), 
         #Add binary column for presence/absence of Global Change Driver
         `Taxonomic Group`=case_when( 
           ###### begin NOTE ###################################################
           # Note can use case_when() or if_else() BUT you need to write one for 
           # each value and not try to combine them as strings using c()
           ###### end NOTE #####################################################
           review_traits_0$`Taxonomic` == "Mammals" ~ "Vertebrate",
           review_traits_0$`Taxonomic` == "Birds" ~ "Vertebrate",
           review_traits_0$`Taxonomic` == "Fish" ~ "Vertebrate",
           review_traits_0$`Taxonomic` == "Herps" ~ "Vertebrate",
           review_traits_0$`Taxonomic` == "Plankton" ~ "Invertebrate",
           review_traits_0$`Taxonomic` == "Insects" ~ "Invertebrate",
           review_traits_0$`Taxonomic` == "Multiple" ~ "Multiple",
           review_traits_0$`Taxonomic` == "Broad" ~ "Multiple",
           review_traits_0$`Taxonomic` == "Other" ~ "Other",
           review_traits_0$`Taxonomic` == "Plants" ~ "Plants")
  ) %>%
  select(`DOI`:`Taxonomic`,`Taxonomic Group`,`System`:`Global Change Driver`, 
         `Global Change`, TOS:Filter, `# of eggs or live young`:`zoogeographical group`)

dim(review_traits_0) #2596 (-8) trait variables that occur in any of 1228 rows of data

# Remove zero sum columns and rows from the dataframe ===========================

#Remove columns that only contain a single occurrence for a trait, as well as any zero sum rows
review_traits.1 = review_traits_0[,-which(colSums(review_traits_0[,11:ncol(review_traits_0)])<2)]
#Not sure why some explanatory variable columns just disappear, so adding them back in:
review_traits.2 = cbind(review_traits_0[,1:10], review_traits.1[,4:ncol(review_traits.1)])
#Then delete rows that sum to zero for the trait occurrences
review_traits1.0 = review_traits.2[-which(rowSums(review_traits.2[11:ncol(review_traits.2)])==0),] 
#1124 observations of 1415 traits

# Randomly select a single replicate per DOI ====================================

review_traits_clean = review_traits1.0 %>% 
  group_by(DOI) %>%
  sample_n(1)

#formerly str(review_traits4.0)
#Check data
summary(as.factor(review_traits_clean$Filter))


write_csv(review_traits_clean, 
          here(paste0('./data/processed-data',
                      '/review_traits_clean_models.csv')))
