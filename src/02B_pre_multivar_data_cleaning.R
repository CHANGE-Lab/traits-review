########## 
##########
# This code contains the data cleaning component of the analysis presented in
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Natasha A. Hardy
# DATE OF CREATION: 2022-03-21
##########
##########

# set-up =======================================================================

library(tidyverse)
library(readr)
library(pander)
library(here)
`%notin%` = Negate(`%in%`)

review_traits_0 = read.csv(paste(here("./data/unprocessed-data/traits_dummy_old.csv"),
                              sep=""), check.names=FALSE, row.names = 1)
  
# clean trait variables in dataset =============================================

summary(as.factor(review_traits_0$`Taxonomic`))

review_traits_0 = review_traits_0 %>%
  dplyr::rename(Predictive = `Forecasting/Predictive`, yield = `yeild`) %>%
  mutate(`Global Change`=if_else(review_traits_0$`Global Change Driver`>0, "yes", "no"),
         PredictiveCat=if_else(Predictive>0, "yes", "no"), 
         Taxonomic =case_when( 
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
  select(`DOI`:`Taxonomic`,`System`:`Global Change Driver`, 
         `Global Change`, TOS:Filter, `# of eggs or live young`:`zoogeographical group`)

dim(review_traits_0) #2596 (-9) trait variables that occur in any of 1228 rows of data

# Remove zero sum columns and rows from the dataframe ===========================

#Remove columns that only contain a single occurrence for a trait, as well as any zero sum rows
review_traits.1 = review_traits_0[,-which(colSums(review_traits_0[,10:ncol(review_traits_0)])<2)]
#Not sure why some explanatory variable columns just disappear, so adding them back in:
review_traits.2 = cbind(review_traits_0[,1:9], review_traits.1[,4:ncol(review_traits.1)])
#Then delete rows that sum to zero for the trait occurrences
review_traits1.0 = review_traits.2[-which(rowSums(review_traits.2[10:ncol(review_traits.2)])==0),] 
#1099 observations of 1411 traits

# Convert to presence/absence ===================================================
review_traits_temp = review_traits1.0[10:ncol(review_traits1.0)]
review_traits_temp[review_traits_temp>0] = 1
review_traits_pa = cbind(review_traits1.0[,1:9], review_traits_temp)

range(review_traits_pa[10:ncol(review_traits_pa)])

# Randomly select a single replicate per DOI ====================================

review_traits_clean = review_traits_pa %>% 
  group_by(DOI) %>%
  sample_n(1)

#formerly str(review_traits4.0)
#Check data
summary(as.factor(review_traits_clean$Filter))

write_csv(review_traits_clean, 
          here(paste0('./data/processed-data',
                      '/review_traits_clean_models.csv')))
