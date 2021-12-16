########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-30
##########
##########

library(knitr)
library(tidyverse)
library(vegan)
library(viridis)
library(PNWColors)
library(mvabund)
library(reshape2)
library(here)

################################## Simper for the different groups

#load data
original_traits = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/original_traits_dummy.csv'))
oiriginal_categorical = original_traits[,1:8]
oiriginal_dummy = original_traits[,9:ncol(original_traits)]

######################## Ecosystem

simper_original_ecos = simper(oiriginal_dummy, oiriginal_categorical$Ecosystem, permutations = 1000)

summary_ecos = summary(simper_original_ecos)

write_csv(simper_original_ecos$Freshwater_Terrestrial, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_ecos_Freshwater_Terrestrial.csv"))
write_csv(simper_original_ecos$Freshwater_Marine, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_ecos_Freshwater_Marine.csv"))
write_csv(simper_original_ecos$Freshwater_Broad, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_ecos_Freshwater_Broad.csv"))
write_csv(simper_original_ecos$Terrestrial_Marine, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_ecos_Terrestrial_Marine.csv"))
write_csv(simper_original_ecos$Terrestrial_Broad, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_ecos_Terrestrial_Broad.csv"))
write_csv(simper_original_ecos$Marine_Broad, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_ecos_Marine_Broad.csv"))

simper_original_filter = simper(oiriginal_dummy, oiriginal_categorical$filter, permutations = 1000)

######################## Filter

summary_filter = summary(simper_original_filter)

write_csv(simper_original_filter$Ecological_Fundamental, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_filter_Ecological_Fundamental.csv"))
write_csv(simper_original_filter$Ecological_Physical, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_filter_Ecological_Physical.csv"))
write_csv(simper_original_filter$Ecological_Trophic, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_filter_Ecological_Trophic.csv"))
write_csv(simper_original_filter$Fundamental_Physical, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_filter_Fundamental_Physical.csv"))
write_csv(simper_original_filter$Fundamental_Trophic, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_filter_Fundamental_Trophic.csv"))
write_csv(simper_original_filter$Physical_Trophic, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_filter_Physical_Trophic.csv"))

######################## Global Change

simper_original_gc = simper(oiriginal_dummy, oiriginal_categorical$GlobalChange, permutations = 1000)

summary_gc = summary(simper_original_gc)

write_csv(simper_original_gc$yes_no, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_gc_yes_no.csv"))

######################## Type of Study

simper_original_tos = simper(oiriginal_dummy, oiriginal_categorical$TOS, permutations = 1000)

summary_tos = summary(simper_original_tos)

write_csv(simper_original_tos$Observational_Experiment, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Observational_Experiment.csv"))
write_csv(simper_original_tos$Observational_Review, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Observational_Review.csv"))
write_csv(simper_original_tos$Observational_Metanalysis, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Observational_Metanalysis.csv"))
write_csv(simper_original_tos$Observational_TModel, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Observational_TModel.csv"))
write_csv(simper_original_tos$Experiment_Review, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Experiment_Review.csv"))
write_csv(simper_original_tos$Experiment_Metanalysis, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Experiment_Metanalysis.csv"))
write_csv(simper_original_tos$Experiment_TModel, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Experiment_TModel.csv"))
write_csv(simper_original_tos$Review_Metanalysis, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Review_Metanalysis.csv"))
write_csv(simper_original_tos$Review_TModel, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Review_TModel.csv"))
write_csv(simper_original_tos$Metanalysis_TModel, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_tos_Metanalysis_TModel.csv"))

######################## Prediction

simper_original_predict = simper(oiriginal_dummy, oiriginal_categorical$Forecasting, permutations = 1000)

write_csv(simper_original_predict$`0_1`, 
          here("./Data/Cole-Output-Simper/Original-Classification/simper_original_PREDICT.csv"))


