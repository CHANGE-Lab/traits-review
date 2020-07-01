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
secondary_traits = read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_pa_models.csv'))
secondary_categorical = secondary_traits[,1:10]
secondary_dummy = secondary_traits[,11:ncol(secondary_traits)]

######################## Ecosystem

simper_secondary_pa_ecos = simper(secondary_dummy, secondary_categorical$Ecosystem, permutations = 1000)

summary_ecos = summary(simper_secondary_pa_ecos)

write_csv(simper_secondary_pa_ecos$Freshwater_Terrestrial, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_ecos_Freshwater_Terrestrial.csv"))
write_csv(simper_secondary_pa_ecos$Freshwater_Marine, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_ecos_Freshwater_Marine.csv"))
write_csv(simper_secondary_pa_ecos$Freshwater_Broad, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_ecos_Freshwater_Broad.csv"))
write_csv(simper_secondary_pa_ecos$Terrestrial_Marine, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_ecos_Terrestrial_Marine.csv"))
write_csv(simper_secondary_pa_ecos$Terrestrial_Broad, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_ecos_Terrestrial_Broad.csv"))
write_csv(simper_secondary_pa_ecos$Marine_Broad, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_ecos_Marine_Broad.csv"))

simper_secondary_pa_filter = simper(secondary_dummy, secondary_categorical$Filter, permutations = 1000)

######################## Filter

summary_filter = summary(simper_secondary_pa_filter)

write_csv(simper_secondary_pa_filter$Ecological_Fundamental, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_filter_Ecological_Fundamental.csv"))
write_csv(simper_secondary_pa_filter$Ecological_Physical, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_filter_Ecological_Physical.csv"))
write_csv(simper_secondary_pa_filter$Ecological_Trophic, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_filter_Ecological_Trophic.csv"))
write_csv(simper_secondary_pa_filter$Fundamental_Physical, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_filter_Fundamental_Physical.csv"))
write_csv(simper_secondary_pa_filter$Fundamental_Trophic, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_filter_Fundamental_Trophic.csv"))
write_csv(simper_secondary_pa_filter$Physical_Trophic, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_filter_Physical_Trophic.csv"))

######################## Global Change

simper_secondary_pa_gc = simper(secondary_dummy, secondary_categorical$GlobalChangeCat, permutations = 1000)

summary_gc = summary(simper_secondary_pa_gc)

write_csv(simper_secondary_pa_gc$yes_no, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_gc_yes_no.csv"))

######################## Type of Study

simper_secondary_pa_tos = simper(secondary_dummy, secondary_categorical$TOS, permutations = 1000)

summary_tos = summary(simper_secondary_pa_tos)

write_csv(simper_secondary_pa_tos$Observational_Experiment, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Observational_Experiment.csv"))
write_csv(simper_secondary_pa_tos$Observational_Review, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Observational_Review.csv"))
write_csv(simper_secondary_pa_tos$Observational_Metanalysis, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Observational_Metanalysis.csv"))
write_csv(simper_secondary_pa_tos$Observational_TModel, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Observational_TModel.csv"))
write_csv(simper_secondary_pa_tos$Experiment_Review, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Experiment_Review.csv"))
write_csv(simper_secondary_pa_tos$Experiment_Metanalysis, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Experiment_Metanalysis.csv"))
write_csv(simper_secondary_pa_tos$Experiment_TModel, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Experiment_TModel.csv"))
write_csv(simper_secondary_pa_tos$Review_Metanalysis, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Review_Metanalysis.csv"))
write_csv(simper_secondary_pa_tos$Review_TModel, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Review_TModel.csv"))
write_csv(simper_secondary_pa_tos$Metanalysis_TModel, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_tos_Metanalysis_TModel.csv"))

######################## Prediction

simper_secondary_pa_predict = simper(secondary_dummy, secondary_categorical$Predictive, permutations = 1000)

write_csv(simper_secondary_pa_predict$`0_1`, 
          here("./Data/Cole-Output-Simper/Secondary-Classification-PA/simper_secondary_pa_PREDICT.csv"))


