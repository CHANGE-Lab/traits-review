########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-07-03•
##########
##########

library(devtools)
library(knitr)
library(tidyverse)
library(vegan)
library(viridis)
library(PNWColors)
library(mvabund)
library(reshape2)
library(here)

#subset data into the two groups needed
secondary_abundance = 
  read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_abundance_models.csv'))
secondary_abundance_species = 
  data.frame(secondary_abundance[,11:ncol(secondary_abundance)])
secondary_abundance_traits = data.frame(secondary_abundance[,1:10])
secondary_abundance_mv = mvabund(secondary_abundance_species)

#figure out what error distribution works
secondary_check_matrix = as.matrix(secondary_abundance_species)
hist(secondary_check_matrix)
hist(secondary_check_matrix[secondary_check_matrix > 0])

#try poisson and neg. binom.
mv_pred_poisson = manyglm(secondary_abundance_mv ~ 
                          secondary_abundance_traits$PredictiveCat, 
                        data= secondary_abundance_traits,
                        family = 'poisson')
plot(mv_pred_poisson) 
qqnorm(residuals(mv_pred_poisson)[which(residuals(mv_pred_poisson)<10000 &
                                          residuals(mv_pred_poisson)>-10000)])

mv_pred_nb = manyglm(secondary_abundance_mv ~ 
                     secondary_abundance_traits$PredictiveCat, 
                   data= secondary_abundance_traits,
                   family = 'negative.binomial')
#family=binomial("cloglog")) 
plot(mv_pred_nb) 
qqnorm(residuals(mv_pred_nb)[which(residuals(mv_pred_nb)<10000)])

# NOTE - okay, neg. binom definitely  better but I'll save both just in case
saveRDS(mv_pred_nb, here('./Data/Cole-Output-ManyGLM/mv_pred_nb.rds')) 
saveRDS(mv_pred_poisson, here('./Data/Cole-Output-ManyGLM/mv_pred_poisson.rds')) 

#model output significance test
mv_pred_nb_an = anova.manyglm(mv_pred_nb)
saveRDS(mv_pred_nb_an, here('./Data/Cole-Output-ManyGLM/mv_pred__nb_anova.rds')) 
#mv_pred_nb_an = readRDS(here('./Data/Cole-Output-ManyGLM/mv_pred__nb_anova.rds'))
write_csv(mv_pred_nb_an$table, 
          here('./Data/Cole-Output-ManyGLM/mv_pred_nb_anova_table.csv')) 

#individual adjusted p-values for species/traits - get univariate p-values
mv_pred_nb_an_uni = anova.manyglm(mv_pred_nb,p.uni="adjusted") 
saveRDS(mv_pred_nb_an_uni, here('./Data/Cole-Output-ManyGLM/mv_pred_univs.rds')) 
#mv_pred_nb_an_uni = readRDS(here('./Data/Cole-Output-ManyGLM/mv_pred_univs.rds'))
#Get the direction of effect fof each species with the main effect
pred_coef = coef(mv_pred_nb)

#figure out what the top traits are - recall traits are our 'species' here
mv_pred_nb_species = 
  sort(mv_pred_nb_an$uni.test[2,],
       decreasing=T,index.return=T)[1:25] #sort and select top species/traits
mv_pred_nb_species$ix[1:25] #the column numbers of the top  imabundancected spp/traits

#Need > 50% deviance explainaed --> result = 25 traits explain > 50% deviance
sum(mv_pred_nb_an$uni.test[2,mv_pred_nb_species$ix[1:25]])*100/
  sum(mv_pred_nb_an$uni.test[2,]) #25 species explained = 51.99% Deviance

pred_top = 
  data.frame(dimnames(secondary_abundance_species)[[2]][mv_pred_nb_species$ix[
    1:25]]) #df with the names of the top 20 traits
pred_top = pred_top %>% 
  rename('traits' = names(pred_top))
str(pred_top)


#How much deviance explained?
write_csv(pred_top, here('./Data/Cole-Output-ManyGLM/mv_pred_top.csv')) 

#Now combine traits with their coeffs and p-values

#Create df to combine coef values,  also p-values from univ anovas & the top 20
pred_coef_l = data.frame(t(pred_coef)) 
pred_coef_l$traits = rownames(pred_coef_l) #convert rownames to a column
pred_coef_l = pred_coef_l %>% 
  rename('coef_intercept' = `X.Intercept.`, 
         'coef_pred_yes' = names(pred_coef_l)[2])

pred_top_coeffs = merge(pred_top, pred_coef_l,
                      by.x = 'traits',
                      by.y = 'traits') 

colnames(pred_top_coeffs)

#need to join with test statistic values
pred_an_test = as.data.frame(t(mv_pred_nb_an_uni$uni.test)) #first transpose coef_filter
pred_an_test$traits = rownames(pred_an_test) #convert rownames to a column

pred_an_test = pred_an_test %>% 
  rename('deviance_explained' = names(pred_an_test)[2])

pred_top_coeffs = merge(pred_top_coeffs,
                      pred_an_test,
                      by.x = 'traits',
                      by.y = 'traits')
pred_top_coeffs = pred_top_coeffs %>%
  select(-"(Intercept)")

#need to join with p-values
pred_an_pvalue = data.frame(t(mv_pred_nb_an_uni$uni.p)) #first transpose coef_filter
pred_an_pvalue$traits = rownames(pred_an_pvalue) #convert rownames to a column

pred_an_pvalue = pred_an_pvalue %>% 
  select(-names(pred_an_pvalue)[1]) 
pred_an_pvalue = pred_an_pvalue%>% 
  rename('p_value' = names(pred_an_pvalue)[1])

pred_top_coeffs = merge(pred_top_coeffs, 
                      pred_an_pvalue,
                      by.x = 'traits',
                      by.y = 'traits') 

write_csv(pred_top_coeffs, here('./Data/Cole-Output-ManyGLM/pred_top_coefs.csv'))

#See how many abundance papers actually have those traits
papers_with_top_25_pred = secondary_abundance_species
top_25_pred = pred_top$traits
papers_with_top_25_pred = papers_with_top_25_pred[top_25_pred]

rownames(papers_with_top_25_pred) = secondary_abundance_traits$DOI
papers_with_top_25_pred = papers_with_top_25_pred[rowSums(papers_with_top_25_pred[, -1])>0, ]
