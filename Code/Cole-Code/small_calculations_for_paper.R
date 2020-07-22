########## 
##########
# This code contains some back-of-the-envelope calculations
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-07-15
##########
##########

library(tidyverse)
library(here)
`%notin%`= Negate(`%in%`)

original_dummy = 
  read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/original_traits_dummy.csv'))

primary_pa_dummy = 
  read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_pa_models.csv'))

orig_categorical = original_dummy[, 1:8]
orig_dums = original_dummy[, c(1, 9:ncol(original_dummy))]
orig_dums = orig_dums %>% 
  distinct()
orig_dums$sums = rowSums(orig_dums[, 2:ncol(orig_dums)])

test_orig = original_dummy

test_orig$sums = rowSums(test_orig[,9:ncol(test_orig)])
test_orig = test_orig %>% 
  select(DOI, sums) %>% 
  distinct()
hist(test_orig$sums)
table(test_orig$sums)

test_prim = primary_pa_dummy

test_prim$sums = rowSums(test_prim[,11:ncol(test_prim)])
test_prim = test_prim %>% 
  select(DOI, sums) %>% 
  distinct()
hist(test_prim$sums)
table(test_orig$sums)


paper_wise = original_dummy[, 1:8]
paper_wise$DOI = as.factor(paper_wise$DOI)
paper_wise$TT = as.factor(paper_wise$TT)
paper_wise$filter = as.factor(paper_wise$filter)

paper_info = data.frame(
  DOI = character(0), 
  TT = integer(0),
  Filter = integer(0),
  No_Traits = integer(0)
)


for(DOI in as.character(unique(paper_wise$DOI))) {
  
  temp = paper_wise[which(paper_wise$DOI == DOI),]
  temp_1 = orig_dums[which(orig_dums$DOI == DOI),ncol(orig_dums)]
  
  paper_info_each = data.frame(
    DOI = character(1), 
    TT = integer(1),
    Filter = integer(1),
    No_Traits = integer(1)
  )
  
  paper_info_each$DOI[1] = as.character(temp$DOI[1])
  paper_info_each$TT = length(unique(temp$TT))
  paper_info_each$Filter = length(unique(temp$filter))
  paper_info_each$No_Traits = temp_1$sums
  
  paper_info = rbind(
    paper_info,
    paper_info_each
  )
}
table(paper_info$No_Traits)
table(paper_info$TT)
table(paper_info$Filter)

orig_categorical$DOI = as.factor(orig_categorical$DOI)

full_db = read_csv(here('./Data/Cole-Original-Data/finalized_lit_db_for_r.csv'))
unique(full_db$`Relevant to Study`)

full_db = full_db %>% 
  filter(`Relevant to Study` %in% c('Y', 'y'))
full_db$trait_sum = rowSums(full_db[, c(13,14,16,17)],) 
table(full_db$trait_sum)

taxonomic = orig_categorical[, c(1,3)]
taxonomic = taxonomic %>% 
  distinct()
table(taxonomic$Taxonomic)
286/822

tt = orig_categorical[, c(1,3,7)]
plants= tt %>% 
  filter(Taxonomic == 'Plants') %>% 
  select(DOI) %>% 
  distinct()
plants_morph = tt %>% 
  filter(Taxonomic == 'Plants' & TT == 'Morphological') %>% 
  distinct()
plants_all_non_morph = tt %>% 
  filter(Taxonomic == 'Plants' & TT != 'Morphological') %>% 
  distinct()

ecosystem = orig_categorical[, 1:2]
ecosystem = ecosystem %>% distinct()
table(ecosystem$Ecosystem)


primary_abundance =
  read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_abundance_models.csv')) 

primary_abund_sums = colSums(primary_abundance[,c(11:19)])

all_trait_instances = colSums(original_dummy[, 9:ncol(original_dummy)])

sum(22+337+366+168+380+773+1901+629+62)
sum(all_trait_instances)
original_dummy_copy = original_dummy

original_dummy_copy = original_dummy_copy[, -c(2:9)]

original_dummy_copy = original_dummy_copy %>% 
  distinct()

sum(original_dummy_copy$`body size`)
sum(original_dummy_copy$`body length`)
sum(original_dummy_copy$`body dry mass`)
sum(original_dummy_copy$`body mass`)
sum(original_dummy_copy$SLA)

original_dummy_copy$sums = rowSums(original_dummy_copy[, 2:ncol(original_dummy_copy)])
original_dummy_copy[which(original_dummy_copy$sums == 55), 1]
max(original_dummy_copy$sums)
sum(original_dummy_copy$sums)
x = colSums(original_dummy_copy[, 2:ncol(original_dummy_copy)])
sum(x)
sum(matrix(original_dummy_copy[, 2:ncol(original_dummy_copy)]))
sum(original_dummy_copy[,2:ncol(original_dummy_copy)])
sum(primary_abundance[,c(11:19)])
max(primary_abundance[,c(11:19)])


primary_abundance =
  read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_abundance_models.csv'))



 
original_dummy_tax_group = original_dummy %>% 
  mutate(TaxonomicGroup = 
           ifelse(original_dummy$Taxonomic %in% 
                    c('Mammals', 'Birds', 'Herpetofauna', 'Fish'), 'Vertebrates', 
                  ifelse(original_dummy$Taxonomic %in%
                           c('Insects', 'Plankton'), 'Invertebrates', 
                         ifelse(original_dummy$Taxonomic == 'Other', 'Other',
                                ifelse(original_dummy$Taxonomic == 'Multiple', 'Multiple',
                                       ifelse(original_dummy$Taxonomic == 'Plants', 'Plants', NA))))))

verts = original_dummy_tax_group %>% 
  filter(TaxonomicGroup %in% c('Vertebrates', 'Invertebrates'))
verts = verts[, -c(2:9)]
verts = verts %>% 
  distinct()
verts_traits = colSums(verts[, 2:2561])
max(verts_traits)
verts_traits[which(verts_traits == 63)]

global_change = original_dummy[, c('DOI', 'GlobalChange')]
global_change = global_change %>% 
  distinct()

table(global_change$GlobalChange)
51+7+23+5+64+37

predictive = original_dummy[, c('DOI', 'Forecasting', 'GlobalChange')]
predictive = predictive %>% 
  filter(Forecasting == 1) %>% 
  filter(GlobalChange %in% c('Global Change Broad', 'Global Change Multiple')) %>% 
  distinct()
quick_dois = unique(predictive$DOI)

pred_multiple = original_dummy %>% 
  filter(DOI %in% quick_dois)

8/27

table(predictive$Forecasting)

#22 predictive papers published 2010 or later 
22/27
13/27

tt = orig_categorical[, c(1,7)]
tt = tt %>% distinct()
table(tt$TT)


filter = orig_categorical[, c(1,8)]
filter = filter %>% distinct()
table(filter$filter)



secondary_pa_dummy = 
  read_csv(here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_pa_models.csv'))

secondary_pa_dummy %>% 
  filter(size == 1)
secondary_dummy_traits = secondary_pa_dummy[,11:ncol(secondary_pa_dummy)]
secondary_counts = colSums(secondary_dummy_traits)
table(secondary_counts)


temp = secondary_dummy_traits %>% 
  select(-size)
secondary_all_traits_count = colSums(temp)
rm(temp)
mean(secondary_all_traits_count)
secondary_all_traits_count = x[!secondary_all_traits_count == 'size']

size_based_traits = 
  read_csv(here('./Data/Cole-Original-Data/size_based_traits.csv'))
orig_size = orig_dums %>% 
  select(-sums)

original_not_size = names(orig_size)[!(names(orig_size) %in% 
                                         unique(size_based_traits$Trait_spell_corrected))]
original_size = names(orig_size)[!(names(orig_size) %notin% 
                                         unique(size_based_traits$Trait_spell_corrected))]
original_not_size = orig_size[, original_not_size]

original_size = orig_size[, original_size]
orig_size_counts = colSums(original_size[,2:ncol(original_size)])
orig_not_size_counts = colSums(original_not_size[,2:ncol(original_not_size)])
mean(orig_size_counts)
 
hist(orig_not_size_counts)

mean(orig_not_size_counts)


original_size[rowSums(original_size > 0) >= 1, ]


trait_levels = read_csv(here('./Data/Cole-Original-Data/trait_levels_types_clean.csv'))
trait_levels = trait_levels %>% 
  distinct(Trait_spell_corrected, Type)
table(trait_levels$Type)
