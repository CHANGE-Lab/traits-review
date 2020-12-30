########## 
##########
# This code contains the data cleaning component of the analysis presented in
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-03-15
##########
##########

# Note to future readers. This code is kind of a mess. I acknowledge this. May 
# the coding gods just be happy it runs and is quasi-reproducible

# set-up =======================================================================
library(tidyverse)
library(data.table)
library(here)
`%notin%` = Negate(`%in%`)

dummy_new = read_csv(here('./Data/Cole-Original-Data/traits_dummy_fixed.csv'))
traits_fixed = read_csv(here('./Data/Cole-Original-Data/traits_fixed.csv'))
func_biogeo_study_data = read_csv(here('./Data/Cole-Original-Data/study_classification_FuncBiogeog_to_append.csv'))
func_biogeo_trait_data = read_csv(here('./Data/Cole-Original-Data/traits_classification_FuncBiogeog_to_append_revised.csv'))

# create data for initial data collection ======================================

all = traits_fixed$DOI
some = as.factor(dummy_new$DOI)
missing_dois = traits_fixed %>% 
  filter(DOI %notin% some) %>% 
  select(DOI) %>% 
  unique()
missing_dois = as.character(unique(missing_dois$DOI))
###### keeping this here for posterity but this problem was solved


##### here, I'm making a new df that has each trait and each DOI paired so I 
##### can pull things based on the DOI and on the traits
# additive = data.frame(DOI = as.character(),
#                       Trait = as.character())
# traits = names(dummy_new)
# pb = txtProgressBar(min = 0, max = nrow(dummy_new), initial = 0) 
# for(i in 1:nrow(dummy_new)) {
#   for(j in 2:ncol(dummy_new)) {
#     if(dummy_new[i,j] != 0) {
#       x = as.character(dummy_new[i,1])
#       y = as.character(traits[j])
#       new = data.frame(DOI = x, Trait = y)
#       additive = rbind(additive,new)
#     } 
#   }
#   setTxtProgressBar(pb,i)
# }
# additive = additive %>% 
#   distinct()
# 
# write_csv(additive, here('./Data/Cole-Original-Data/additive.csv'))
additive = read_csv(here('./Data/Cole-Original-Data/additive.csv'))

dummy_new$sums = rowSums(dummy_new[,c(2:ncol(dummy_new))])
check_add = data.frame(table(additive$DOI)) %>% 
  rename(DOI = Var1, Trait = Freq) %>% 
  distinct()
check_add = check_add[order(check_add$DOI),] #important addition to make sure 
                                             #things are ordered properly
check_orig = data.frame(dummy_new[,c(1,ncol(dummy_new))]) %>% 
  distinct()
check_orig = check_orig[order(check_orig$DOI),]
check_add == check_orig
check_add[500:802,] == check_orig[500:802,]

check_orig[duplicated(check_orig$DOI),]
###### begin NOTE ##############################################################
# ok these above checks ran fine so the number of traits associated with 
# each DOI look fine
# I'm interpreting this as the additive df is doing what I'm asking it to 
# do and we're good on that front
###### end NOTE ################################################################

# find missing DOIs and put them in a df =======================================

#grab traits_fixed df with the DOIs that I'm missing
missing_doi_traits = traits_fixed %>% 
  filter(DOI %in% missing_dois) %>% 
  select(DOI, Traits) %>%  #note, all the traits are actually in the 
                           #trait_levels database that we went through, 
                           #so I'll just join here
  rename(Trait = Traits)

missing_doi_traits$DOI = as.factor(missing_doi_traits$DOI)

additive = rbind(additive, missing_doi_traits)
n_distinct(additive$DOI) #okay, we have all 822 studies now

#join all the other trait levels to the additive database 
trait_levels = 
  read_csv(here('./Data/Cole-Original-Data/trait_levels_clean.csv'))

###### begin NOTE ##############################################################
# now, there are likely duplicated traits in the trait_levels dataframe, 
# I have to get rid of them so I can make proper pairs to them 
###### end NOTE ################################################################
dups = trait_levels[duplicated(trait_levels$Trait_spell_corrected),]
dup_df = trait_levels %>% 
  filter(Trait_spell_corrected %in% dups$Trait_spell_corrected)
#after checking visually, all duplicates are true duplicates 
#(i.e. no traits that were the same were coded differently)
#so I can get rid of them now with no problems

trait_levels = trait_levels[!duplicated(trait_levels$Trait_spell_corrected),]

trait_levels_sub = trait_levels %>% 
  select(Trait_spell_corrected, Primary_classification, Secondary_classification) %>% 
  rename(Trait = Trait_spell_corrected)

#do a check to make sure that there are no traits coming in from the dummy set 
#that aren't in the levels set and vice versa
traits_levels_unique = sort(unique(as.character(trait_levels_sub$Trait)))
traits_additive_unique = sort(unique(as.character(additive$Trait)))
missing_traits = #as long as this is empty
  data.frame(setdiff(traits_additive_unique, traits_levels_unique)) 
missing_traits1 = #and this is empty - all the traits that should be there are 
                   #accounted for
  data.frame(setdiff(traits_levels_unique, traits_additive_unique)) 

# Now begin the merge of new and old data ======================================

#first get the trait classifications to the DOIs by the traits
additive_levels = merge(additive, trait_levels_sub, 
                        by.x = 'Trait', by.y = 'Trait')

###### begin NOTE ##############################################################
# did some checking, and dois: "10.1111/1365-2435.13142" & 
# "10.1111/jbi.13171" already exist in previous set of studies
# so i'll remove them as we should go with the first pass on those papers
###### end NOTE ################################################################
func_biogeo_study_data = func_biogeo_study_data %>% 
  filter(Relevance == 1) 

func_biogeo_study_data = func_biogeo_study_data %>% 
  filter(DOI %notin% c("10.1111/1365-2435.13142", "10.1111/jbi.13171"))
func_biogeo_trait_data = func_biogeo_trait_data %>% 
  filter(DOI %notin% c("10.1111/1365-2435.13142", "10.1111/jbi.13171"))

#do a check to make sure that all the dois are in both 
traits_levels_unique_biogeo = 
  sort(unique(as.character(func_biogeo_study_data$DOI)))
traits_additive_unique_biogeo = 
  sort(unique(as.character(func_biogeo_trait_data$DOI)))
missing_dois_biogeo =  #as long as this is empty
  data.frame(setdiff(traits_additive_unique_biogeo,
                     traits_levels_unique_biogeo)) 
missing_dois_biogeo_1 = #and this is empty - all the traits are accounted for
  data.frame(setdiff(traits_levels_unique_biogeo, 
                     traits_additive_unique_biogeo)) 
###### begin NOTE ##############################################################
# so the above two dataframes (missing_dois..) are empty so that means that the
# traits are the same in each dataframe which is what we want. 
# This addresses the issue we had in mid December 2020 with data missing - 
# note it was remedied as there was an issue in pulling the data from the Google
# sheet but now all the traits are there that should be there
###### end NOTE ################################################################

# Make an 'additive' dataframe for new data ====================================

additive_biogeo = func_biogeo_trait_data %>% 
  select(Original_trait, DOI, Primary_classification, Secondary_classification) %>% 
  rename(Trait = Original_trait)
names(additive_levels) == names(additive_biogeo) #make sure this is all 'TRUE' 

additive_levels = rbind(additive_levels, additive_biogeo)
additive_levels = unique(additive_levels)
n_distinct(additive_levels$DOI) # this needs to == 865

#make the dummy datasets for the different levels here
additive_orig_traits = additive_levels %>% 
  select(DOI, Trait)
additive_prim_traits = additive_levels %>% 
  select(DOI, Primary_classification) %>% 
  distinct()

###### begin NOTE ##############################################################
# There are two options here - fill secondary with primary or cut it out, 
# I'll do both but I'll start with option 1
###### end NOTE ################################################################
#option 1
additive_sec_traits_fillempty = additive_levels %>% 
  select(DOI, Primary_classification, Secondary_classification) %>% 
  mutate_all(na_if,"") %>% 
  distinct()
additive_sec_traits_fillempty$Secondary_classification =
  ifelse(is.na(additive_sec_traits_fillempty$Secondary_classification), 
         as.character(additive_sec_traits_fillempty$Primary_classification), 
         as.character(additive_sec_traits_fillempty$Secondary_classification))
additive_sec_traits_fillempty = additive_sec_traits_fillempty %>% 
  select(-Primary_classification) %>% 
  distinct()

#option2
additive_sec_traits_dropempty = additive_levels %>% 
  select(DOI, Secondary_classification) %>% 
  distinct() %>% 
  filter(!is.na(Secondary_classification))

# Perform reshaping to get dummy data ==========================================

orig_traits_multivar = additive_orig_traits %>% 
  mutate(counts = 1) %>% 
  spread(Trait, counts, fill = 0)
prim_traits_multivar = additive_prim_traits %>% 
  mutate(counts = 1) %>% 
  spread(Primary_classification, counts, fill = 0)
sec_fill_traits_multivar = additive_sec_traits_fillempty %>% 
  mutate(counts = 1) %>% 
  spread(Secondary_classification, counts, fill = 0)
sec_empty_traits_multivar = additive_sec_traits_dropempty %>% 
  mutate(counts = 1) %>% 
  spread(Secondary_classification, counts, fill = 0)

#now get the DOI and other info and bring that in
  ##note: just using the old dummy here to check things for peace of mind 
old_dummy = read_csv(here('./Data/Cole-Original-Data/traits_dummy_old.csv'))
old_dummy = old_dummy %>% 
  select(DOI, Ecosystem, Taxonomic, System, `Forecasting/Predictive`, 
         `Global Change Driver`,
         TOS, Filter)
n_distinct(old_dummy$DOI) #note, only 802 here, gotta get the other 20 from the other df

old = old_dummy %>% 
  select(DOI, Filter)
test = old[duplicated(old),]

current = read_csv(here('./Data/Cole-Original-Data/finalized_lit_db_for_r.csv'),
                   guess_max = 10000)
current = current %>% 
  filter(`Relevant to Study` == 'Y' | `Relevant to Study` == 'y') %>% 
  rename(TOS = `Type of study`,
         Relevance = `Relevant to Study`)

#gotta do it for the new data too
func_biogeo_study_data = func_biogeo_study_data %>% 
  filter(Relevance == 1) %>%
  filter(DOI %in% additive_biogeo$DOI) %>% 
  rename(TOS = `Type of study`,
         Year = PY) %>% 
  select(-c(TI, AB, AU, PT, SO, AdditionalReviewNeeded, ReviewerNotes, 
            TypeofGC, ReviewedBy))
sort(names(current)) == sort(names(func_biogeo_study_data)) #this must all be T

# join new and old data
func_biogeo_study_data =
  func_biogeo_study_data[, match(names(current), 
                                 names(func_biogeo_study_data))]
if(all(names(func_biogeo_study_data) == names(current))) {
  
  current = rbind(current, func_biogeo_study_data)
}

# Add in categorical variables =================================================

#TOS
tos = current %>% 
  select(DOI, Review, Observational, Experiment, Metanalysis,
         QModel, TModel)

TOS_data = data.frame(DOI = as.character(),
                      TOS = as.character())

for(doi in unique(tos$DOI)) {
  temp = tos %>% 
    filter(DOI == doi) 
  temp = temp[, colSums(temp != 0) > 0]
  if(ncol(temp) > 2) {
    sub = data.frame(DOI = as.character(),
                          TOS = as.character())
    for(n in unique(names(temp[,2:ncol(temp)]))) {
      temp1 = temp[,c('DOI', n)]
      temp2 = cbind(temp1[1L], TOS = names(temp1[-1L])[max.col(temp1[-1L] == 1L)])
      sub = rbind(sub, temp2)
      
    }
    TOS_data = rbind(TOS_data, sub)
  } else {
    temp = cbind(temp[1L], TOS = names(temp[-1L])[max.col(temp[-1L] == 1L)])
    TOS_data = rbind(TOS_data, temp)
  }
}
levels(TOS_data$TOS)[levels(TOS_data$TOS)=="QModel"] <- "Observational"
levels(TOS_data$TOS)[levels(TOS_data$TOS)=="TModel"] <- "Theoretical"
TOS_data = TOS_data %>% 
  unique()


#Trait Type
trait_type = current %>% 
  select(DOI, NEWPhysiological, Morphological, `Life History`,
         Behavioural) %>% 
  rename(Physiological = NEWPhysiological)

TT_data = data.frame(DOI = as.character(),
                      TT = as.character())

for(doi in unique(trait_type$DOI)) {
  temp = trait_type %>% 
    filter(DOI == doi) 
  temp = temp[, colSums(temp != 0) > 0]
  if(ncol(temp) > 2) {
    sub = data.frame(DOI = as.character(),
                     TT = as.character())
    for(n in unique(names(temp[,2:ncol(temp)]))) {
      temp1 = temp[,c('DOI', n)]
      temp2 = cbind(temp1[1L], TT = names(temp1[-1L])[max.col(temp1[-1L] == 1L)])
      sub = rbind(sub, temp2)
      
    }
    TT_data = rbind(TT_data, sub)
  } else {
    temp = cbind(temp[1L], TT = names(temp[-1L])[max.col(temp[-1L] == 1L)])
    TT_data = rbind(TT_data, temp)
  }
}


#Filter
filter = current %>% 
  select(DOI, Fundamental, Physical, Ecological,
         Trophic) %>% 
  rename(Abiotic = Fundamental, Dispersal = Physical, Biotic = Ecological)

filter_data = data.frame(DOI = as.character(),
                     filter = as.character())

for(doi in unique(filter$DOI)) {
  temp = filter %>% 
    filter(DOI == doi) 
  temp = temp[, colSums(temp != 0) > 0]
  if(ncol(temp) > 2) {
    sub = data.frame(DOI = as.character(),
                     filter = as.character())
    for(n in unique(names(temp[,2:ncol(temp)]))) {
      temp1 = temp[,c('DOI', n)]
      temp2 = cbind(temp1[1L], filter = names(temp1[-1L])[max.col(temp1[-1L] == 1L)])
      sub = rbind(sub, temp2)
      
    }
    filter_data = rbind(filter_data, sub)
  } else {
    temp = cbind(temp[1L], filter = names(temp[-1L])[max.col(temp[-1L] == 1L)])
    filter_data = rbind(filter_data, temp)
  }
}


#grab the other data
other_data = current %>% 
  select(DOI, Ecosystem, Taxonomic, `Global Change Driver`, `Forecasting/Predictive`) %>% 
  rename(GlobalChange = `Global Change Driver`,
         Forecasting = `Forecasting/Predictive`)
other_data$Ecosystem = as.factor((other_data$Ecosystem))
other_data$Taxonomic = as.factor((other_data$Taxonomic))
other_data$DOI = as.factor(other_data$DOI)

levels(other_data$Ecosystem)[levels(other_data$Ecosystem)=="Broad"] <- "Multiple"
levels(other_data$Taxonomic)[levels(other_data$Taxonomic)=="Herps"] <- "Herpetofauna"
levels(other_data$Taxonomic)[levels(other_data$Taxonomic)=="Broad"] <- "Multiple"
levels(other_data$Ecosystem)
levels(other_data$Ecosystem)[levels(other_data$Ecosystem)=="freshwater"] <- "Freshwater"
levels(other_data$Ecosystem)[levels(other_data$Ecosystem)=="terrestrial"] <- "Terrestrial"
levels(other_data$Ecosystem)[levels(other_data$Ecosystem)=="marine"] <- "Marine"

#merge all data together
other_data_TOS = merge(other_data, TOS_data, 
                        by.x = 'DOI', by.y = 'DOI')
other_data_TOS_TT = merge(other_data_TOS, TT_data, 
                          by.x = 'DOI', by.y = 'DOI')
other_data_TOS_TT_filter = merge(other_data_TOS_TT, filter_data, 
                          by.x = 'DOI', by.y = 'DOI')
categorial_data = other_data_TOS_TT_filter

### bring categorical data together with dummy data to finish the datasets

#original traits
orig_traits = merge(categorial_data, orig_traits_multivar,
                    by.x = 'DOI', by.y = 'DOI')
write_csv(orig_traits, here('./Data/Cole-Output-Data(readyforanalysis)/original_traits_dummy.csv'))
#primary traits
prim_traits = merge(categorial_data, prim_traits_multivar,
                    by.x = 'DOI', by.y = 'DOI')
write.csv(prim_traits, here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy.csv'))
#secondary (keep empty) traits
sec_empty_traits = merge(categorial_data, sec_empty_traits_multivar,
                    by.x = 'DOI', by.y = 'DOI')
write.csv(sec_empty_traits, here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_empty_dummy.csv'))
#secondary (keep empty) traits
sec_fill_traits = merge(categorial_data, sec_fill_traits_multivar,
                         by.x = 'DOI', by.y = 'DOI')
write.csv(sec_fill_traits, here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_f_dummy.csv'))

#hoping to show a summary (rank abundance curve for traits instead of species) 
#so come up with how many traits go to each primary/secondary (i.e. if a paper 
#has three 1s for three primary sections, each of those ones might be due to three
#or four or however many traits per each one)

######### Repeat data creation process for secondary and primary, but instead
######### of binary, make it abundance based (i.e. if there are 5 traits) 
######### in a single paper that are 'Life History', life history now recieves
######### 5 instead of 1 for that particular paper

#so, make the count tables
additive_prim_traits_abundance = additive_levels %>% 
  group_by(DOI, Primary_classification) %>% 
  dplyr::summarize(n = length(Primary_classification))

additive_sec_traits_abundance = additive_levels %>% 
  group_by(DOI, Secondary_classification) %>% 
  filter(!is.na(Secondary_classification)) %>% 
  dplyr::summarize(n = length(Secondary_classification))

#now turn them into the quasi-dummy data
prim_traits_abundance_multivar = additive_prim_traits_abundance %>% 
  spread(Primary_classification, n, fill = 0)

additive_sec_traits_abundance_multivar = additive_sec_traits_abundance %>% 
  spread(Secondary_classification, n, fill = 0)

#perform joins and write to files
#primary traits
prim_traits_abundance = merge(categorial_data, prim_traits_abundance_multivar,
                    by.x = 'DOI', by.y = 'DOI')
write.csv(prim_traits_abundance, 
          here('./Data/Cole-Output-Data(readyforanalysis)/primary_traits_dummy_abundance.csv'))

#secondary (keep empty) traits
sec_empty_traits_abundance = merge(categorial_data, 
                                   additive_sec_traits_abundance_multivar,
                         by.x = 'DOI', by.y = 'DOI')
write.csv(sec_empty_traits_abundance, 
          here('./Data/Cole-Output-Data(readyforanalysis)/secondary_traits_dummy_abundance.csv'))




#we could exclude cases that don't share any traits with other studies
#or we could exclude traits

