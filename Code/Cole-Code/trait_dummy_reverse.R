#Take Dummy variable dataset and reverse it so we can bring in higher-level trait categorizations
library(tidyverse)
library(data.table)
library(fastDummies)
library(here)
`%notin%` = Negate(`%in%`)

dummy_new = read_csv(here('./Data/Cole-Original-Data/traits_dummy_fixed.csv'))
traits_fixed = read_csv(here('./Data/Cole-Original-Data/traits_fixed.csv'))

all = traits_fixed$DOI
some = as.factor(dummy_new$DOI)
missing_dois = traits_fixed %>% 
  filter(DOI %notin% some) %>% 
  select(DOI) %>% 
  unique()
missing_dois = as.character(unique(missing_dois$DOI))
###### keeping this here for posterity but this problem was solved


##### here, I'm making a new df that has each trait and each DOI paired so I can pull things based on the DOI and on the traits
additive = data.frame(DOI = as.character(),
                      Trait = as.character())
traits = names(dummy_new)
for(i in 1:nrow(dummy_new)) {
  for(j in 2:ncol(dummy_new)) {
    if(dummy_new[i,j] != 0) {
      x = as.character(dummy_new[i,1])
      y = as.character(traits[j])
      new = data.frame(DOI = x, Trait = y)
      additive = rbind(additive,new)
    } 
  }
}
additive = additive %>% 
  distinct()


dummy_new$sums = rowSums(dummy_new[,c(2:ncol(dummy_new))])
check_add = data.frame(table(additive$DOI)) %>% 
  rename(DOI = Var1, Trait = Freq) %>% 
  distinct()
check_orig = data.frame(dummy_new[,c(1,ncol(dummy_new))]) %>% 
  distinct()
check_add == check_orig
check_add[500:802,] == check_orig[500:802,]

check_orig[duplicated(check_orig$DOI),]
###### ok these above checks ran fine so the number of traits associated with each DOI look fine
###### I'm interpreting this as the additive df is doing what I'm asking it to do and we're good on that front


#grab traits_fixed df with the DOIs that I'm missing
missing_doi_traits = traits_fixed %>% 
  filter(DOI %in% missing_dois) %>% 
  select(DOI, Traits) %>%  #note, all the traits are actually in the trait_levels database that we went through, so I'll just join here
  rename(Trait = Traits)

missing_doi_traits$DOI = as.factor(missing_doi_traits$DOI)

names(missing_doi_traits)
additive = rbind(additive, missing_doi_traits)
n_distinct(additive$DOI) #okay, we have all 822 studies now

additive$Trait = trimws(additive$Trait, which = 'both')

#join all the other trait levels to the additive database 
trait_levels = read_csv(here('./Data/Cole-Original-Data/trait_levels.csv'))

#now, there are likely duplicated traits in the trait_levels dataframe, I have to get rid of them so I can make proper pairs to them 
dups = trait_levels[duplicated(trait_levels$Trait_spell_corrected),]
dup_df = trait_levels %>% 
  filter(Trait_spell_corrected %in% dups$Trait_spell_corrected)
#after checking visually, all duplicates are true duplicates (i.e. no traits that were the same were coded differently)
#so I can get rid of them now with no problems

trait_levels = trait_levels[!duplicated(trait_levels$Trait_spell_corrected),]

trait_levels_sub = trait_levels %>% 
  select(Trait_spell_corrected, Primary_classification, Secondary_classification) %>% 
  rename(Trait = Trait_spell_corrected)

#do a check to make sure that there are no traits coming in from the dummy set that aren't in the levels set and vice versa
traits_levels_unique = sort(unique(as.character(trait_levels_sub$Trait)))
traits_additive_unique = sort(unique(as.character(additive$Trait)))
missing_traits = data.frame(setdiff(traits_additive_unique, traits_levels_unique)) #as long as this is empty
missing_traits1 = data.frame(setdiff(traits_levels_unique, traits_additive_unique)) #and this is empty - all the traits that should be there are accounted for

#### Begin the merge

#first get the trait classifications to the DOIs by the traits
additive_levels = merge(additive, trait_levels_sub, 
                        by.x = 'Trait', by.y = 'Trait')


#make the dummy datasets for the different levels here
additive_orig_traits = additive_levels %>% 
  select(DOI, Trait)
additive_prim_traits = additive_levels %>% 
  select(DOI, Primary_classification) %>% 
  distinct()
#note two options here - fill secondary with primary or cut it out, I'll do both
#option1
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
  distinct()

#now perform reshaping to get the dummies 
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
  select(DOI, Ecosystem, Taxonomic, System, `Forecasting/Predictive`, `Global Change Driver`,
         TOS, Filter)
n_distinct(old_dummy$DOI) #note, only 802 here, gotta get the other 20 from the other df

old = old_dummy %>% 
  select(DOI, Filter)
test = old[duplicated(old),]

current = read_csv(here('./Data/Cole-Original-Data/finalized_lit_db_for_r.csv'))
current = current %>% 
  filter(`Relevant to Study` == 'Y' | `Relevant to Study` == 'y') %>% 
  rename(TOS = `Type of study`)

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
write.csv(orig_traits, here('./Data/Cole-Output-Data(readyforanalysis)/original_traits_dummy.csv'))
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
