########## 
##########
# This code contains some figures
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-01-28
##########
##########

# set-up =======================================================================

library(tidyverse)
library(wordcloud)
library(PNWColors)
library(cowplot)
library(here)
library(fastDummies)
library(ggwordcloud)
library(viridis)

categorical_data = read_csv(here('./data/processed-data/categorical_data.csv'))
trait_levels = read_csv(here(paste0('./data/processed-data/',
                                    'trait_levels_orig_prim_sec.csv')))
categorical_data_nondum = 
  read_csv(here(paste0('./data/processed-data',
                       '/categorical_data_nondummy.csv')))

# initial data processing  =====================================================

factor_cols = c("Ecosystem", "Taxonomic", "GlobalChange", "Forecasting", 
                "TOS", "TT", "filter")
categorical_data[factor_cols] = lapply(categorical_data[factor_cols], factor)

categorical_data = categorical_data %>% 
  rename(Filter = filter, 
         Trait = TT)
categorical_data$Ecosystem =
  as.factor(categorical_data$Ecosystem)
categorical_data$Taxonomic =
  as.factor(categorical_data$Taxonomic)
categorical_data$TOS =
  as.factor(categorical_data$TOS)
categorical_data$Filter =
  as.factor(categorical_data$Filter)
categorical_data$GlobalChange =
  as.factor(categorical_data$GlobalChange)

levels(categorical_data$TOS)[levels(categorical_data$TOS)==
                                  'TModel']='Theory'
levels(categorical_data$TOS)[levels(categorical_data$TOS)==
                                  'QModel']='Observational'
levels(categorical_data$Filter)[levels(categorical_data$Filter)==
                                     "Fundamental"] = "Abiotic"
levels(categorical_data$Filter)[levels(categorical_data$Filter)==
                                     "Physical"] = "Dispersal"
levels(categorical_data$Filter)[levels(categorical_data$Filter)==
                                     "Ecological"] = "Biotic"
# Make themes for the heat plots ===============================================

theme1 = function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,
                                          color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, 
                                    size = 20, vjust = 1.25, 
                                    margin = margin(t = 0, 
                                                    r = 0, 
                                                    b = 20, 
                                                    l = 0))) +
    theme(axis.text.x = element_text(size = 15, 
                                     color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 15, 
                                     color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 18, 
                                      color = color.axis.title, 
                                      vjust = 0,
                                      margin = margin(t = 17, 
                                                      r = 0, 
                                                      b = 17, 
                                                      l = 0))) +
    theme(axis.title.y = element_text(size = 18, 
                                      color = color.axis.title, 
                                      vjust = 1.5, 
                                      margin = margin(t = 0,
                                                      r = 17, 
                                                      b = 0, 
                                                      l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) +
    theme(strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 16))+
    theme(legend.position = 'right') +
    theme(plot.title = element_text(hjust = 0.5))
}
theme2 = function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill='#FCFDBFFF',
                                          color = '#FCFDBFFF')) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'grey20')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, 
                                    size = 20, vjust = 1.25, 
                                    margin = margin(t = 0, 
                                                    r = 0, 
                                                    b = 20, 
                                                    l = 0))) +
    theme(axis.text.x = element_text(size = 18, 
                                     color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 18, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 18, 
                                      color = color.axis.title, 
                                      vjust = 0, 
                                      margin = margin(t = 17, 
                                                      r = 0, 
                                                      b = 17, 
                                                      l = 0))) +
    theme(axis.title.y = element_text(size = 18, 
                                      color = color.axis.title, 
                                      vjust = 1.5, 
                                      margin = margin(t = 0, 
                                                      r = 17, 
                                                      b = 0, 
                                                      l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="grey20", size = 0.15),
          axis.line.y = element_line(color="grey20", size = 0.15)) +
    theme(strip.background = element_rect(fill = c('#FCFDBFFF'), 
                                          colour = 'grey20'),
          strip.placement = 'outside',
          strip.text = element_text(size = 16))+
    theme(legend.position = 'right',
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5))
}
theme3 = function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,
                                          color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'grey80')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, 
                                    size = 20, 
                                    vjust = 1.25,
                                    margin = margin(t = 0, 
                                                    r = 0, 
                                                    b = 20, 
                                                    l = 0))) +
    theme(axis.text.x = element_text(size = 15, 
                                     color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 15, 
                                     color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 18, 
                                      color = color.axis.title, 
                                      vjust = 0, 
                                      margin = margin(t = 17, 
                                                      r = 0, 
                                                      b = 17, 
                                                      l = 0))) +
    theme(axis.title.y = element_text(size = 18, 
                                      color = color.axis.title, 
                                      vjust = 1.5, 
                                      margin = margin(t = 0, 
                                                      r = 17, 
                                                      b = 0, 
                                                      l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="grey80", size = 0.15),
          axis.line.y = element_line(color="grey80", size = 0.15)) +
    theme(strip.background = element_rect(fill = 'white', colour = 'grey70'),
          strip.placement = 'outside',
          strip.text = element_text(size = 16))+
    theme(legend.position = 'right') +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.text = element_text(size = 10))
}

# Heat plot for trait x TOS x filter x ecosystem ===============================

# make the dataframe
trait_env_df3 = categorical_data %>% 
  group_by(Ecosystem, Trait, TOS, Filter, .drop = FALSE) %>% 
  summarize(Total = n()) 
trait_env_df3[which(trait_env_df3$Total == 0), "Total"] = NA

# make the actual plot itself 
trait_x_filter_by_ecosystem_x_TOS_plot = 
  ggplot(data = trait_env_df3) +
  geom_tile(aes(x = TOS, 
                y = Trait, 
                fill = Total)) +
  facet_grid(Filter ~ Ecosystem, 
             switch = "both",
             scales = "free_x", 
             space = "free_x") +
  labs(x = 'Study type by ecosystem',
       y = 'Trait type by level of environmental filtering')+
  scale_fill_viridis(option = 'A', 
                     direction = -1,
                     begin = 0.01, 
                     end = 0.99, 
                     na.value = "#FFFFE0") +
  theme2() 

ggsave(here(paste0('./figures/heatplots-and-timeseries',
                   '/trait_x_filter_by_ecosystem_x_TOS_small.png')), 
       plot = trait_x_filter_by_ecosystem_x_TOS_plot, 
       width = 15, height = 9, dpi = 200)
ggsave(here(paste0('./figures/heatplots-and-timeseries',
                   '/trait_x_filter_by_ecosystem_x_TOS_large.png')), 
       plot = trait_x_filter_by_ecosystem_x_TOS_plot, 
       width = 15, height = 9, dpi = 600)

# Heat plot for trait x taxonomy x filter x ecosystem ==========================

# make dataframe
trait_env_df4 = categorical_data %>% 
  group_by(Ecosystem, Trait, Taxonomic, Filter, .drop = FALSE) %>% 
  summarize(Total = n()) 
trait_env_df4[which(trait_env_df4$Total == 0), "Total"] = NA


# make heat plot
trait_x_filter_by_ecosystem_x_taxonomic_plot = 
  ggplot(data = trait_env_df4, 
         aes(x = Taxonomic, 
             y = Trait, 
             fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, 
             switch = "both", 
             scales = "free_x",
             space = "free_x") +
  labs(x = 'Taxonomic group by ecosystem', 
       y = 'Trait type by level of environmental filtering') +
  scale_fill_viridis(option = 'A', 
                     direction = -1,
                     begin = 0, 
                     end = 0.99, 
                     na.value = "#FFFFE0") +
  theme2() 

ggsave(here(paste0('./figures/heatplots-and-timeseries',
                   '/trait_x_filter_by_ecosystem_x_taxonomic_small.png')), 
       plot = trait_x_filter_by_ecosystem_x_taxonomic_plot, 
       width = 15, height = 9, dpi = 200)
ggsave(here(paste0('./figures/heatplots-and-timeseries',
                   '/trait_x_filter_by_ecosystem_x_taxonomic_large.png')), 
       plot = trait_x_filter_by_ecosystem_x_taxonomic_plot, 
       width = 15, height = 9, dpi = 600)

# Data and functions for wordcloud creation ====================================

trait_levels_cat = merge(trait_levels, categorical_data_nondum, 
                       by.x = 'DOI', by.y = 'DOI')


# write a function to make the proces easier
make_dataframe = function(column, level, n) {
  
  # function takes in the column name and the level of the column to be subset
  # to, then returns a dataframe ready for a wordcloud to be made
  
  df = trait_levels_cat %>% 
    select(!!column, Trait) %>% 
    filter(!!column == level) %>% 
    group_by(Trait) %>% 
    summarize(frequency = n()) %>% 
    filter(frequency > n)
  
  return(df)
    
}

# make environmental filter dataframes
filter_abiotic = make_dataframe(quo(Fundamental), 1, 5)
filter_dispersal = make_dataframe(quo(Physical), 1, 1)
filter_biotic = make_dataframe(quo(Ecological), 1, 3)
filter_trophic = make_dataframe(quo(Trophic), 1, 2)

# trait type filter dataframes
trait_morph = make_dataframe(quo(Morphological), 1, 5)
trait_behav = make_dataframe(quo(Behavioural), 1, 3)
trait_physio = make_dataframe(quo(NEWPhysiological), 1, 2)
trait_life = make_dataframe(quo(`Life History`), 1, 5)

# ecosystem type dataframes
trait_levels_cat$Ecosystem = as.factor(trait_levels_cat$Ecosystem)
levels(trait_levels_cat$Ecosystem)[
  levels(trait_levels_cat$Ecosystem)=="Broad"] =
  "Multiple"
levels(trait_levels_cat$Ecosystem)[
  levels(trait_levels_cat$Ecosystem)=="freshwater"] = 
  "Freshwater"
levels(trait_levels_cat$Ecosystem)[
  levels(trait_levels_cat$Ecosystem)=="terrestrial"] = 
  "Terrestrial"
levels(trait_levels_cat$Ecosystem)[
  levels(trait_levels_cat$Ecosystem)=="marine"] = 
  "Marine"
eco_terrest = make_dataframe(quo(Ecosystem), 'Terrestrial', 5)
eco_fresh = make_dataframe(quo(Ecosystem), 'Freshwater', 2)
eco_marine = make_dataframe(quo(Ecosystem), 'Marine', 1)
eco_multiple = make_dataframe(quo(Ecosystem), 'Multiple', 1)

make_wordclouds = function(df, name) {
  
  # function takes in a dataframe from the ones created above and creates a 
  # wordcloud from that dataframe. The wordcloud is then given a name and 
  # returned as that name
  cloud = ggwordcloud(df$Trait, 
                      df$frequency, 
                      scale=c(3,.55), 
                      random.order = FALSE, 
                      min.freq = 2, 
                      max.words = Inf, 
                      rot.per = 0.15, 
                      colors=pnw_palette('Bay', 8, 'continuous'))

  return(cloud)

}

# Employ functions to create wordclouds ========================================

eco_terrest_cloud = make_wordclouds(df = eco_terrest, name = 'eco_terrest')
eco_marine_cloud = make_wordclouds(df = eco_marine, name = 'eco_marine')
eco_fresh_cloud = make_wordclouds(df = eco_fresh, name = 'eco_fresh')
eco_multiple_cloud = make_wordclouds(df = eco_multiple, name = 'eco_multiple')

# traits
trait_morph_cloud = make_wordclouds(df = trait_morph, name = 'trait_morph')
trait_life_cloud = make_wordclouds(df = trait_life, name = 'trait_life')
trait_behav_cloud = make_wordclouds(df = trait_behav, name = 'trait_behav')
trait_physio_cloud = make_wordclouds(df = trait_physio, name = 'trait_physio')


filter_abiotic_cloud = make_wordclouds(df = filter_abiotic, 
                                       name = 'filter_abiotic')
filter_biotic_cloud = make_wordclouds(df = filter_biotic, 
                                      name = 'filter_biotic')
filter_dispersal_cloud = make_wordclouds(df = filter_dispersal, 
                                         name = 'filter_dispersal')
filter_trophic_cloud = make_wordclouds(df = filter_trophic, 
                                       name = 'filter_trophic')

# use `cowplot` to stitch the plots together and put custom labels on them, 
# then use it again to put titles 
filter_cloud =  
  plot_grid(filter_abiotic_cloud, filter_dispersal_cloud, filter_biotic_cloud, 
            filter_trophic_cloud, ncol = 4, 
            labels = c('Abiotic', 'Dispersal', 'Biotic', 'Trophic'), 
            vjust = 0.8, 
            label_size = 20) 
trait_cloud =  
  plot_grid(trait_morph_cloud, trait_behav_cloud, trait_physio_cloud, 
            trait_life_cloud, ncol = 4, 
            labels = c('Morphology', 'Behaviour', 'Physiology', 'Life History'), 
            vjust = 0.8, 
            label_size = 20)
ecosystem_cloud =  
  plot_grid(eco_terrest_cloud, eco_marine_cloud, eco_fresh_cloud, 
            eco_multiple_cloud, ncol = 4, 
            labels = c('Terrestrial', 'Marine', 'Freshwater', 'Multiple'), 
            vjust = 0.8, 
            label_size = 20)

all_cloud = plot_grid(NULL, filter_cloud, NULL, 
                      trait_cloud, NULL, ecosystem_cloud,
                      nrow = 6,
                      labels = c('Environmental Filter', '', 
                                 'Trait Type', '',
                                 'Ecosystem', ''),
                      label_size = 30,
                      label_x = c(0.20, 0.33, 0.33),
                      rel_heights = c(0.3, 1, 0.3, 1, 0.3, 1))

ggsave(here('./figures/word-clouds/cloud_grid_small.png'),
       plot = all_cloud,
       width = 15, height = 23, dpi = 200)
ggsave(here('./figures/word-clouds/cloud_grid_large.png'),
       plot = all_cloud,
       width = 15, height = 23, dpi = 600)

# Prepare data and themes for the timeseries ===================================

categorical_data = merge(categorical_data, categorical_data_nondum %>% 
                           select(DOI, Year), 
                         by.x = 'DOI', by.y = 'DOI')

levels(categorical_data$GlobalChange)[levels(categorical_data$GlobalChange)==
                                         "Global Change Broad"] = "Multiple"
levels(categorical_data$GlobalChange)[levels(categorical_data$GlobalChange)==
                                         "Global Change Multiple"] = "Multiple"

theme4 = function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,
                                          color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, 
                                    size = 26, 
                                    vjust = 1.25, 
                                    margin = margin(t = 0, 
                                                    r = 0, 
                                                    b = 20, 
                                                    l = 0))) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_text(size = 20, color = color.axis.text)) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 26, color = color.axis.title, 
                                      vjust = 1.5, 
                                      margin = margin(t = 0, 
                                                      r = 17, 
                                                      b = 0, 
                                                      l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(strip.background = element_rect(fill = 'white', colour = 'grey70'),
          strip.placement = 'outside',
          strip.text = element_text(size = 24))+
    theme(legend.position = c(0.4,0.6)) +
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          legend.key.width= unit(4.5, 'line'),
          legend.key.height = unit(0.7,'line'))
}
theme5 = function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,
                                          color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 26, 
                                    vjust = 1.25, 
                                    margin = margin(t = 0, 
                                                    r = 0, 
                                                    b = 20, 
                                                    l = 0))) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_text(size = 20, color = 'black')) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(strip.background = element_rect(fill = 'white', colour = 'grey70'),
          strip.placement = 'outside',
          strip.text = element_text(size = 24))+
    theme(legend.position = c(0.4,0.6)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          legend.key.width= unit(4.5, 'line'),
          legend.key.height = unit(0.7,'line'))
}
theme6 = function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,
                                          color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 26, 
                                    vjust = 1.25, 
                                    margin = margin(t = 0, 
                                                    r = 0, 
                                                    b = 20, 
                                                    l = 0))) +
    theme(axis.text.x = element_text(size = 20, 
                                     color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 20, color = 'black')) + 
    theme(axis.title.x = element_text(size = 26, 
                                      color = color.axis.title, 
                                      vjust = 0, 
                                      margin = margin(t = 17, 
                                                      r = 0, 
                                                      b = 17, 
                                                      l = 0))) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(strip.background = element_rect(fill = 'white', colour = 'grey70'),
          strip.placement = 'outside',
          strip.text = element_text(size = 24))+
    theme(legend.position = c(0.4,0.6)) +
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          legend.key.width= unit(4.5, 'line'),
          legend.key.height = unit(0.7,'line'))
}
theme7 = function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,
                                          color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 26, 
                                    vjust = 1.25,
                                    margin = margin(t = 0, 
                                                    r = 0, 
                                                    b = 20, 
                                                    l = 0))) +
    theme(axis.text.x = element_text(size = 20, 
                                     color = color.axis.text, 
                                     angle = 90)) + 
    theme(axis.text.y = element_text(size = 20, color = 'black')) + 
    theme(axis.title.x = element_text(size = 26, color = color.axis.title, 
                                      vjust = 0, 
                                      margin = margin(t = 17, 
                                                      r = 0, 
                                                      b = 17, 
                                                      l = 0))) +
    theme(axis.title.y = element_text(size = 26, color = 'black', 
                                      vjust = 1.5, 
                                      margin = margin(t = 0, 
                                                      r = 17, 
                                                      b = 0, 
                                                      l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(strip.background = element_rect(fill = 'white', colour = 'grey70'),
          strip.placement = 'outside',
          strip.text = element_text(size = 20))+
    theme(legend.position = c(0.4,0.6)) +
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          legend.key.width= unit(4.5, 'line'),
          legend.key.height = unit(0.7,'line'))
}

# total studies over the years 

for_hist = categorical_data_nondum %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year) %>% 
  summarize(no_obs = length(Year))
for_hist$n = for_hist$no_obs

pal = pnw_palette('Bay',100)
total_studies_colour = 'midnightblue'
for_hist$group = 'Published Papers'
total_studies = ggplot(data = for_hist, aes(x = Year, y = no_obs, 
                                            group = group, 
                                            colour = group, 
                                            shape = group),) +
  labs(y = 'Number of Studies')+
  theme4()+ #begins in 1978
  geom_line(size = 1.08) +
  geom_point(size = 3)+
  scale_color_manual('Total Published Studies', values = total_studies_colour) +
  scale_linetype_manual('Total Published Studies', values=c(1)) +
  scale_shape_manual('Total Published Studies', values = c(19))

# ecosystem 
pal = pnw_palette('Bay', 4, type = 'discrete')
for_eco_hist = categorical_data %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  select(DOI, Year, Ecosystem) %>% 
  distinct() %>% 
  group_by(Year, Ecosystem) %>% 
  summarize(no_obs = length(Year))
for_eco_hist$n = for_eco_hist$no_obs

ecosystem_studies = ggplot(data = for_eco_hist, aes(x = Year, y = no_obs, 
                                                    group = Ecosystem, 
                                                    colour = Ecosystem,
                                                    linetype = Ecosystem), ) +
  geom_line(size = 1.08) +
  theme5()+ #begins in 1978
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_manual('Ecosystem', values = pal) +
  scale_linetype_manual('Ecosystem', values=c(1,5,6,4)) +
  scale_shape_manual('Ecosystem', values = c(19,19,19,19))+
  labs(x = 'Year', y = 'Number of Studies')

# trait type
all_traits_line = categorical_data %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  select(DOI, Year, Trait) %>% 
  distinct() %>%
  group_by(Year, Trait) %>% 
  summarize(no_obs = n())

trait_studies = ggplot(data = all_traits_line, aes(x = Year, y = no_obs, 
                                                   group = Trait, 
                                                   colour = Trait, 
                                                   shape = Trait, 
                                                   linetype = Trait), ) +
  geom_line(size = 1.08) +
  geom_point(size = 3)+
  scale_color_manual('Trait Type', values = pal) +
  scale_linetype_manual('Trait Type', values=c(1,5,6,4)) +
  scale_shape_manual('Trait Type', values = c(19,19,19,19))+
  theme7()+ #begins in 1978
  labs(x = 'Year', y = 'Number of Studies')+
  scale_x_discrete(breaks = c(1978, 1983, 1988, 1993,
                                1998, 2003, 2008, 2013, 2018))


# level of environmental filtering
all_filters_line = categorical_data %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  select(DOI, Year, Filter) %>% 
  distinct() %>%
  group_by(Year, Filter) %>% 
  summarize(no_obs = n())

filter_studies = ggplot(data = all_filters_line, aes(x = as.numeric(Year), y = no_obs, 
                                                     group = Filter, 
                                                     colour = Filter, 
                                                     shape = Filter, 
                                                     linetype = Filter)) +
  geom_line(size = 1.08) +
  geom_point(size = 3)+
  theme6()+ #begins in 1978
  scale_linetype_manual('Environmental Filter', values=c(1,5,6,4)) +
  scale_shape_manual('Environmental Filter', values = c(19,19,19,19,19,19))+
  scale_color_manual('Environmental Filter', values = pal) +
  labs(x = 'Year', y = 'Number of Studies') +
  scale_y_continuous(limits = c(0,100), breaks = c(0,30,60,90))+
  scale_x_continuous(limits = c(1978, 2019), 
                     breaks = c(1978, 1983, 1988, 1993,
                                1998, 2003, 2008, 2013, 2018))

# taxonomic 
for_tax_hist = categorical_data %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  select(DOI, Year, Taxonomic) %>% 
  distinct() %>%
  group_by(Year, Taxonomic) %>%   
  summarize(no_obs = length(Year))

pal = pnw_palette('Bay',10, type = 'continuous')
tax_studies = ggplot(data = for_tax_hist, aes(x = Year, y = no_obs, 
                                        colour = Taxonomic, 
                                        linetype = Taxonomic, 
                                        group = Taxonomic,
                                        shape = Taxonomic),) +
  geom_point(size = 3)+
  geom_line(size = 1.08) +
  theme5()+ #begins in 1978
  scale_color_manual('Taxonomic Focus', values = pal) +
  scale_linetype_manual('Taxonomic Focus', 
                        values=c(1,5,6,4,2,3,1,5,6)) +
  scale_shape_manual('Taxonomic Focus', 
                     values = c(19,19,19,19,19,19,19,19,19)) +
  labs(x = 'Year', y = 'Number of Studies')

# global change 
global_driver = categorical_data %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  select(DOI, Year, GlobalChange) %>% 
  distinct() %>%
  group_by(Year, GlobalChange) %>% 
  filter(GlobalChange != 0) %>% 
  summarize(no_obs = length(Year)) 

pal = pnw_palette('Bay', 6, type = 'continuous')
global_driver$Year = as.numeric(global_driver$Year)
global_plot = ggplot(data = global_driver, 
                     aes(x = Year, y = no_obs, 
                       group = GlobalChange, 
                       colour = GlobalChange, 
                       linetype = GlobalChange,
                       shape = GlobalChange)) +
  geom_line(size = 1.08) +
  geom_point(size = 3)+
  scale_color_manual('Global Change Driver', values = pal) +
  scale_linetype_manual('Global Change Driver', values=c(1,5,6,4,2,3)) +
  scale_shape_manual('Global Change Driver', values = c(19,19,19,19,19,19)) +
  labs(x = 'Year', y = 'Number of Studies') +
  scale_y_continuous(limits = c(0,15)) +
  scale_x_continuous(limits = c(1978, 2019), 
                     breaks = c(1978, 1983, 1988, 1993,
                                1998, 2003, 2008, 2013, 2018)) +
  theme6() #begins in 1992

# bind them all together 
alltypes = plot_grid(total_studies, ecosystem_studies, 
                     tax_studies, trait_studies, filter_studies, global_plot,
                           nrow = 2, ncol = 3, 
                     rel_widths = c(0.91,0.76,0.76, 0.91,0.77,0.77), 
                     rel_heights = c(0.75,1), 
                     labels = c('A', 'B','C','D','E','F'), 
                     label_x = c(0.185, 0.07, 0.07, 0.185, 0.07,0.07), vjust = 2.5)

ggsave(here('./figures/heatplots-and-timeseries/timeseries_small.png'), 
       plot = alltypes, 
       width = 20, height = 12, dpi = 200)
ggsave(here('./figures/heatplots-and-timeseries/timeseries_large.png'), 
       plot = alltypes, 
       width = 20, height = 12, dpi = 600)

