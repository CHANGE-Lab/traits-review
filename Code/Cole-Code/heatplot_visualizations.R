########## 
##########
# This code contains some figures
# in Green et al. (2020) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-01-28
##########
##########

library(tidyverse)
library(RColorBrewer)
library(viridis)
library(scales)
library(tm)
library(wordcloud)
library(PNWColors)
library(extrafont)
library(cowplot)
library(here)
library(gridExtra)

current = read_csv('./Data/Cole-Original-Data/finalized_lit_db_for_r.csv')

unique(current$`Relevant to Study`)

current = current %>% 
  filter(`Relevant to Study` %in% c('Y', 'y'))


############################## Trait by Ecosystem

#data manip 
trait_env_df1 = current %>% 
  select(Ecosystem, Morphological, 
         NEWPhysiological, Behavioural, `Life History`)

trait_env_df1$Ecosystem = as.factor(trait_env_df1$Ecosystem)

trait_env_df1 = trait_env_df1 %>% 
  rename(Physiological = NEWPhysiological)

trait_env_df1 = trait_env_df1 %>% 
  gather(Trait, Count, Morphological,
         Physiological, Behavioural, `Life History`) %>% 
  group_by(Ecosystem, Trait)

trait_env_df1 = trait_env_df1 %>% 
  group_by(Ecosystem, Trait) %>% 
  summarize(Total = sum(Count))

#make a theme
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

########################## Trait x Env Filtering by Ecosystem x TOS
#data manip 
trait_env_df3 = current %>% 
  select(Ecosystem, Morphological, 
         NEWPhysiological, Behavioural, `Life History`, 
         QModel, Review, Observational, 
         TModel, Experiment, Metanalysis, `Fundamental`, `Physical`, 
         `Ecological`, `Trophic`)

trait_env_df3 = trait_env_df3 %>% 
  rename(Physiological = NEWPhysiological)

trait_env_df3$Ecosystem = as.factor(trait_env_df3$Ecosystem)

trait_env_df3 = trait_env_df3 %>% 
  group_by(Ecosystem) %>% 
  gather(TOS, CountTOS, QModel, Review, 
         Observational, TModel, Experiment, Metanalysis) %>% 
  filter(CountTOS == 1)


trait_env_df3$TOS = as.factor(trait_env_df3$TOS)

trait_env_df3 = trait_env_df3 %>% 
  group_by(Ecosystem) %>% 
  gather(Filter, CountFIL, `Fundamental`, `Physical`, 
         `Ecological`, `Trophic`) %>% 
  filter(CountFIL == 1)

levels(trait_env_df3$TOS)[levels(trait_env_df3$TOS)=="QModel"] = 
  "Observational"
levels(trait_env_df3$TOS)[levels(trait_env_df3$TOS)=="TModel"] = 
  "Theoretical"
levels(trait_env_df3$Filter)[levels(trait_env_df3$Filter)=="Fundamental"] = 
  "Abiotic"
levels(trait_env_df3$Filter)[levels(trait_env_df3$Filter)=="Physical"] = 
  "Dispersal"
levels(trait_env_df3$Filter)[levels(trait_env_df3$Filter)=="Ecological"] = 
  "Biotic"
levels(trait_env_df3$Ecosystem)[levels(trait_env_df3$Ecosystem)=="Broad"] = 
  "Multiple"
levels(trait_env_df3$Ecosystem)[levels(trait_env_df3$Ecosystem)=="Broad"] = 
  "Multiple"
levels(trait_env_df3$Ecosystem)[levels(trait_env_df3$Ecosystem)=="Broad"] = 
  "Multiple"

trait_env_df3$Filter = as.factor(trait_env_df3$Filter)

trait_env_df3 = trait_env_df3 %>% 
  group_by(TOS) %>% 
  gather(Trait, CountT, Morphological, Physiological,
         Behavioural, `Life History`) %>% 
  filter(CountT == 1)

trait_env_df3 = trait_env_df3 %>% 
  group_by(Ecosystem, Trait, TOS, Filter) %>% 
  summarize(Total = sum(CountT))

trait_env_df3$Trait = factor(trait_env_df3$Trait, 
                             levels = c('Life History', 
                                        'Behavioural', 
                                        'Morphological', 
                                        'Physiological'))

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
                     end = 0.99) +
  theme2() 

ggsave(here('./Figures/Heatplots-Timeseries/trait_x_filter_by_ecosystem_x_TOS_small.png'), 
       plot = trait_x_filter_by_ecosystem_x_TOS_plot, 
       width = 15, height = 9, dpi = 200)
ggsave(here('./Figures/Heatplots-Timeseries/trait_x_filter_by_ecosystem_x_TOS_large.png'), 
       plot = trait_x_filter_by_ecosystem_x_TOS_plot, 
       width = 15, height = 9, dpi = 1200)

########################## Trait x Env Filtering by Ecosystem x Taxonomic
#data manip 
trait_env_df4 = current %>% 
  select(Ecosystem, Morphological, NEWPhysiological, 
         Behavioural, `Life History`, 
         Taxonomic, `Fundamental`, `Physical`, 
         `Ecological`, `Trophic`)

unique(trait_env_df4$Taxonomic)

trait_env_df4 = trait_env_df4 %>% 
  rename(Physiological = NEWPhysiological)

trait_env_df4$Ecosystem = as.factor(trait_env_df4$Ecosystem)

trait_env_df4$Taxonomic = as.factor(trait_env_df4$Taxonomic)

trait_env_df4 = trait_env_df4 %>% 
  group_by(Ecosystem) %>% 
  gather(Filter, CountFIL, `Fundamental`, `Physical`, 
         `Ecological`, `Trophic`) %>% 
  filter(CountFIL == 1)

trait_env_df4$Filter = as.factor(trait_env_df4$Filter)

trait_env_df4 = trait_env_df4 %>% 
  group_by(Taxonomic) %>% 
  gather(Trait, CountT, Morphological, Physiological, 
         Behavioural, `Life History`) %>% 
  filter(CountT == 1)

trait_env_df4 = trait_env_df4 %>% 
  group_by(Ecosystem, Trait, Taxonomic, Filter) %>% 
  summarize(Total = sum(CountT))

levels(trait_env_df4$Taxonomic)[levels(trait_env_df4$Taxonomic)=="Herps"] = 
  "Herpetofauna"
levels(trait_env_df4$Taxonomic)[levels(trait_env_df4$Taxonomic)=="Broad"] = 
  "Multiple"

trait_env_df4$Filter = factor(trait_env_df4$Filter, 
                              levels = c('Fundamental', 'Physical', 
                                         'Ecological', 'Trophic'))
trait_env_df4$Trait = factor(trait_env_df4$Trait, 
                             levels = c('Life History', 'Behavioural', 
                                        'Morphological', 'Physiological'))
trait_env_df4$Taxonomic = factor(trait_env_df4$Taxonomic, 
                                 levels = c('Plants', 'Plankton', 'Insects', 
                                            'Herpetofauna', 'Birds', 'Fish', 
                                            'Mammals', 'Multiple', 'Other'))

levels(trait_env_df4$Filter)[levels(trait_env_df4$Filter)=="Fundamental"] = 
  "Abiotic"
levels(trait_env_df4$Filter)[levels(trait_env_df4$Filter)=="Physical"] = 
  "Dispersal"
levels(trait_env_df4$Filter)[levels(trait_env_df4$Filter)=="Ecological"] = 
  "Biotic"
levels(trait_env_df4$Ecosystem)[levels(trait_env_df4$Ecosystem)=="Broad"] = 
  "Multiple"

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
                     begin = 0.01, 
                     end = 0.99) +
  theme2() 

ggsave(here('./Figures/Heatplots-Timeseries/trait_x_filter_by_ecosystem_x_taxonomic_small.png'), 
       plot = trait_x_filter_by_ecosystem_x_taxonomic_plot, 
       width = 15, height = 9, dpi = 200)
ggsave(here('./Figures/Heatplots-Timeseries/trait_x_filter_by_ecosystem_x_taxonomic_large.png'), 
       plot = trait_x_filter_by_ecosystem_x_taxonomic_plot, 
       width = 15, height = 9, dpi = 1200)

################################## Looking at what traits are actually used
#wordclouds

#load data already in phrase-frequency format
fish = read_csv('FishForR.csv')
marine = read_csv('MarineForR.csv')
trophic = read_csv('TrophicForR.csv')
trophicsub = read_csv('Trophic(Marine-Freshwater)ForR.csv')

#make a wordcloud

#fish - min. 2
wordcloud(fish$Trait, fish$`Number of Times Used`, scale=c(3,.55), 
          random.order = FALSE, min.freq = 2, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#marine - min. 2
wordcloud(marine$Traits, marine$Frequency, scale=c(3,.55), 
          random.order = FALSE, min.freq = 2, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophic - min. 2
wordcloud(trophic$Trait, trophic$`Number of Times Used`, scale=c(3,.55), 
          random.order = FALSE, min.freq = 2, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophicsub - min. 2
wordcloud(trophicsub$Trait, trophicsub$`Number of Times Used`, scale=c(3,.55), 
          random.order = FALSE, min.freq = 2, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))

#fish - min. 1
wordcloud(fish$Trait, fish$`Number of Times Used`, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#marine - min. 1
wordcloud(marine$Traits, marine$Frequency, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophic - min. 2
wordcloud(trophic$Trait, trophic$`Number of Times Used`, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophicsub - min. 1
wordcloud(trophicsub$Trait, trophicsub$`Number of Times Used`, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, 
          rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))

######## Studies over time

for_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year) %>% 
  summarize(no_obs = length(Year))
for_hist$n = for_hist$no_obs

#Current change global change driver
levels(current$`Global Change Driver`)[levels(current$`Global Change Driver`)==
                                         "Global Change Broad"] = "Multiple"
levels(current$`Global Change Driver`)[levels(current$`Global Change Driver`)==
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
#total studies
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

#by ecosystem
pal = pnw_palette('Bay', 4, type = 'discrete')
for_eco_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
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

#by trait type
for_phys_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, NEWPhysiological) %>%   
  filter(NEWPhysiological == 1)%>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(trait = 'Physiological') %>% 
  select(Year, no_obs, trait)
for_morph_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Morphological) %>% 
  filter(Morphological == 1)%>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(trait = 'Morphological') %>% 
  select(Year, no_obs, trait)
for_life_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, `Life History`) %>% 
  filter(`Life History` == 1) %>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(trait = 'Life History') %>% 
  select(Year, no_obs, trait)
for_behav_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Behavioural) %>% 
  filter(Behavioural == 1) %>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(trait = 'Behavioural') %>% 
  select(Year, no_obs, trait)

all_traits_line = rbind(for_behav_hist, for_morph_hist, 
                        for_life_hist, for_phys_hist)

trait_studies = ggplot(data = all_traits_line, aes(x = Year, y = no_obs, 
                                                   group = trait, 
                                                   colour = trait, 
                                                   shape = trait, 
                                                   linetype = trait), ) +
  geom_line(size = 1.08) +
  geom_point(size = 3)+
  scale_color_manual('Trait Type', values = pal) +
  scale_linetype_manual('Trait Type', values=c(1,5,6,4)) +
  scale_shape_manual('Trait Type', values = c(19,19,19,19))+
  theme7()+ #begins in 1978
  labs(x = 'Year', y = 'Number of Studies')+
  scale_x_continuous(breaks = c(1978, 1983, 1988, 1993,
                                1998, 2003, 2008, 2013, 2018))


#for level of environmental filtering
for_fund_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Fundamental) %>%   
  filter(Fundamental == 1)%>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(Filter = 'Fundamental') %>% 
  select(Year, no_obs, Filter)
for_fund_hist$Filter = 'Abiotic' #match with upper plots
for_physical_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Physical) %>% 
  filter(Physical == 1)%>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(Filter = 'Physically Accessible') %>% 
  select(Year, no_obs, Filter)
for_physical_hist$Filter = 'Dispersal' #match with upper plots
for_ecolog_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Ecological) %>% 
  filter(Ecological == 1) %>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(Filter = 'Ecologically Accessible') %>% 
  select(Year, no_obs, Filter)
for_ecolog_hist$Filter = 'Biotic' #match with upper plots
for_troph_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Trophic) %>% 
  filter(Trophic == 1) %>% 
  summarize(no_obs = length(Year)) %>% 
  mutate(Filter = 'Trophic') %>% 
  select(Year, no_obs, Filter)
for_troph_hist$Filter = 'Trophic' #match with upper plots


all_filters_line = rbind(for_fund_hist, for_physical_hist, 
                         for_ecolog_hist, for_troph_hist)

filter_studies = ggplot(data = all_filters_line, aes(x = Year, y = no_obs, 
                                                     group = Filter, 
                                                     colour = Filter, 
                                                     shape = Filter, 
                                                     linetype = Filter), ) +
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

#for level of environmental filtering
temp = current
temp$Taxonomic = as.factor(temp$Taxonomic)
levels(temp$Taxonomic)[levels(temp$Taxonomic)=="Broad"] = "Multiple"
levels(temp$Taxonomic)[levels(temp$Taxonomic)=="Herps"] = "Herpetofauna"
for_tax_hist = temp %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Taxonomic) %>%   
  summarize(no_obs = length(Year))
for_tax_hist$Taxonomic = as.factor(for_tax_hist$Taxonomic)

for_tax_hist1 = for_tax_hist %>% 
  filter(Taxonomic %in% c('Birds', 'Fish', 'Herpetofauna', 'Mammals')) %>% 
  mutate(spine = 'yay')
for_tax_hist2 = for_tax_hist %>% 
  filter(Taxonomic %in% c('Plants', 'Insects', 'Plankton')) %>% 
  mutate(spine = 'nay')
for_tax_hist_3 = for_tax_hist %>% 
  filter(Taxonomic %in% c('Multiple', 'Other')) %>% 
  mutate(spine = 'poss')
forTax = rbind(for_tax_hist1, for_tax_hist2, for_tax_hist_3)
forTax$spine = as.factor(forTax$spine)
forTax$no_obs = as.integer(forTax$no_obs)
forTax$Taxonomic = as.character(forTax$Taxonomic)

pal = pnw_palette('Bay',10, type = 'continuous')
tax_studies = ggplot(data = forTax, aes(x = Year, y = no_obs, 
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

#for level of environmental filtering
global_driver = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, `Global Change Driver`) %>% 
  filter(`Global Change Driver` != 0) %>% 
  summarize(no_obs = length(Year)) %>% 
  select(Year, no_obs, `Global Change Driver`)
global_driver$`Global Change Driver` = 
  as.factor(global_driver$`Global Change Driver`)
levels(global_driver$
         `Global Change Driver`)[levels(global_driver$`Global Change Driver`)==
                                               "Global Change Broad"] = 
  "Multiple"
levels(global_driver$
         `Global Change Driver`)[levels(global_driver$`Global Change Driver`)==
                                               "Global Change Multiple"] = 
  "Multiple"
pal = pnw_palette('Bay', 6, type = 'continuous')
global_driver$Year = as.numeric(global_driver$Year)
global_plot = ggplot(data = global_driver, 
                     aes(x = Year, y = no_obs, 
                       group = `Global Change Driver`, 
                       colour = `Global Change Driver`, 
                       linetype = `Global Change Driver`,
                       shape = `Global Change Driver`)) +
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
library(cowplot)
alltypes = plot_grid(total_studies, ecosystem_studies, 
                     tax_studies, trait_studies, filter_studies, global_plot,
                           nrow = 2, ncol = 3, 
                     rel_widths = c(0.91,0.76,0.76, 0.91,0.77,0.77), 
                     rel_heights = c(0.75,1), 
                     labels = c('A', 'B','C','D','E','F'), 
                     label_x = c(0.17, 0.07, 0.07, 0.17,0.07,0.07), vjust = 2.5)

ggsave(here('./Figures/Heatplots-Timeseries/timeseries_small.png'), 
       plot = alltypes, 
       width = 18, height = 12, dpi = 200)
ggsave(here('./Figures/Heatplots-Timeseries/timeseries_large.png'), 
       plot = alltypes, 
       width = 18, height = 12, dpi = 1200)

