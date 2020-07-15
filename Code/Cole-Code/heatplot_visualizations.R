library(tidyverse)
library(RColorBrewer)
library(viridis)
library(scales)
library(tm)
library(wordcloud)
library(PNWColors)
library(extrafont)
library(cowplot)

#font_import()
#loadfonts(device = "win")
#fonttable()

current = read_csv('Working-Lit-DB-forR.csv')

unique(current$`Relevant to Study`)


current = current %>% 
  filter(`Relevant to Study` == 'Y' | `Relevant to Study` == 'y')
#levels(current$value)[levels(mydt$value)=="A"] <- "X"

############################## Trait by Ecosystem

#data manip 
trait_env_df1 = current %>% 
  select(Ecosystem, Morphological, NEWPhysiological, Behavioural, `Life History`)

trait_env_df1$Ecosystem = as.factor(trait_env_df1$Ecosystem)

trait_env_df1 = trait_env_df1 %>% 
  rename(Physiological = NEWPhysiological)

trait_env_df1 = trait_env_df1 %>% 
  gather(Trait, Count, Morphological, Physiological, Behavioural, `Life History`) %>% 
  group_by(Ecosystem, Trait)

trait_env_df1 = trait_env_df1 %>% 
  group_by(Ecosystem, Trait) %>% 
  summarize(Total = sum(Count))

#make a theme
theme1 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 20, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(axis.text.x = element_text(size = 15, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 15, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 18, color = color.axis.title, vjust = 0, margin = margin(t = 17, r = 0, b = 17, l = 0))) +
    theme(axis.title.y = element_text(size = 18, color = color.axis.title, vjust = 1.5, margin = margin(t = 0, r = 17, b = 0, l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) +
    theme(strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 16))+
    theme(legend.position = 'right') +
    theme(plot.title = element_text(hjust = 0.5))
}
theme2 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'grey80')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 20, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(axis.text.x = element_text(size = 18, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 18, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 18, color = color.axis.title, vjust = 0, margin = margin(t = 17, r = 0, b = 17, l = 0))) +
    theme(axis.title.y = element_text(size = 18, color = color.axis.title, vjust = 1.5, margin = margin(t = 0, r = 17, b = 0, l = 17))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="grey80", size = 0.15),
          axis.line.y = element_line(color="grey80", size = 0.15)) +
    theme(strip.background = element_rect(fill = c('#FCFDBFFF'), 
                                          colour = 'grey80'),
          strip.placement = 'outside',
          strip.text = element_text(size = 16))+
    theme(legend.position = 'right',
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5))
}
theme3 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'grey80')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 20, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(axis.text.x = element_text(size = 15, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 15, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 18, color = color.axis.title, vjust = 0, margin = margin(t = 17, r = 0, b = 17, l = 0))) +
    theme(axis.title.y = element_text(size = 18, color = color.axis.title, vjust = 1.5, margin = margin(t = 0, r = 17, b = 0, l = 17))) +
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

trait_by_ecosystem_plot = ggplot(data = trait_env_df1, aes(x = Ecosystem, y = Trait, fill = Total)) +
  geom_tile() +
  labs(x = 'Ecosystem', y = 'Trait of Interest', title = 'Trait By Ecosystem')+
  scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  theme1()

#################### Trait by Ecosystem x Type of Study
#data manip 
trait_env_df2 = current %>% 
  select(Ecosystem, Morphological, NEWPhysiological, Behavioural, `Life History`, 
         QModel, Review, Observational, TModel, Experiment, Metanalysis)

trait_env_df2 = trait_env_df2 %>% 
  rename(Physiological = NEWPhysiological)

trait_env_df2$Ecosystem = as.factor(trait_env_df2$Ecosystem)

trait_env_df2 = trait_env_df2 %>% 
  group_by(Ecosystem) %>% 
  gather(TOS, CountTOS, QModel, Review, Observational, TModel, Experiment, Metanalysis) %>% 
  filter(CountTOS == 1)
levels(trait_env_df2$TOS)[levels(trait_env_df2$TOS)=="QModel"] <- "Observational"

trait_env_df2$TOS = as.factor(trait_env_df2$TOS)

trait_env_df2 = trait_env_df2 %>% 
  group_by(TOS) %>% 
  gather(Trait, CountT, Morphological, Physiological, Behavioural, `Life History`) %>% 
  filter(CountT == 1)

trait_env_df2 = trait_env_df2 %>% 
  group_by(Ecosystem, Trait, TOS) %>% 
  summarize(Total = sum(CountT))

#test
trait_env_df2 %>% 
  filter(Ecosystem == 'Broad' & Trait == 'Behavioural' & TOS == 'Experiment')
trait_env_df2 %>% 
  filter(Ecosystem == 'Freshwater' & Trait == 'Physiological' & TOS == 'QModel')

#find the viridis colours I need
# v_colors =  viridis(200, option = "A")
# v_colors[200]

trait_by_ecosystem_TOS_plot = ggplot(data = trait_env_df2, aes(x = TOS, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(~ Ecosystem, switch = "x", scales = "free_x", space = "free_x") +
  labs(x = 'Ecosystem', y = 'Trait of Interest', title = 'Trait Distribution By Ecosystem and Type of Study')+
  scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  #scale_fill_gradient(name = "Number of Studies", low = "springgreen4", high = "magenta") +
  theme2() #export at 1600x900

########################## Trait x Env Filtering by Ecosystem x TOS
#data manip 
trait_env_df3 = current %>% 
  select(Ecosystem, Morphological, NEWPhysiological, Behavioural, `Life History`, 
         QModel, Review, Observational, TModel, Experiment, Metanalysis, `Fundamental`, `Physical`, 
         `Ecological`, `Trophic`)

trait_env_df3 = trait_env_df3 %>% 
  rename(Physiological = NEWPhysiological)

trait_env_df3$Ecosystem = as.factor(trait_env_df3$Ecosystem)

trait_env_df3 = trait_env_df3 %>% 
  group_by(Ecosystem) %>% 
  gather(TOS, CountTOS, QModel, Review, Observational, TModel, Experiment, Metanalysis) %>% 
  filter(CountTOS == 1)


trait_env_df3$TOS = as.factor(trait_env_df3$TOS)

trait_env_df3 = trait_env_df3 %>% 
  group_by(Ecosystem) %>% 
  gather(Filter, CountFIL, `Fundamental`, `Physical`, 
         `Ecological`, `Trophic`) %>% 
  filter(CountFIL == 1)

levels(trait_env_df3$TOS)[levels(trait_env_df3$TOS)=="QModel"] <- "Observational"
levels(trait_env_df3$TOS)[levels(trait_env_df3$TOS)=="TModel"] <- "Theoretical"
levels(trait_env_df3$Filter)[levels(trait_env_df3$Filter)=="Fundamental"] <- "Abiotic"
levels(trait_env_df3$Filter)[levels(trait_env_df3$Filter)=="Physical"] <- "Dispersal"
levels(trait_env_df3$Filter)[levels(trait_env_df3$Filter)=="Ecological"] <- "Biotic"
levels(trait_env_df3$Ecosystem)[levels(trait_env_df3$Ecosystem)=="Broad"] <- "Multiple"

trait_env_df3$Filter = as.factor(trait_env_df3$Filter)

trait_env_df3 = trait_env_df3 %>% 
  group_by(TOS) %>% 
  gather(Trait, CountT, Morphological, Physiological, Behavioural, `Life History`) %>% 
  filter(CountT == 1)

trait_env_df3 = trait_env_df3 %>% 
  group_by(Ecosystem, Trait, TOS, Filter) %>% 
  summarize(Total = sum(CountT))

#trait_env_df3$Filter = factor(trait_env_df3$Filter, levels = c('Abiotic', 'Dispersal', 'Biotic', 'Trophic'))
trait_env_df3$Trait = factor(trait_env_df3$Trait, levels = c('Life History', 'Behavioural', 'Morphological', 'Physiological'))

trait_x_filter_by_ecosystem_x_TOS_plot = ggplot(data = trait_env_df3, aes(x = TOS, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, switch = "both", scales = "free_x", space = "free_x") +
  labs(x = 'Study type by ecosystem', y = 'Trait type by level of environmental filtering')+
  scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  #scale_fill_gradient(name = "Number of Studies", low = "springgreen4", high = "magenta") +
  theme2() #export at 1600x900
# trait_x_filter_by_ecosystem_x_TOS_plot = 
#   ggdraw(trait_x_filter_by_ecosystem_x_TOS_plot) + 
#   geom_text(
#     data = data.frame(x = 0.205, y = 0.8, label = "A"),
#     aes(x, y, label = label),
#     hjust = 0.5, vjust = 0, angle = 0, size = 18/.pt,
#     color = "black",
#     inherit.aes = FALSE
#   )
ggsave('FilterLevel-Trait-Ecosystem-StudyType.png',trait_x_filter_by_ecosystem_x_TOS_plot,dpi = 300,
       height = 10, width = 17)
###### OPTION 2: zeros a separate colour
trait_env_df3_copy = trait_env_df3
trait_env_df3_copy$Total[trait_env_df3_copy$Total ==0] <- NA

trait_x_filter_by_ecosystem_x_TOS_plot_2 = ggplot(data = trait_env_df3_copy, aes(x = TOS, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, switch = "both", scales = "free_x", space = "free_x") +
  labs(x = 'Study type by ecosystem', y = 'Trait type by level of environmental filtering')+
  scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1, na.value = 'grey95') +
  #scale_fill_gradient(name = "Number of Studies", low = "springgreen4", high = "magenta") +
  theme2() #export at 1600x900

########################## Trait x Env Filtering by Ecosystem x Taxonomic
#data manip 
trait_env_df4 = current %>% 
  select(Ecosystem, Morphological, NEWPhysiological, Behavioural, `Life History`, 
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
  gather(Trait, CountT, Morphological, Physiological, Behavioural, `Life History`) %>% 
  filter(CountT == 1)

trait_env_df4 = trait_env_df4 %>% 
  group_by(Ecosystem, Trait, Taxonomic, Filter) %>% 
  summarize(Total = sum(CountT))

levels(trait_env_df4$Taxonomic)[levels(trait_env_df4$Taxonomic)=="Herps"] <- "Herpetofauna"
levels(trait_env_df4$Taxonomic)[levels(trait_env_df4$Taxonomic)=="Broad"] <- "Multiple"

trait_env_df4$Filter = factor(trait_env_df4$Filter, levels = c('Fundamental', 'Physical', 'Ecological', 'Trophic'))
trait_env_df4$Trait = factor(trait_env_df4$Trait, levels = c('Life History', 'Behavioural', 'Morphological', 'Physiological'))
trait_env_df4$Taxonomic = factor(trait_env_df4$Taxonomic, 
                                 levels = c('Plants', 'Plankton', 'Insects', 'Herpetofauna', 'Birds', 'Fish', 'Mammals', 'Multiple', 'Other'))

levels(trait_env_df4$Filter)[levels(trait_env_df4$Filter)=="Fundamental"] <- "Abiotic"
levels(trait_env_df4$Filter)[levels(trait_env_df4$Filter)=="Physical"] <- "Dispersal"
levels(trait_env_df4$Filter)[levels(trait_env_df4$Filter)=="Ecological"] <- "Biotic"
levels(trait_env_df4$Ecosystem)[levels(trait_env_df4$Ecosystem)=="Broad"] <- "Multiple"

trait_x_filter_by_ecosystem_x_taxonomic_plot = ggplot(data = trait_env_df4, aes(x = Taxonomic, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, switch = "both", scales = "free_x", space = "free_x") +
  labs(x = 'Taxonomic group by ecosystem', y = 'Trait type by level of environmental filtering')+
  scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  #scale_fill_gradient(name = "Number of Studies", low = "springgreen4", high = "magenta") +
  theme2() #export at 1600x900
ggsave('FilterLevel-Trait-Ecosystem-Taxonomic.png',trait_x_filter_by_ecosystem_x_taxonomic_plot,dpi = 300,
       height = 10, width = 17)
############################## Other colour options
trait_x_filter_by_ecosystem_x_TOS_plot_purp = ggplot(data = trait_env_df3, aes(x = TOS, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, switch = "both", scales = "free_x", space = "free_x") +
  labs(x = 'Ecosystem & Type of Study', y = 'Level of Environmental Filtering & Trait of Interest', title = 'Environmental Filtering by Traits, Ecosystem, and Study Type')+
  #scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  scale_fill_gradient(low = "grey92", high = "darkorchid4") +
  theme3() #export at 1600x900

trait_x_filter_by_ecosystem_x_taxonomic_plot_purp = ggplot(data = trait_env_df4, aes(x = Taxonomic, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, switch = "both", scales = "free_x", space = "free_x") +
  labs(x = 'Ecosystem & Taxonomic Group', y = 'Level of Environmental Filtering & Trait of Interest', title = 'Environmental Filtering by Traits, Ecosystem, and Taxonomy')+
  #scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  scale_fill_gradient(low = "grey92", high = "darkorchid4") +
  theme3() #export at 1600x900

######################################## Taxonomic version with logical things being removed

#we're going to exclude taxonomic groups that obviously don't belong in particular groups - i.e. fish in Terrestrial
#and Herps in Marine
trait_env_df4_TL = trait_env_df4 %>% 
  filter(Ecosystem == 'Terrestrial')

trait_env_df4_ME = trait_env_df4 %>% 
  filter(Ecosystem == 'Marine')

trait_env_df4_others = trait_env_df4 %>% 
  filter(Ecosystem %in% c('Freshwater', 'Broad'))
`%notin%` <- Negate(`%in%`)
trait_env_df4_TL = trait_env_df4_TL %>% 
  filter(Taxonomic %notin% c('Fish', 'Plankton'))
trait_env_df4_ME = trait_env_df4_ME %>% 
  filter(Taxonomic != 'Herps')

trait_env_df4_Pared = rbind(trait_env_df4_others, trait_env_df4_TL, trait_env_df4_ME)

#make factors
trait_env_df4_Pared$Filter = factor(trait_env_df4_Pared$Filter, levels = c('Fundamental', 'Physical', 'Ecological', 'Trophic'))
trait_env_df4_Pared$Trait = factor(trait_env_df4_Pared$Trait, levels = c('Behavioural', 'Life History','Morphological', 'Physiological'))
trait_env_df4_Pared$Taxonomic = factor(trait_env_df4_Pared$Taxonomic, 
                                 levels = c('Plants', 'Mammals', 'Birds', 'Fish', 'Insects', 'Herps', 'Multiple', 'Broad', 'Other', 'Plankton'))

#make plots again
trait_x_filter_by_ecosystem_x_taxonomic_plot_pared = ggplot(data = trait_env_df4_Pared, aes(x = Taxonomic, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, switch = "both", scales = "free_x", space = "free_x") +
  labs(x = 'Ecosystem & Taxonomic Group', y = 'Level of Environmental Filtering & Trait of Interest', title = 'Environmental Filtering by Traits, Ecosystem, and Taxonomy')+
  scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  #scale_fill_gradient(name = "Number of Studies", low = "springgreen4", high = "magenta") +
  theme2() #export at 1600x900

trait_x_filter_by_ecosystem_x_taxonomic_plot_purp_pared = ggplot(data = trait_env_df4_Pared, aes(x = Taxonomic, y = Trait, fill = Total)) +
  geom_tile() +
  facet_grid(Filter ~ Ecosystem, switch = "both", scales = "free_x", space = "free_x") +
  labs(x = 'Ecosystem & Taxonomic Group', y = 'Level of Environmental Filtering & Trait of Interest', title = 'Environmental Filtering by Traits, Ecosystem, and Taxonomy')+
  #scale_fill_viridis(discrete = FALSE, option = 'A', direction = -1) +
  scale_fill_gradient(low = "grey92", high = "darkorchid4") +
  theme3() #export at 1600x900

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
          random.order = FALSE, min.freq = 2, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#marine - min. 2
wordcloud(marine$Traits, marine$Frequency, scale=c(3,.55), 
          random.order = FALSE, min.freq = 2, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophic - min. 2
wordcloud(trophic$Trait, trophic$`Number of Times Used`, scale=c(3,.55), 
          random.order = FALSE, min.freq = 2, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophicsub - min. 2
wordcloud(trophicsub$Trait, trophicsub$`Number of Times Used`, scale=c(3,.55), 
          random.order = FALSE, min.freq = 2, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))

#fish - min. 1
wordcloud(fish$Trait, fish$`Number of Times Used`, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#marine - min. 1
wordcloud(marine$Traits, marine$Frequency, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophic - min. 2
wordcloud(trophic$Trait, trophic$`Number of Times Used`, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))
#trophicsub - min. 1
wordcloud(trophicsub$Trait, trophicsub$`Number of Times Used`, scale=c(2.2,.6), 
          random.order = FALSE, min.freq = 1, max.words = Inf, rot.per = 0.15, colors=brewer.pal(8, 'Dark2'))

################################## Pulling some summary stats from final database
traits_dummy = read_csv('traits_dummy.csv')
table(traits_dummy$`Global Change Driver`)
current %>% 
  filter(Ecosystem == 'Broad')
nrow(current %>% 
  filter(`Global Change Driver` != 0))/822
current$`Global Change Driver` = as.factor(current$`Global Change Driver`)
table(current$`Global Change Driver`)
current %>% 
  filter(`Life History` == 1)
current %>% 
  filter(Trophic == 1)
journals = current %>% 
  group_by(Journal) %>% 
  summarize(no_rows = length(Journal)) %>% 
  filter(no_rows > 10)

current %>% 
  mutate(sum = Physiological+Morphological+`Life History`+Behavioural) %>% 
  filter(sum > 3)
current %>% 
  mutate(sum = Fundamental+ Ecological+ Physical+ Trophic) %>% 
  filter(sum > 3)

#%>% 
  # filter(Journal %in% c('Journal of Applied Ecology', 'FUNCTIONAL ECOLOGY', 'METHODS IN ECOLOGY AND EVOLUTION', 
  #                       'JOURNAL OF ECOLOGY', 'FUNCTIONAL ECOLOGY'))



######## Studies over time

for_hist = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year) %>% 
  summarize(no_obs = length(Year))
for_hist$n = for_hist$no_obs

#Current change global change driver
levels(current$`Global Change Driver`)[levels(current$`Global Change Driver`)=="Global Change Broad"] <- "Multiple"
levels(current$`Global Change Driver`)[levels(current$`Global Change Driver`)=="Global Change Multiple"] <- "Multiple"


theme4 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 26, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_text(size = 20, color = color.axis.text)) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 26, color = color.axis.title, vjust = 1.5, margin = margin(t = 0, r = 17, b = 0, l = 17))) +
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
theme5 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 26, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
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
theme6 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 26, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(axis.text.x = element_text(size = 20, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 20, color = 'black')) + 
    theme(axis.title.x = element_text(size = 26, color = color.axis.title, vjust = 0, margin = margin(t = 17, r = 0, b = 17, l = 0))) +
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
theme7 <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_blank()) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.line.x.bottom = element_line(colour = 'black', size = 1))+
    theme(axis.line.x.top = element_line(colour = 'white'))+
    theme(axis.line.y.left = element_line(colour = 'black', size = 1))+
    theme(axis.line.y.right = element_line(colour = 'white'))+
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 26, vjust = 1.25, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
    theme(axis.text.x = element_text(size = 20, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 20, color = 'black')) + 
    theme(axis.title.x = element_text(size = 26, color = color.axis.title, vjust = 0, margin = margin(t = 17, r = 0, b = 17, l = 0))) +
    theme(axis.title.y = element_text(size = 26, color = 'black', vjust = 1.5, margin = margin(t = 0, r = 17, b = 0, l = 17))) +
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
  #geom_segment(aes(x=3.6, xend=4.3, y=75, yend=75), size = 1.5, colour = 'midnightblue') + 
  #geom_text(aes(x = 7.6, y = 80, label = 'Total Published Studies', size = 2), colour = 'black', family = 'Calibri')+
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

all_traits_line = rbind(for_behav_hist, for_morph_hist, for_life_hist, for_phys_hist)

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
  scale_x_discrete(breaks = c(1978, 1983, 1988, 1993, 1998, 2003, 2008, 2013, 2018))


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


all_filters_line = rbind(for_fund_hist, for_physical_hist, for_ecolog_hist, for_troph_hist)

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
  scale_x_discrete(breaks = c(1978, 1983, 1988, 1993, 1998, 2003, 2008, 2013, 2018))

#for level of environmental filtering
temp = current
temp$Taxonomic = as.factor(temp$Taxonomic)
levels(temp$Taxonomic)[levels(temp$Taxonomic)=="Broad"] <- "Multiple"
levels(temp$Taxonomic)[levels(temp$Taxonomic)=="Herps"] <- "Herpetofauna"
for_tax_hist = temp %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, Taxonomic) %>%   
  summarize(no_obs = length(Year))
for_tax_hist$Taxonomic = as.factor(for_tax_hist$Taxonomic)
#levels(for_tax_hist$Taxonomic)[levels(for_tax_hist$Taxonomic)=="Broad"] <- "Multiple"
#levels(for_tax_hist$Taxonomic)[levels(for_tax_hist$Taxonomic)=="Herps"] <- "Herptofauna"
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
  scale_linetype_manual('Taxonomic Focus', values=c(1,5,6,4,2,3,1,5,6)) +
  scale_shape_manual('Taxonomic Focus', values = c(19,19,19,19,19,19,19,19,19)) +
  labs(x = 'Year', y = 'Number of Studies')

#for level of environmental filtering
global_driver = current %>% 
  filter(Year != 'in review' & Year < 2019) %>% 
  group_by(Year, `Global Change Driver`) %>% 
  filter(`Global Change Driver` != 0) %>% 
  summarize(no_obs = length(Year)) %>% 
  select(Year, no_obs, `Global Change Driver`)
global_driver$`Global Change Driver` = as.factor(global_driver$`Global Change Driver`)
levels(global_driver$`Global Change Driver`)[levels(global_driver$`Global Change Driver`)=="Global Change Broad"] <- "Multiple"
levels(global_driver$`Global Change Driver`)[levels(global_driver$`Global Change Driver`)=="Global Change Multiple"] <- "Multiple"
pal = pnw_palette('Bay', 6, type = 'continuous')
global_driver$Year = as.numeric(global_driver$Year)
global_plot = ggplot(data = global_driver, aes(x = Year, y = no_obs, 
                                               group = `Global Change Driver`, 
                                               colour = `Global Change Driver`, 
                                               linetype = `Global Change Driver`,
                                               shape = `Global Change Driver`)) +
  geom_line(size = 1.08) +
  geom_point(size = 3)+
  theme6()+ #begins in 1992
  scale_color_manual('Global Change Driver', values = pal) +
  scale_linetype_manual('Global Change Driver', values=c(1,5,6,4,2,3)) +
  scale_shape_manual('Global Change Driver', values = c(19,19,19,19,19,19)) +
  labs(x = 'Year', y = 'Number of Studies') +
  scale_y_continuous(limits = c(0,15)) +
  scale_x_continuous(limits = c(1978, 2019), breaks = c(1978, 1983, 1988, 1993, 1998, 2003, 2008, 2013, 2018))
library(cowplot)
alltypes = plot_grid(total_studies, ecosystem_studies, tax_studies, trait_studies, filter_studies, global_plot,
                           nrow = 2, ncol = 3, rel_widths = c(0.91,0.76,0.76, 0.91,0.77,0.77), rel_heights = c(0.75,1), 
                     labels = c('A', 'B','C','D','E','F'), label_x = c(0.17, 0.07, 0.07, 0.17,0.07,0.07), vjust = 2.5)
ggsave('all_studies_by_year.png', plot = alltypes, width = 24, height = 12, dpi = 200)
######### random stats pulling
nrow(current %>% 
       rowwise() %>% 
       filter(sum(Fundamental, Physical, Ecological, Trophic) > 2))

nrow(current %>% 
       filter(`Global Change Driver` != 0))

nrow(current %>% 
       filter(`Forecasting/Predictive` != 0) %>% 
       filter(`Global Change Driver` != 0))
nrow(current %>% 
       filter(Taxonomic == 'Plants') %>% 
       filter(Morphological ==1 | `Life History` == 1))
nrow(current %>% 
       filter(Ecosystem %in% c('Marine', 'Freshwater')) %>% 
       filter(Taxonomic == 'Fish'))
nrow(current %>% 
       filter(Ecosystem == 'Terrestrial') %>% 
       filter(Taxonomic == 'Plants'))
#these did do the thing from 'inter-specific'

!168 #ones that didn't do the thing

#got to 297 in the 'interspecific' search

#is there a plant example that actually predicted a change in community structure
#or biogeography?
#we could feature the ones that are moving towards the predictive framework
#ideally in the plant world

#select a few of the predictive papers to have a plot that pulls out the key elements
#of those papers and highlight them in a multi-panel figure. In those case studies, we
#could also look for opportuniteis to highlihgt htose ones in the 'challenges and 
#opportunites' part of the paper ---- go back to the drawing board to those 22 papers
#and figure out what we want from them for case studies etc. 
