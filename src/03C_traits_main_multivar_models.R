########## 
##########
# This code contains the multivariate modeling component of the analysis presented in
# in Green et al. (2021) 
# A review on the use of traits in ecological research
##########
##########
# AUTHOR: Cole Brookson & Natasha A. Hardy
# DATE OF CREATION: 2022-03-21
##########
##########

# set-up =======================================================================

library(tidyverse)
library(vegan)
library(viridis)
library(mvabund)
library(here)
library(cowplot)


trait_main_use = 
  read_csv(here(paste0('./data/processed-data',
                       '/review_traits_clean_models.csv')))

# Ordination ===================================================================

###### begin NOTE ##############################################################
# The below section has been commented out for simpler use. The ordination takes 
# a good while to run, easily over 2 hours on a normal laptop, so we've saved
# the result as an RDS object, which can be easily read into the environment 
# using the code provided. This will allow the user to inspect the result and
# subsequent plots that are made in this script, without re-running the 
# ordination. Note that due to the nature of the analysis, re-running the actual
# ordination will result in slightly different (not significantly) results each
# time, and thus should be avoided. All statistics about the ordination itself 
# can be accessed via the RDS object after reading it into the environment.
###### end NOTE ################################################################

# split into 'sites' and 'species' just to put it into typical ecological
# multivariate context
traits_sites_use <- as.data.frame(trait_main_use[,1:10])
traits_species_use <- trait_main_use[,11:ncol(trait_main_use)] #1411 trait columns

# run actual ordination
# this is done in code doc '03B_traits_main_ordination.R' and output saved
# run time is approx. ~3h
# load the saved output of that ordination below

traits_ord_use = readRDS(here::here('./data/nmds-intermediate/traits_ord_use.rds'))

# Get results from ordination ==================================================

# extract scores
traits_ord_use_scores = data.frame(scores(traits_ord_use))
traits_ord_use_scores$points = rownames(traits_ord_use_scores)
traits_main_scores = cbind(traits_sites_use, traits_ord_use_scores)

# add species
traits_ord_use_scores = data.frame(scores(traits_ord_use, 'species'))
traits_ord_use_scores$species = rownames(traits_ord_use_scores)

# Plotting pipeline ============================================================

### All factors? ----
sapply(traits_main_scores[,1:10], class)

traits_main_scores$Ecosystem =
  as.factor(traits_main_scores$Ecosystem)
traits_main_scores$Taxonomic =
  as.factor(traits_main_scores$Taxonomic)
traits_main_scores$System =
  as.factor(traits_main_scores$System)
traits_main_scores$Predictive =
  as.factor(traits_main_scores$Predictive)
traits_main_scores$`Global Change Driver` =
  as.factor(traits_main_scores$`Global Change Driver`)
traits_main_scores$`Global Change` =
  as.factor(traits_main_scores$`Global Change`)
traits_main_scores$TOS =
  as.factor(traits_main_scores$TOS)
traits_main_scores$Filter =
  as.factor(traits_main_scores$Filter)


### Ecosystem ----
levels(traits_main_scores$Ecosystem)
eco = as.character(unique(traits_main_scores$Ecosystem))
for(i in 1:length(eco)) {
  temp = eco[i]
  df = traits_main_scores[
    traits_main_scores$Ecosystem == temp,
  ][chull(traits_main_scores[
    traits_main_scores$Ecosystem == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_eco_',temp), df)
}
hull_ecosystem = rbind(grp_eco_Terrestrial, grp_eco_Freshwater,
                          grp_eco_Marine, grp_eco_Broad)

### Taxonomic ----
unique(traits_main_scores$`Taxonomic`) 
summary(traits_main_scores$Taxonomic)
traits_main_scores = traits_main_scores %>% 
  dplyr::mutate(Taxonomic = replace_na(Taxonomic, "Other")) #Replace NA's in the vector
#Taxonomic group - hull loop
tax = as.character(unique(traits_main_scores$Taxonomic))
for(i in 1:length(tax)) {
  temp = tax[i]
  df = traits_main_scores[
    traits_main_scores$Taxonomic == temp,
  ][chull(traits_main_scores[
    traits_main_scores$Taxonomic == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_tax_',temp), df)
}
hull_taxonomic = rbind(grp_tax_Invertebrate, grp_tax_Multiple, grp_tax_Other, 
                       grp_tax_Plants, grp_tax_Vertebrate)

### TOS ----
levels(traits_main_scores$TOS)[levels(traits_main_scores$TOS)==
                                'TModel']='Theory'
tos = as.character(unique(traits_main_scores$TOS))
for(i in 1:length(tos)) {
  temp = tos[i]
  df = traits_main_scores[
    traits_main_scores$TOS == temp,
  ][chull(traits_main_scores[
    traits_main_scores$TOS == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_tos_',temp), df)
}
hull_tos = rbind(grp_tos_Observational, grp_tos_Experiment,
                    grp_tos_Metanalysis, grp_tos_Theory,
                    grp_tos_Review)

### Filter ----
levels(traits_main_scores$Filter)[levels(traits_main_scores$Filter)==
                                   "Fundamental"] = "Abiotic"
levels(traits_main_scores$Filter)[levels(traits_main_scores$Filter)==
                                   "Physical"] = "Dispersal"
levels(traits_main_scores$Filter)[levels(traits_main_scores$Filter)==
                                   "Ecological"] = "Biotic"
fil = as.character(unique(traits_main_scores$Filter))
for(i in 1:length(fil)) {
  temp = fil[i]
  df = traits_main_scores[
    traits_main_scores$Filter == temp,
  ][chull(traits_main_scores[
    traits_main_scores$Filter == temp,
    c("NMDS1", "NMDS2")
  ]), ]
  assign(paste0('grp_fil_',temp), df)
}
hull_fil = rbind(grp_fil_Abiotic, grp_fil_Biotic, grp_fil_Dispersal, grp_fil_Trophic)

### Global change binary ----
unique(traits_main_scores$`Global Change`) #no/yes
levels(traits_main_scores$`Global Change`)[levels(traits_main_scores$`Global Change`)=="no"] <- "No"
levels(traits_main_scores$`Global Change`)[levels(traits_main_scores$`Global Change`)=="yes"] <- "Yes"

#Yes
grp_clim_yes <- traits_main_scores[traits_main_scores$`Global Change` == "Yes", ][chull(traits_main_scores[traits_main_scores$`Global Change` == "Yes", c("NMDS1", "NMDS2")]), ] 
#No
grp_clim_no <- traits_main_scores[traits_main_scores$`Global Change` == "No", ][chull(traits_main_scores[traits_main_scores$`Global Change` == "No", c("NMDS1", "NMDS2")]), ] 

#combine the hull data
hull_clim <- rbind(grp_clim_yes, grp_clim_no)  

### Global change categories ----
unique(traits_main_scores$`Global Change Driver`)

summary(traits_main_scores$`Global Change Driver`)

levels(traits_main_scores$`Global Change Driver`)[levels(traits_main_scores$`Global Change Driver`)==0] <- "Not assessed"
levels(traits_main_scores$`Global Change Driver`)[levels(traits_main_scores$`Global Change Driver`)=="Global Change Broad"] <- "Multiple"
levels(traits_main_scores$`Global Change Driver`)[levels(traits_main_scores$`Global Change Driver`)=="Global Change Multiple"] <- "Multiple"
levels(traits_main_scores$`Global Change Driver`)[levels(traits_main_scores$`Global Change Driver`)=="Habitat Degredation"] <- "Habitat loss"
levels(traits_main_scores$`Global Change Driver`)[levels(traits_main_scores$`Global Change Driver`)=="Climate Change"] <- "Climate change"

#Global Change Drivers - hull loop
gcd = as.character(unique(traits_main_scores$`Global Change Driver`))
for(i in 1:length(gcd)) {
  temp = gcd[i]
  df = traits_main_scores[traits_main_scores$`Global Change Driver` == temp, ][chull(traits_main_scores[traits_main_scores$`Global Change Driver` == temp, c("NMDS1", "NMDS2")]), ]
  assign(paste0('grp_climd_',temp), df)
}

#combine the hull data
hull_gcd <- rbind(`grp_climd_Not assessed`,`grp_climd_Climate change`, grp_climd_Exploitation, grp_climd_Multiple, `grp_climd_Habitat loss`, grp_climd_Invasion)  

### Predictive ----
summary(traits_main_scores$Predictive)
levels(traits_main_scores$Predictive)[levels(traits_main_scores$Predictive)=="0"] <- "No"
levels(traits_main_scores$Predictive)[levels(traits_main_scores$Predictive)=="1"] <- "Yes"

#Yes
traits_main_scores$Predictive <- as.character(traits_main_scores$Predictive)
grp_pred_yes <- traits_main_scores[traits_main_scores$Predictive == "Yes", ][chull(traits_main_scores[traits_main_scores$Predictive == "Yes", c("NMDS1", "NMDS2")]), ] 
#No
grp_pred_no <- traits_main_scores[traits_main_scores$Predictive == "No", ][chull(traits_main_scores[traits_main_scores$Predictive == "No", c("NMDS1", "NMDS2")]), ]

#combine the hull data
hull_predict <- rbind(grp_pred_yes, grp_pred_no)  

## Make Plots ==========================================================

### Ecosystem ----
eco_plot = ggplot() +
  geom_polygon(data=hull_ecosystem,
               aes(x=NMDS1,y=NMDS2, fill=fct_rev(`Ecosystem`), 
                   group=fct_rev(`Ecosystem`)), alpha=0.30) +
  geom_point(data= traits_main_scores,
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_rev(`Ecosystem`)), size=2) +
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
                     name = "Ecosystem") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
                       name = "Ecosystem") 
eco_plot
ggsave(here('./figures/main-nmds/main-nmds_eco_small.png'),
       plot = tax_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/main-nmds/main-nmds_eco_large.png'),
       plot = tax_plot,
       width = 8, height = 8, dpi = 600)

### Tax group ----
tax_plot = ggplot() +
  geom_polygon(data = hull_taxonomic,
               aes(x=NMDS1 ,y=NMDS2, 
                   fill= fct_relevel(Taxonomic, 
                                     "Vertebrate", "Invertebrate", "Plants", "Other", "Multiple"), 
                   group= fct_relevel(Taxonomic, "Vertebrate", "Invertebrate", "Plants", "Other", "Multiple")),
               alpha=0.30) +
  geom_point(data = traits_main_scores,
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_relevel(Taxonomic, 
                                      "Vertebrate", "Invertebrate", "Plants", "Other", "Multiple")), 
             size=2) +
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
                     name = "Taxa") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
                       name = "Taxa")
tax_plot
ggsave(here('./figures/main-nmds/main-nmds_tax_small.png'),
       plot = tax_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/main-nmds/main-nmds_tax_large.png'),
       plot = tax_plot,
       width = 8, height = 8, dpi = 600)

### TOS ----
tos_plot = ggplot() +
  geom_polygon(data = hull_tos,
              aes(x = NMDS1, y = NMDS2, 
                  fill= fct_relevel(TOS, 
                                   "Observational", "Experiment", "Metanalysis", "Review", "Theory"),
                  group = fct_relevel(TOS, 
                                      "Observational", "Experiment", "Metanalysis", "Review", "Theory")),
              alpha=0.30) +
  geom_point(data = traits_main_scores,
             aes(x=NMDS1, y=NMDS2, 
                 colour = fct_relevel(TOS, "Observational", "Experiment", "Metanalysis", "Review", "Theory")), 
             size=2) + 
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
                     name = "Study Type") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
                       name = "Study Type")
tos_plot
ggsave(here('./figures/main-nmds/main-nmds_tos_small.png'),
       plot = tos_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/main-nmds/main-nmds_tos_large.png'),
       plot = tos_plot,
       width = 8, height = 8, dpi = 600)

### Filter ----
fil_plot = ggplot() +
  geom_polygon(data=hull_fil, 
               aes(x=NMDS1, y=NMDS2, 
                   fill= fct_relevel(Filter, "Abiotic", "Biotic", "Dispersal", "Trophic"), 
                   group=fct_relevel(Filter, "Abiotic", "Biotic", "Dispersal", "Trophic")),
               alpha=0.30) + # add the convex hulls
  geom_point(data=traits_main_scores,
             aes(x=NMDS1, y=NMDS2, 
                 colour = fct_relevel(Filter, "Abiotic", "Biotic", "Dispersal", "Trophic")), size=2) +
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
                     name = "Filter") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
                       name = "Filter")
fil_plot
ggsave(here('./figures/main-nmds/main-nmds_fil_small.png'),
       plot = fil_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/main-nmds/main-nmds_fil_large.png'),
       plot = fil_plot,
       width = 8, height = 8, dpi = 600)

### Global change ----
gc_plot = ggplot() + 
  geom_polygon(data=hull_gcd, 
               aes(x=NMDS1, y=NMDS2, 
                   fill= fct_relevel(`Global Change Driver`, 
                                     "Not assessed", "Climate change", "Exploitation", 
                                     "Multiple", "Habitat loss", "Invasion"), 
                   group=fct_relevel(`Global Change Driver`, 
                                     "Not assessed", "Climate change", "Exploitation", 
                                     "Multiple", "Habitat loss", "Invasion")), 
               alpha=0.30) + # add the convex hulls
  geom_point(data=traits_main_scores, 
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_relevel(`Global Change Driver`, 
                                      "Not assessed", "Climate change", "Exploitation", 
                                      "Multiple", "Habitat loss", "Invasion")), 
             size=2) + # add the point markers
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()) +
  scale_fill_viridis(option="magma", discrete = TRUE, begin = 0.8, end = 0.2, 
                     name = "Driver of change") + #name = "Global Change Assessed"
  scale_colour_viridis(option="magma", discrete = TRUE, begin = 0.8, end = 0.2, 
                       name = "Driver of change") #+ #name = "Global Change Assessed"
gc_plot
ggsave(here('./figures/main-nmds/main-nmds_gc_small.png'),
       plot = gc_plot,
       width = 8, height = 8, dpi = 200)
ggsave(here('./figures/main-nmds/main-nmds_gc_large.png'),
       plot = gc_plot,
       width = 8, height = 8, dpi = 600)

### Predictive ----
pred_plot = ggplot() +
  geom_polygon(data=hull_predict,
               aes(x=NMDS1,y=NMDS2,
                   fill=Predictive,
                   group=Predictive), alpha=0.30) +
  geom_point(data=traits_main_scores,
             aes(x=NMDS1,y=NMDS2, colour = Predictive), size=2) +
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()) +
  scale_fill_viridis(option = 'magma', discrete = TRUE, begin = 0.8, end = 0.2,
                     name = "Predictive") +
  scale_colour_viridis(option = 'magma',discrete = TRUE, begin = 0.8, end = 0.2,
                       name = "Predictive")
pred_plot
ggsave(here('./figures/main-nmds/main-nmds_pred_small.png'),
       plot = pred_plot,
       width = 8, height = 9, dpi = 200)
ggsave(here('./figures/main-nmds/main-nmds_plot_pred_large.png'),
       plot = pred_plot,
       width = 8, height = 9, dpi = 600)

## Merge plots ----

#All 6 panels

review_main_nMDS_6 =
  plot_grid(tax_plot, tos_plot, eco_plot,
            fil_plot, gc_plot, pred_plot,
            labels = c("A", "B", "C", "D", "E", "F"),
            label_x = 0.15, vjust = 3.5, ncol = 2,
            rel_widths = c(1, 1), align = "v")
review_main_nMDS_6
ggsave(here('./figures/main-nmds/review_main_nMDS_6_small.png'),
       plot = review_main_nMDS_6,
       width = 16, height = 12, dpi = 200)
ggsave(here('./figures/main-nmds/review_main_nMDS_6_large.png'),
       plot = review_main_nMDS_6,
       width = 16, height = 12, dpi = 600)




