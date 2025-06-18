# Summarize data by training, validation, and test splits

library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)   # For log transformations
library(patchwork) 
library(viridis)


## Read in and create summaries ------------------------------------------------

#read in reviewed images
tags_reviewed <- fread('outputs/reviewed_images.csv')
  
#total number of images reviewed
length(unique(tags_reviewed$filename))

#how many unique hexagons, cameras, and camera deployments?
length(unique(tags_reviewed$hex)) 
length(unique(tags_reviewed$site)) 
length(unique(tags_reviewed$site_check))

#clean up some species tags
sort(unique(tags_reviewed$species)) 
tags_reviewed$species_cleaned <- tags_reviewed$species
tags_reviewed$species_cleaned <- ifelse(tags_reviewed$species == 'dog', 'domestic_dog', tags_reviewed$species_cleaned)
tags_reviewed$species_cleaned <- ifelse(tags_reviewed$species_cleaned == 'jackal', 'side-striped_jackal', tags_reviewed$species_cleaned)
tags_reviewed$species_cleaned <- ifelse(tags_reviewed$species_cleaned == 'vervet', 'vervet_monkey', tags_reviewed$species_cleaned)
tags_reviewed$species_cleaned <- ifelse(tags_reviewed$species_cleaned %in% c('duiker','red_duiker','grysbok','klipspringer'), 
                                   'small_antelope', tags_reviewed$species_cleaned)
sort(unique(tags_reviewed$species_cleaned)) 

#create site summary
cameraSummary <- tags_reviewed %>% select(-V1) %>% 
                                   group_by(site) %>%
                                   summarise(n_photos = n(),
                                             n_checks = length(unique(site_check)))
summary(cameraSummary$n_photos) #mean photos per camera
table(cameraSummary$n_checks, useNA = 'a') #number of checks each

#create hex summary
hex_summary <- tags_reviewed %>% select(-V1) %>% 
                                 group_by(hex) %>% 
                                 summarise(n_photos = n())
summary(hex_summary$n_photos) #mean photos per hex

#create species summary
species_summary <- tags_reviewed %>% select(-V1) %>% 
                                     group_by(species_cleaned) %>% 
                                     summarise(n_photos = n(),
                                               n_hex = length(unique(hex)),
                                               n_sites = length(unique(site)))
  
  
## Compare distribution of classes in evaluation set vs training set -----------

#read in training data species summary
train_species <- fread('data/training_summaries/training_classes_summary.csv')
  
#edit some groups
sort(unique(train_species$species))
train_species <- train_species %>%
  mutate(species = if_else(species == "dog", "domestic_dog", species)) %>%
  mutate(species = if_else(species == "jackal", "side-striped_jackal", species)) %>%
  group_by(species, split) %>%
  summarise(number_images = sum(number_images, na.rm = TRUE), 
            unique_hexagons = sum(unique_hexagons, na.rm = TRUE), 
            unique_sites = sum(unique_sites, na.rm = TRUE), 
            .groups = "drop")
  
#edit eval dataframe for merging
species_summary_mod <- species_summary
colnames(species_summary_mod) <- c('species','number_images','unique_hexagons','unique_sites')
species_summary_mod$split <- 'test'

#merge
species_combined <- bind_rows(train_species, species_summary_mod)

#save
write.csv(species_combined, 'outputs/species_summary_train_test.csv')

 
## Prep for plotting -----------------------------------------------------------

#remove some classes not included in model training
classes_remove <- c('palm_civet','other_animal','other_bird','small_mammal','empty')

#clean up other names for plot
species_combined$species <- ifelse(species_combined$species == 'small_antelope','small antelope',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'vervet_monkey','vervet monkey',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'bush_squirrel','squirrel',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'ground_hornbill','ground hornbill',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'guinea_fowl','guineafowl',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'honey_badger','honey badger',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'scrub_hare','savanna hare',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'blue_monkey','blue monkey',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'bat_sp.','bat',species_combined$species)
# species_combined$species <- ifelse(species_combined$species == 'domestic_dog','dog',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'domestic_cattle','cattle',species_combined$species)
species_combined$species <- ifelse(species_combined$species == 'side-striped_jackal','jackal',species_combined$species)

#add asterisk to translocated species
transloc <- c('elephant','buffalo','sable','impala','waterbuck','warthog','eland','zebra','kudu')
species_combined$species <- ifelse(species_combined$species %in% transloc,
                                   paste0(species_combined$species, '*'), as.character(species_combined$species))

#complete all combinations of train/test/val
species_combined <- species_combined %>% 
  complete(species, split = unique(split), 
           fill = list(number_images = 0, unique_hexagons = 0, unique_sites = 0))

#plot number of images by class
class_number <- ggplot(species_combined[!species_combined$species %in% classes_remove,],
                       aes(x = reorder(species, number_images), y = number_images, fill = split)) +
    geom_bar(stat = 'identity', position = 'dodge') + 
    scale_fill_brewer(palette = 'Set2') +
    labs(fill = 'Split') + ylab('Number of images') +
    coord_flip() +
    theme_bw() + theme(axis.title.y = element_blank())
class_number
#ggsave('figures/images_train_val.png', class_number, dpi = 600, width = 6, height = 4)
    
#calculate proportion of dataset (not including empty images)
species_combined <- species_combined %>%
  filter(species != 'empty') %>%
  group_by(split) %>%
  mutate(pct = number_images / sum(number_images)) %>%
  ungroup()
  
#plot proportion of split by class
class_prop <- ggplot(species_combined[!species_combined$species %in% classes_remove,], 
                     aes(x = reorder(species, pct), y = pct, fill = split)) +
    geom_bar(stat = 'identity', position = 'dodge') + 
    scale_fill_brewer(palette = 'Set2') +
    labs(fill = 'Split') + ylab('Proportion of dataset') +
    coord_flip() +
    theme_bw() + theme(axis.title.y = element_blank())
class_prop
#ggsave('figures/images_train_val_prop.png', class_prop, dpi = 600, width = 6, height = 4)

#plot in 3 facets according to test set size so we can see small ones (>5000, 1000-5000, <1000)
# species_over5000 <- unique(species_combined[species_combined$number_images >= 5000 & 
#                                               species_combined$split == 'test',]$species)
# species_1000to5000 <- unique(species_combined[species_combined$number_images < 5000 &  
#                                                 species_combined$number_images >= 500 &
#                                                 species_combined$split == 'test',]$species)
# species_combined$group <- ifelse(species_combined$species %in% species_over5000, 'a',
#                                  ifelse(species_combined$species %in% species_1000to5000, 'b', 'c'))

#plot in 3 facets according to training size so we can see small ones (>5000, 1000-5000, <1000)
species_over1000 <- unique(species_combined[species_combined$number_images >= 1000 & 
                                              species_combined$split == 'train',]$species)
species_100to1000 <- unique(species_combined[species_combined$number_images < 1000 &  
                                                species_combined$number_images >= 100 &
                                                species_combined$split == 'train',]$species)

species_combined$group <- ifelse(species_combined$species %in% species_over1000, 'a',
                                 ifelse(species_combined$species %in% species_100to1000, 'b', 'c'))


#view groups
species_combined %>% distinct(species, group) %>% print(n = 50)

#sort species by number in training data
species_order <- species_combined %>%
  group_by(species) %>%
  filter(split == 'train') %>%
  arrange(number_images) %>%
  pull(species)

#convert species to factor with ordered levels
species_combined$species <- factor(species_combined$species, levels = species_order)

#spell out splits, then order the levels
species_combined$split <- ifelse(species_combined$split == 'train', 'training',
                                 ifelse(species_combined$split == 'val', 'validation', 'test'))
species_combined$split <- factor(species_combined$split, levels = c('test','validation','training'))



## Grouped bar plots -----------------------------------------------------------

class_prop_grpA <- ggplot(species_combined[species_combined$group == 'a' & !species_combined$species %in% classes_remove,],
                          aes(x = species, y = pct, fill = split)) +
  geom_hline(yintercept = c(0.1,0.2,0.3), color = 'gray95') +
  geom_bar(stat = 'identity', position = 'dodge', ) + 
  scale_fill_brewer(palette = 'Set2') +
  # facet_grid(rows = vars(group), scales = 'free_y', space = 'free_y') +
  labs(fill = 'Split') + ylab('Proportion of dataset') +
  ylim(c(0,0.4)) +
  coord_flip() +
  geom_vline(xintercept = seq(0.5, 6, 1), color = 'gray90') +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
class_prop_grpA

class_prop_grpB <- ggplot(species_combined[species_combined$group == 'b' & !species_combined$species %in% classes_remove,], 
                           aes(x = species, y = pct, fill = split)) +
  geom_hline(yintercept = c(0.02,0.04), color = 'gray95') +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_brewer(palette = 'Set2') +
  # facet_grid(rows = vars(group), scales = 'free_y', space = 'free_y') +
  labs(fill = 'Split') + ylab('Proportion of dataset') +
  coord_flip() +
  geom_vline(xintercept = seq(0.5, 11, 1), color = 'gray90') +  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
class_prop_grpB

class_prop_grpC <- ggplot(species_combined[species_combined$group == 'c' & !species_combined$species %in% classes_remove,], 
                          aes(x = species, y = pct, fill = split)) +
  geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
  geom_bar(stat = 'identity', #position = 'dodge',
           width=1, position=position_dodge(1)) + 
  # geom_text(aes(label = round(pct,3)), vjust = 0) +
  scale_fill_brewer(palette = 'Set2') +
  # facet_grid(rows = vars(group), scales = 'free_y', space = 'free_y') +
  labs(fill = 'Split') + ylab('Proportion of dataset') +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
  # geom_text(aes(label = number_images), position = position_dodge(1), hjust = 0.1) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
class_prop_grpC

combined <- ((class_prop_grpA + theme(legend.position = 'none')) / 
               (class_prop_grpB + theme(legend.position = 'none')) / 
               (class_prop_grpC + theme(legend.position = 'inside',
                                        legend.position.inside = c(0.8,0.3)))) + 
  plot_layout(heights = c(1.5, 2.5, 5)) + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = 'bold', size = 16))
combined

ggsave('figures/images_train_val_prop_groups5.png', combined, dpi = 600, width = 5, height = 8)
  
#export CSV for S1
write.csv(species_combined, 'outputs/images_train_val_props.csv')
  
  
## Plot side-by-side -----------------------------------------------------------

#custom labels if <0.001
species_combined$label <- ifelse(species_combined$pct < 0.001 & species_combined$pct > 0, "<0.001", 
                                 ifelse(species_combined$pct == 0, '0', round(species_combined$pct, 3)))

#plot
class_nphotos_a1 <- ggplot(species_combined[species_combined$group == 'a' &
                                            !species_combined$species %in% classes_remove &
                                             species_combined$split == 'training',], 
                          aes(x = species, y = (number_images), fill = split)) +
  geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
  geom_bar(stat = 'identity', #position = 'dodge',
           width=1, position=position_dodge(1)) + 
  geom_bar(data = species_combined[species_combined$group == 'a' &
                                     !species_combined$species %in% classes_remove &
                                     species_combined$split == 'validation',],
           stat = 'identity', width = 0.5, position = position_dodge(1),) +
  scale_fill_manual(values = c('#1b9e77','#d95f02')) +
  labs(fill = 'Split') + ylab('Number of images') +
  scale_y_reverse(limits = c(10000,0), breaks = c(0,5000,10000)) +
  scale_x_discrete(position = 'top') +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
  geom_text(aes(label = round(pct,2)), position = position_dodge(1), hjust = 1.1,
            size = 3, fontface = 'italic', color = '#1b9e77') +
  # geom_text(data = species_combined[species_combined$group == 'a' &
  #                                     !species_combined$species %in% classes_remove &
  #                                     species_combined$split == 'validation',],
  #           aes(label = round(pct,3)), position = position_dodge(1), hjust = 1.1,
  #           size = 3, fontface = 'italic', color = 'white') +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.placement = 'outside',
        strip.text.y = element_text(angle = 0, size = 12))
class_nphotos_a1

class_nphotos_a2 <- ggplot(species_combined[species_combined$group == 'a' & 
                                              !species_combined$species %in% classes_remove &
                                              species_combined$split == 'test',], 
                          aes(x = species, y = (number_images), fill = split)) +
  geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
  geom_bar(stat = 'identity', #position = 'dodge',
           width=1, position=position_dodge(1)) + 
  scale_fill_manual(values = c('#7570b3')) +
  labs(fill = 'Split') + ylab('Number of images') +
  coord_flip() +
  scale_y_continuous(limits = c(0,52000), breaks = c(0,25000,50000)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
  geom_text(aes(label = round(pct,2)), position = position_dodge(1), hjust = 0,
            size = 3, fontface = 'italic', color = '#7570b3') +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(hjust = 0.5),
        strip.placement = 'outside',
        strip.text.y = element_text(angle = 0, size = 12))
class_nphotos_a2

class_nphotos_b1 <- ggplot(species_combined[species_combined$group == 'b' &
                                              !species_combined$species %in% classes_remove &
                                              species_combined$split == 'training',], 
                           aes(x = species, y = (number_images), fill = split)) +
  geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
  geom_bar(stat = 'identity', #position = 'dodge',
           width=1, position=position_dodge(1)) + 
  geom_bar(data = species_combined[species_combined$group == 'b' &
                                     !species_combined$species %in% classes_remove &
                                     species_combined$split == 'validation',],
           stat = 'identity', width = 0.5, position = position_dodge(1),) +
  scale_fill_manual(values = c('#1b9e77','#d95f02')) +
  labs(fill = 'Split') + ylab('Number of images') +
  scale_y_reverse(limits = c(1300,0), breaks = c(0,500,1000)) +
  scale_x_discrete(position = 'top') +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
  geom_text(aes(label = label), position = position_dodge(1), hjust = 1.1,
            size = 3, fontface = 'italic', color = '#1b9e77') +
  # geom_text(data = species_combined[species_combined$group == 'b' &
  #                                     !species_combined$species %in% classes_remove &
  #                                     species_combined$split == 'validation',],
  #           aes(label = round(pct,3)), position = position_dodge(1), hjust = 1.1,
  #           size = 3, fontface = 'italic', color = 'white') +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.placement = 'outside',
        strip.text.y = element_text(angle = 0, size = 12))
class_nphotos_b1

class_nphotos_b2 <- ggplot(species_combined[species_combined$group == 'b' & 
                                              !species_combined$species %in% classes_remove &
                                              species_combined$split == 'test',], 
                           aes(x = species, y = (number_images), fill = split)) +
  geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
  geom_bar(stat = 'identity', #position = 'dodge',
           width=1, position=position_dodge(1)) + 
  scale_fill_manual(values = c('#7570b3')) +
  labs(fill = 'Split') + ylab('Number of images') +
  coord_flip() +
  scale_y_continuous(limits = c(0,5000), breaks = c(0,2500,5000)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = seq(0.5, 21, 1), color = 'gray90') +
  geom_text(aes(label = label), position = position_dodge(1), hjust = 0,
            size = 3, fontface = 'italic', color = '#7570b3') +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(hjust = 0.5),
        strip.placement = 'outside',
        strip.text.y = element_text(angle = 0, size = 12))
class_nphotos_b2

class_nphotos_c1 <- ggplot(species_combined[species_combined$group == 'c' &
                                              !species_combined$species %in% classes_remove &
                                              species_combined$split == 'training',], 
                           aes(x = species, y = (number_images), fill = split)) +
  # geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
  geom_bar(stat = 'identity', #position = 'dodge',
           width=1, position=position_dodge(1)) + 
  geom_bar(data = species_combined[species_combined$group == 'c' &
                                     !species_combined$species %in% classes_remove &
                                     species_combined$split == 'validation',],
           stat = 'identity', width = 0.5, position = position_dodge(1),) +
  scale_fill_manual(values = c('#1b9e77','#d95f02')) +
  labs(fill = 'Split') + 
  coord_flip() +
  scale_y_reverse(position = 'left', limits = c(100,0), breaks = c(0,50,100)) +
  scale_x_discrete(position = 'top') +
  ylab('Number of images') +
  geom_vline(xintercept = seq(0.5, 21, 1), color = 'gray90') +
  # geom_text(aes(label = round(pct,3)), position = position_dodge(1), hjust = 1.1,
            # size = 3, fontface = 'italic', color = '#1b9e77') +
  geom_text(aes(label = label), position = position_dodge(1), hjust = 1.1,
            size = 3, fontface = 'italic', color = '#1b9e77') +
  #Validation labels:
  # geom_text(data = species_combined[species_combined$group == 'c' &
  #                                     !species_combined$species %in% classes_remove &
  #                                     species_combined$split == 'validation',],
  #           aes(label = round(pct,3)), position = position_dodge(1), hjust = 1.1,
  #           size = 3, fontface = 'italic', color = 'white') +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12),
        strip.placement = 'outside',
        strip.text.y = element_text(angle = 0, size = 12))
class_nphotos_c1

class_nphotos_c2 <- ggplot(species_combined[species_combined$group == 'c' & 
                                              !species_combined$species %in% classes_remove &
                                              species_combined$split == 'test',], 
                           aes(x = species, y = (number_images), fill = split)) +
  geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
  geom_bar(stat = 'identity', #position = 'dodge',
           width=1, position=position_dodge(1)) + 
  scale_fill_manual(values = c('#7570b3')) +
  labs(fill = 'Split') + ylab('Number of images') +
  coord_flip() +
  scale_y_continuous(limits = c(0,900), breaks = c(0,250,500,750)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
  # geom_text(aes(label = round(pct,3)), position = position_dodge(1), hjust = 0,
            # size = 3, fontface = 'italic', color = '#7570b3') +
  geom_text(aes(label = label), position = position_dodge(1), hjust = 0,
            size = 3, fontface = 'italic', color = '#7570b3') +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(hjust = 0.5),
        strip.placement = 'outside',
        strip.text.y = element_text(angle = 0, size = 12))
class_nphotos_c2

#(class_nphotos_a1 + theme(legend.position = 'none') | class_nphotos_a2) + plot_layout(widths = c(1, 1))

combined_2 <- (((class_nphotos_a1 + theme(legend.position = 'none') | class_nphotos_a2 + theme(legend.position = 'none')) + plot_layout(widths = c(1, 1))) /
  ((class_nphotos_b1 + theme(legend.position = 'none') | class_nphotos_b2 + theme(legend.position = 'none')) + plot_layout(widths = c(1, 1))) /
  ((class_nphotos_c1 + theme(legend.position = 'inside', legend.position.inside = c(0.2,0.2), legend.text = element_text(size = 12), legend.title = element_blank()) | 
      class_nphotos_c2 + theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.2), legend.text = element_text(size = 12), legend.title = element_blank())) + plot_layout(widths = c(1, 1)))) +
  # plot_layout(heights = c(1.5, 2.5, 5)) + 
  plot_layout(heights = c(2, 3.6, 3.8)) +
  plot_annotation(tag_levels = list(c('A', '', 'B', '', 'C', ''))) &
  theme(plot.tag = element_text(face = 'bold', size = 16))
combined_2

ggsave('figures/images_train_val_prop_groups_nphotos6.tif', combined_2, dpi = 600, width = 5, height = 8)


## Plot side-by-side (proportions) ---------------------------------------------
# 
# class_nphotos_a1_prop <- ggplot(species_combined[species_combined$group == 'a' &
#                                               !species_combined$species %in% classes_remove &
#                                               species_combined$split == 'training',], 
#                            aes(x = species, y = (pct), fill = split)) +
#   geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
#   geom_bar(stat = 'identity', #position = 'dodge',
#            width=1, position=position_dodge(1)) + 
#   geom_bar(data = species_combined[species_combined$group == 'a' &
#                                      !species_combined$species %in% classes_remove &
#                                      species_combined$split == 'validation',],
#            stat = 'identity', width = 0.5, position = position_dodge(1),) +
#   scale_fill_manual(values = c('#1b9e77','#d95f02')) +
#   labs(fill = 'Split') + ylab('Proportion of dataset') +
#   scale_y_reverse(limits = c(0.4,0), 
#                   breaks = c(0,0.2,0.4)
#                   ) +
#   scale_x_discrete(position = 'top') +
#   coord_flip() +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
#   geom_text(aes(label = round(number_images,2)), position = position_dodge(1), hjust = 1.1,
#             size = 3, fontface = 'italic', color = '#1b9e77') +
#   # geom_text(data = species_combined[species_combined$group == 'a' &
#   #                                     !species_combined$species %in% classes_remove &
#   #                                     species_combined$split == 'validation',],
#   #           aes(label = round(pct,3)), position = position_dodge(1), hjust = 1.1,
#   #           size = 3, fontface = 'italic', color = 'white') +
#   theme_classic() +
#   theme(axis.title.y = element_blank(),
#         axis.title = element_text(size = 12),
#         axis.text.y = element_blank(),
#         axis.text = element_text(size = 12),
#         strip.placement = 'outside',
#         strip.text.y = element_text(angle = 0, size = 12))
# class_nphotos_a1_prop
# 
# class_nphotos_a2_prop <- ggplot(species_combined[species_combined$group == 'a' & 
#                                               !species_combined$species %in% classes_remove &
#                                               species_combined$split == 'test',], 
#                            aes(x = species, y = (pct), fill = split)) +
#   geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
#   geom_bar(stat = 'identity', #position = 'dodge',
#            width=1, position=position_dodge(1)) + 
#   scale_fill_manual(values = c('#7570b3')) +
#   labs(fill = 'Split') + ylab('Proportion of dataset') +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,0.4), 
#                      breaks = c(0,0.2,0.4)
#                      ) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
#   geom_text(aes(label = round(number_images,2)), position = position_dodge(1), hjust = 0,
#             size = 3, fontface = 'italic', color = '#7570b3') +
#   theme_classic() +
#   theme(axis.title.y = element_blank(),
#         axis.title = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         axis.text.y = element_text(hjust = 0.5),
#         strip.placement = 'outside',
#         strip.text.y = element_text(angle = 0, size = 12))
# class_nphotos_a2_prop
# 
# class_nphotos_b1_prop <- ggplot(species_combined[species_combined$group == 'b' &
#                                               !species_combined$species %in% classes_remove &
#                                               species_combined$split == 'training',], 
#                            aes(x = species, y = (pct), fill = split)) +
#   geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
#   geom_bar(stat = 'identity', #position = 'dodge',
#            width=1, position=position_dodge(1)) + 
#   geom_bar(data = species_combined[species_combined$group == 'b' &
#                                      !species_combined$species %in% classes_remove &
#                                      species_combined$split == 'validation',],
#            stat = 'identity', width = 0.5, position = position_dodge(1),) +
#   scale_fill_manual(values = c('#1b9e77','#d95f02')) +
#   labs(fill = 'Split') + ylab('Proportion of dataset') +
#   scale_y_reverse(limits = c(0.05,0), 
#                   breaks = c(0,0.02,0.04)
#                   ) +
#   scale_x_discrete(position = 'top') +
#   coord_flip() +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
#   geom_text(aes(label = round(number_images,3)), position = position_dodge(1), hjust = 1.1,
#             size = 3, fontface = 'italic', color = '#1b9e77') +
#   # geom_text(data = species_combined[species_combined$group == 'b' &
#   #                                     !species_combined$species %in% classes_remove &
#   #                                     species_combined$split == 'validation',],
#   #           aes(label = round(pct,3)), position = position_dodge(1), hjust = 1.1,
#   #           size = 3, fontface = 'italic', color = 'white') +
#   theme_classic() +
#   theme(axis.title.y = element_blank(),
#         axis.title = element_text(size = 12),
#         axis.text.y = element_blank(),
#         axis.text = element_text(size = 12),
#         strip.placement = 'outside',
#         strip.text.y = element_text(angle = 0, size = 12))
# class_nphotos_b1_prop
# 
# class_nphotos_b2_prop <- ggplot(species_combined[species_combined$group == 'b' & 
#                                               !species_combined$species %in% classes_remove &
#                                               species_combined$split == 'test',], 
#                            aes(x = species, y = (pct), fill = split)) +
#   geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
#   geom_bar(stat = 'identity', #position = 'dodge',
#            width=1, position=position_dodge(1)) + 
#   scale_fill_manual(values = c('#7570b3')) +
#   labs(fill = 'Split') + ylab('Proportion of dataset') +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,0.05), 
#                      breaks = c(0,0.02,0.04)
#                      ) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_vline(xintercept = seq(0.5, 21, 1), color = 'gray90') +
#   geom_text(aes(label = round(number_images,3)), position = position_dodge(1), hjust = 0,
#             size = 3, fontface = 'italic', color = '#7570b3') +
#   theme_classic() +
#   theme(axis.title.y = element_blank(),
#         axis.title = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         axis.text.y = element_text(hjust = 0.5),
#         strip.placement = 'outside',
#         strip.text.y = element_text(angle = 0, size = 12))
# class_nphotos_b2_prop
# 
# class_nphotos_c1_prop <- ggplot(species_combined[species_combined$group == 'c' &
#                                               !species_combined$species %in% classes_remove &
#                                               species_combined$split == 'training',], 
#                            aes(x = species, y = (pct), fill = split)) +
#   # geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
#   geom_bar(stat = 'identity', #position = 'dodge',
#            width=1, position=position_dodge(1)) + 
#   geom_bar(data = species_combined[species_combined$group == 'c' &
#                                      !species_combined$species %in% classes_remove &
#                                      species_combined$split == 'validation',],
#            stat = 'identity', width = 0.5, position = position_dodge(1),) +
#   scale_fill_manual(values = c('#1b9e77','#d95f02')) +
#   labs(fill = 'Split') + 
#   coord_flip() +
#   scale_y_reverse(position = 'left', limits = c(0.007,0), 
#                   breaks = c(0,0.002,0.004)
#                   ) +
#   scale_x_discrete(position = 'top') +
#   ylab('Proportion of dataset') +
#   geom_vline(xintercept = seq(0.5, 21, 1), color = 'gray90') +
#   geom_text(aes(label = round(number_images,3)), position = position_dodge(1), hjust = 1.1,
#             size = 3, fontface = 'italic', color = '#1b9e77') +
#   # geom_text(data = species_combined[species_combined$group == 'c' &
#   #                                     !species_combined$species %in% classes_remove &
#   #                                     species_combined$split == 'validation',],
#   #           aes(label = round(pct,3)), position = position_dodge(1), hjust = 1.1,
#   #           size = 3, fontface = 'italic', color = 'white') +
#   theme_classic() +
#   theme(axis.title.y = element_blank(),
#         axis.title = element_text(size = 12),
#         axis.text.y = element_blank(),
#         axis.text = element_text(size = 12),
#         strip.placement = 'outside',
#         strip.text.y = element_text(angle = 0, size = 12))
# class_nphotos_c1_prop
# 
# class_nphotos_c2_prop <- ggplot(species_combined[species_combined$group == 'c' & 
#                                               !species_combined$species %in% classes_remove &
#                                               species_combined$split == 'test',], 
#                            aes(x = species, y = (pct), fill = split)) +
#   geom_hline(yintercept = c(0.002,0.004,0.006), color = 'gray95') +
#   geom_bar(stat = 'identity', #position = 'dodge',
#            width=1, position=position_dodge(1)) + 
#   scale_fill_manual(values = c('#7570b3')) +
#   labs(fill = 'Split') + ylab('Proportion of dataset') +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,0.007), 
#                      breaks = c(0,0.002,0.004)
#                      ) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   geom_vline(xintercept = seq(0.5, 20, 1), color = 'gray90') +
#   geom_text(aes(label = round(number_images,3)), position = position_dodge(1), hjust = 0,
#             size = 3, fontface = 'italic', color = '#7570b3') +
#   theme_classic() +
#   theme(axis.title.y = element_blank(),
#         axis.title = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         axis.text.y = element_text(hjust = 0.5),
#         strip.placement = 'outside',
#         strip.text.y = element_text(angle = 0, size = 12))
# class_nphotos_c2_prop
# 
# #(class_nphotos_a1 + theme(legend.position = 'none') | class_nphotos_a2) + plot_layout(widths = c(1, 1))
# 
# combined_2_prop <- (((class_nphotos_a1_prop + theme(legend.position = 'none') | class_nphotos_a2_prop + theme(legend.position = 'none')) + plot_layout(widths = c(1, 1))) /
#                  ((class_nphotos_b1_prop + theme(legend.position = 'none') | class_nphotos_b2_prop + theme(legend.position = 'none')) + plot_layout(widths = c(1, 1))) /
#                  ((class_nphotos_c1_prop + theme(legend.position = 'inside', legend.position.inside = c(0.3,0.2), legend.text = element_text(size = 12), legend.title = element_blank()) | 
#                      class_nphotos_c2_prop + theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.2), legend.text = element_text(size = 12), legend.title = element_blank())) + plot_layout(widths = c(1, 1)))) +
#   # plot_layout(heights = c(1.5, 2.5, 5)) + 
#   plot_layout(heights = c(2, 3.6, 3.8)) +
#   plot_annotation(tag_levels = list(c('A', '', 'B', '', 'C', ''))) &
#   theme(plot.tag = element_text(face = 'bold', size = 16))
# combined_2_prop
# 
# ggsave('figures/images_train_val_prop_groups_sidebyside.png', combined_2_prop, dpi = 600, width = 5, height = 8)


# ## Try circular (polar) bar plots ----------------------------------------------
# 
# species_proportions <- species_combined %>% filter(!species %in% classes_remove) %>%
#                               group_by(split) %>%
#                               mutate(proportion = number_images / sum(number_images)) %>%
#                               ungroup
# 
# # order levels of split
# species_proportions$split <- factor(species_proportions$split, levels = c('training','validation','test'))
# 
# # Sort species by proportion in training data
# species_order <- species_proportions %>%
#   group_by(species) %>%
#   filter(split == 'training') %>%
#   arrange(desc(number_images)) %>%
#   pull(species)
# 
# # Convert species to factor with ordered levels
# species_proportions$species <- factor(species_proportions$species, 
#                                       levels = species_order)
#   
# common_facet <- ggplot(species_proportions[species_proportions$group == 'a',], 
#                        aes(x = species, y = proportion, fill = split)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   scale_fill_viridis(discrete = TRUE) +
#   # facet_grid(~split) +
#   # coord_polar() +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 0, hjust = 1),
#     legend.position = "none",
#     panel.grid.major = element_line(color = "gray90"),
#     panel.grid.minor = element_line(color = "gray95")
#   ) 
# common_facet
# 
# rare_facet <- ggplot(species_proportions[species_proportions$group == 'b',], 
#                      aes(x = species, y = proportion, fill = split)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   scale_fill_viridis(discrete = TRUE) +
#   # facet_grid(~split) +
#   # coord_polar() +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 0, hjust = 1),
#     legend.position = "none",
#     panel.grid.major = element_line(color = "gray90"),
#     panel.grid.minor = element_line(color = "gray95")
#   )
# rare_facet
# 
# vrare_facet <- ggplot(species_proportions[species_proportions$group == 'c',], 
#                       aes(x = species, y = proportion, fill = split)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   scale_fill_viridis(discrete = TRUE) +
#   # facet_grid(~split) +
#   # coord_polar() +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 0, hjust = 1),
#     legend.position = "bottom",
#     panel.grid.major = element_line(color = "gray90"),
#     panel.grid.minor = element_line(color = "gray95")
#   ) 
# vrare_facet
# 
# circular_plot <- (common_facet + 
#                     coord_polar() + 
#                     facet_grid(~split) +
#                     theme(legend.position = 'none', axis.title = element_blank())) / 
#                  (rare_facet + 
#                     coord_polar() + 
#                     facet_grid(~split) +
#                     theme(legend.position = 'none', axis.title = element_blank())) / 
#                  (vrare_facet + 
#                     coord_polar() + 
#                     facet_grid(~split) +
#                     theme(legend.position = 'none', axis.title = element_blank()))
# circular_plot
# 
# bar_facet <- (common_facet + 
#                     facet_grid(~split) +
#                     theme(legend.position = 'none', axis.title = element_blank())) / 
#              (rare_facet + 
#                 facet_grid(~split) +
#                 theme(legend.position = 'none', axis.title = element_blank())) / 
#              (vrare_facet + 
#                 facet_grid(~split) +
#                 theme(legend.position = 'none', axis.title = element_blank()))
# bar_facet
# 
# bar_grouped <- (common_facet + 
#                 theme(legend.position = 'none', axis.title = element_blank())) / 
#               (rare_facet + 
#                  theme(legend.position = 'none', axis.title = element_blank())) / 
#               (vrare_facet + 
#                  theme(legend.position = 'none', axis.title = element_blank()))
# bar_grouped


## Which sites were in training, test, both? -----------------------------------
hex_summary$split <- 'test'; head(hex_summary)
cameraSummary$split <- 'test'; head(cameraSummary)

#read in training data hex/site summaries
train_hex <- fread('data/training_hex_summary.csv')
train_site <- fread('data/training_site_summary.csv')

#pad hex names to match
train_hex$hex <- paste(substr(train_hex$hex, 1, 1),
                       str_pad(str_sub(train_hex$hex, 2), width = 2, side = 'left', pad = '0'), sep = '')
train_site$hex <- sapply(strsplit(train_site$site, '\\-'), '[', 1)
train_site$hex <- paste(substr(train_site$hex, 1, 1),
                       str_pad(str_sub(train_site$hex, 2), width = 2, side = 'left', pad = '0'), sep = '')
train_site$site <- paste(train_site$hex,
                         sapply(strsplit(train_site$site, '-'), '[', 2), sep = '-')

#combine and compare
hex_compare <- merge(hex_summary, train_hex[train_hex$split == 'train',], all = TRUE)
site_compare <- merge(cameraSummary, train_site[train_site$split == 'train',], all = TRUE)

#remove the example ones
hex_compare <- hex_compare[!hex_compare$hex %in% c('Malawi_tags_(1).JPG','Malawi_tags.JPG'),]
site_compare <- site_compare[!site_compare$site %in% c('Malawi_tags_(1).JPG-NA', 'Malawi_tags.JPG-NA'),]

length(unique(hex_compare$hex)) #total hex
length(unique(site_compare$site)) #total sites

train_test_hex <- hex_compare %>% group_by(hex) %>% summarise(splits = paste(sort(unique(split)), collapse = ","))
(train_test_hex_sum <- train_test_hex %>% group_by(splits) %>% summarise(nhex = n()))
  #5 hex in train only, 24 hex in test only, 40 in both

train_test_site <- site_compare %>% group_by(site) %>% summarise(splits = paste(sort(unique(split)), collapse = ","))
(train_test_site_sum <- train_test_site %>% group_by(splits) %>% summarise(nsites = n()))
  #31 sites in train only, 67 sites in test only, 91 in both


## Summarize training / val split ----------------------------------------------

train_species

train_species %>% group_by(split) %>% summarise(n_photos = sum(number_images))


