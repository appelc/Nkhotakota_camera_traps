# Summarize data for training, validation, and test splits


library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)


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
cameraSummary <- tags_reviewed %>% group_by(site) %>% summarise(n_photos = n(),
                                                          n_checks = length(unique(site_check)))
summary(cameraSummary$n_photos) #mean photos per camera
table(cameraSummary$n_checks, useNA = 'a') #number of checks each

#create hex summary
hex_summary <- tags_reviewed %>% group_by(hex) %>% summarise(n_photos = n())
summary(hex_summary$n_photos) #mean photos per hex

#create species summary
species_summary <- tags_reviewed %>% group_by(species_cleaned) %>% 
                                    summarise(n_photos = n(),
                                              n_hex = length(unique(hex)),
                                              n_sites = length(unique(site)))
  
  
## Compare distribution of classes in evaluation set vs training set -----------

#read in training data species summary
train_species <- fread('data/training_classes_summary.csv')
  
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

 
## Plot for Appendix 1 ---------------------------------------------------------

#remove some classes not included in model training
classes_remove <- c('palm_civet','other_animal','other_bird','small_mammal','empty')

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
tot_eval <- sum(species_combined[species_combined$split == 'test' & species_combined$species != 'empty',]$number_images) #total eval images not counting empty
tot_train <- sum(species_combined[species_combined$split == 'train' &  species_combined$species != 'empty',]$number_images) #total train images not counting empty
tot_val <- sum(species_combined[species_combined$split == 'val' &  species_combined$species != 'empty',]$number_images) #total train images not counting empty
species_combined$pct <- ifelse(species_combined$split == 'test', species_combined$number_images / tot_eval,
                               ifelse(species_combined$split == 'train', species_combined$number_images / tot_train, 
                                      species_combined$number_images / tot_val))
  
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
  
#plot in 2 facets according to size so we can see small ones (>5000 and <5000)
species_over5000 <- unique(species_combined[species_combined$number_images > 5000 & species_combined$split == 'test',]$species)
species_combined$group <- ifelse(species_combined$species %in% species_over5000, 'high','low')
  
class_prop_grp1 <- ggplot(species_combined[species_combined$group == 'high' & !species_combined$species %in% classes_remove,],
                          aes(x = reorder(species, pct), y = pct, fill = split)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_brewer(palette = 'Set2') +
  # facet_grid(rows = vars(group), scales = 'free_y', space = 'free_y') +
  labs(fill = 'Split') + ylab('Proportion of dataset') +
  coord_flip() +
  theme_bw() + theme(axis.title.y = element_blank())
class_prop_grp1

class_prop_grp2 <- ggplot(species_combined[species_combined$group == 'low' & !species_combined$species %in% classes_remove,], 
                           aes(x = reorder(species, pct), y = pct, fill = split)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_brewer(palette = 'Set2') +
  # facet_grid(rows = vars(group), scales = 'free_y', space = 'free_y') +
  labs(fill = 'Split') + ylab('Proportion of dataset') +
  coord_flip() +
  theme_bw() + theme(axis.title.y = element_blank())
class_prop_grp2

combined <- (class_prop_grp1 / class_prop_grp2) + plot_layout(heights = c(1, 5)) + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = 'bold', size = 16))
combined
#ggsave('figures/images_train_val_prop_groups.png', combined, dpi = 600, width = 5, height = 7)
  
#export CSV for S1
write.csv(species_combined, 'outputs/images_train_val_props.csv')
  
  
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


