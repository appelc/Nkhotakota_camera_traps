# Evaluate Malawi YOLO model 
## - Compare with MegaDetector predictions

library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)


## Read in and combine MegaDetector predictions --------------------------------
md21_dir <- 'data/megadetector/md_2021/'
md22_dir <- 'data/megadetector/md_2022/'
md23_dir <- 'data/megadetector/md_2023/'

md21_files <- list.files(md21_dir, pattern = '*.csv', full.names = TRUE)
md22_files <- list.files(md22_dir, pattern = '*.csv', full.names = TRUE)
md23_files <- list.files(md23_dir, pattern = '*.csv', full.names = TRUE)

md21_list <- lapply(md21_files, fread)
md22_list <- lapply(md22_files, fread)
md23_list <- lapply(md23_files, fread)

md21_df <- rbindlist(md21_list)
md22_df <- rbindlist(md22_list)
md23_df <- rbindlist(md23_list)

#combine all years
md_preds <- bind_rows(md21_df, md22_df, md23_df)

#remove Chipata sites
md_preds$remove <- ifelse(grepl('CHI-0', md_preds$filename), 'remove', 'keep')
md_preds <- md_preds[md_preds$remove == 'keep',]

#remove folder from filename
md_preds$filename_mod <- sapply(strsplit(md_preds$filename, '\\/'), '[', 2)

#empties are here now too
table(md_preds$class_md, useNA = 'a')

nrow(md_preds)
length(unique(md_preds$filename_mod)) #some images have multiple MD predictions (e.g., person and animal)


## Read in validated YOLO predictions ("true" classes) -------------------------
true_df <- fread('outputs/true_df.csv'); true_df <- true_df[,-'V1']

#convert all classes to 'animal', 'human', or 'empty'
true_df$class_simple <- ifelse(true_df$species_cleaned == 'empty', 'empty',
                               ifelse(true_df$species_cleaned == 'human', 'person',
                                      'animal'))
table(true_df$species_cleaned, useNA = 'a')
table(true_df$class_simple, useNA = 'a') #make sure empty and person match above

#summarize to the image-class level
true_df_sum <- true_df %>% group_by(filename, class_simple) %>% summarise(count = sum(count))

#add column for combining below
true_df$model <- 'true'
true_df_sum$model <- 'true'

 
## Read in YOLO model predictions (ALL images) ---------------------------------
preds_all <- preds_all <- fread('outputs/preds_cleaned.csv'); preds_all <- preds_all[,-'V1']

#convert all classes to 'animal', 'human', or 'empty'
preds_all$class_simple <- ifelse(preds_all$class == 'empty', 'empty',
                                 ifelse(preds_all$class == 'human', 'person',
                                 'animal'))
table(preds_all$class, useNA = 'a')
table(preds_all$class_simple, useNA = 'a')
  
#convert back to separate rows per score
preds_all <- preds_all %>% mutate(score = strsplit(scores, ',')) %>% unnest(score)
preds_all$score <- as.numeric(preds_all$score)
 
#note whether it was part of the test set
preds_all$validated <- ifelse(preds_all$filename %in% unique(true_df$filename), 'y', 'n')


## Cross-check -----------------------------------------------------------------
    
#Were all images processed with MD also processed with YOLO?
length(unique(md_preds[!md_preds$filename_mod %in% unique(preds_all$filename),]$filename_mod))

#No, 52,800 were not run through YOLO. remove for now
md_preds <- md_preds[md_preds$filename_mod %in% unique(preds_all$filename),]
  
#Were all images processed with YOLO also processed with MD?
length(unique(preds_all[!preds_all$filename %in% unique(md_preds$filename_mod),]$filename))
  
#No, 24,078 were not run thru MD. remove them for now
preds_all <- preds_all[preds_all$filename %in% unique(md_preds$filename_mod),]
  
#What about true?
nrow(true_df_sum[!true_df_sum$filename %in% unique(md_preds$filename_mod),]) 
nrow(true_df_sum[!true_df_sum$filename %in% unique(preds_all$filename),]) 

#Remove those
true_df_sum <- true_df_sum[true_df_sum$filename %in% unique(md_preds$filename_mod),]
  
nrow(preds_all[!preds_all$filename %in% true_df_sum$filename,]) #this is OK! these were run thru YOLO but not validated
nrow(md_preds[!md_preds$filename_mod %in% true_df_sum$filename,]) #this is OK! these were run thru MD but not validated

  
## Iterate through thresholds --------------------------------------------------  

thresh <- seq(0.25, 0.95, 0.1)
  
results_list <- list()
  
(start <- Sys.time()) #took about 10 minutes
for (tt in thresh){
  
  #filter YOLO preds (if prediction is below threshold, covert to 'empty')
  preds_tt <- preds_all
  preds_tt$class_tt <- ifelse(preds_tt$score < tt & !is.na(preds_tt$score), 'empty', preds_tt$class_simple) #score=NA means class='empty'
  
  #filter MD preds
  md_tt <- md_preds
  md_tt$class_tt <- ifelse(md_tt$score < tt & !is.na(md_tt$score), 'empty', md_tt$class_md)
  
  #summarize to image-level
  preds_tt_img <- preds_tt %>% group_by(filename, class_tt) %>% 
    summarise(count = n()) %>% rename(class_simple = class_tt) 
  md_tt_img <- md_tt %>% group_by(filename_mod, class_tt) %>% summarise(count = n()) %>% 
    rename(filename = filename_mod, class_simple = class_tt)
  
  #now combine into long-format df
  preds_tt_img$model <- 'yolo'
  md_tt_img$model <- 'md'
  
  all_tt <- bind_rows(true_df_sum, preds_tt_img, md_tt_img)
  
  #then summarize by image again
  all_tt_sum <- all_tt %>% group_by(filename, model) %>% summarise(class = paste(class_simple, collapse = ',')) %>% ungroup()
  
  #and convert to wide
  all_tt_img <- all_tt_sum %>% pivot_wider(names_from = model, values_from = class)
  
  #not quite perfect here... there are still multiple classes in some.
  #we'll disregard these later (e.g., "animal,human") bc there aren't that many.
  
  #clean up
  all_tt_img$md <- ifelse(all_tt_img$md == 'animal,empty', 'animal', all_tt_img$md) #animal+empty means it had another animal below thresh; just call it animal
  all_tt_img$md <- ifelse(all_tt_img$md == 'animal,empty,person', 'animal,person', all_tt_img$md) #likewise
  all_tt_img$md <- ifelse(all_tt_img$md == 'empty,person', 'person', all_tt_img$md) 
  
  all_tt_img$yolo <- ifelse(all_tt_img$yolo == 'animal,empty', 'animal', all_tt_img$yolo) 
  all_tt_img$yolo <- ifelse(all_tt_img$yolo == 'animal,empty,person', 'animal,person', all_tt_img$yolo) 
  all_tt_img$yolo <- ifelse(all_tt_img$yolo == 'empty,person', 'person', all_tt_img$yolo) 
  
  #store
  all_tt_img$thresh <- tt
  results_list[[paste('thresh', gsub('0.','',tt), sep = '_')]] <- all_tt_img
  
  print(paste('Finished threshold = ', tt, sep = ''))
  
}
(end <- Sys.time() - start)  

#Combine detections at all thresholds
results_combined <- rbindlist(results_list)  

#save
# saveRDS(results_list, 'outputs/empty_results_list.RDS')
# write.csv(results_combined, 'outputs/empty_results.csv')

  
## Create confusion matrices ---------------------------------------------------
  
## Threshold = 0.25

#read in if necessary
# results_list <- readRDS('outputs/empty_results_list.RDS')
# results_combined <- fread('outputs/empty_results.csv')

#split and expand
thresh_split <- results_list$thresh_25 %>%
        mutate(across(md:yolo, ~ strsplit(as.character(.), ","))) #split multiple classes
thresh_expanded <- thresh_split %>%
        unnest(c(md, true, yolo)) #create separate rows

#create pairwise dataframes
cm_md_true <- as.data.frame(table(thresh_expanded$md, thresh_expanded$true)) %>%
  rename(MegaDetector = Var1, True = Var2) %>% mutate(pair = 'MD vs. True') %>%
  mutate(MegaDetector = factor(MegaDetector, levels = c('empty','person','animal','vehicle')),
         True = factor(True, levels = c('animal','person','empty')))
cm_yolo_true <- as.data.frame(table(thresh_expanded$yolo, thresh_expanded$true)) %>%
  rename(NWR_model = Var1, True = Var2) %>% mutate(pair = 'YOLO vs. True') %>%
  mutate(NWR_model = factor(NWR_model, levels = c('empty','person','animal')),
         True = factor(True, levels = c('animal','person','empty')))
cm_md_yolo <- as.data.frame(table(thresh_expanded$md, thresh_expanded$yolo)) %>%
  rename(MegaDetector = Var1, NWR_model = Var2) %>% mutate(pair = 'MD vs. YOLO') %>%
  mutate(MegaDetector = factor(MegaDetector, levels = c('empty','person','animal','vehicle')),
         NWR_model = factor(NWR_model, levels = c('animal','person','empty')))

#plot
freq_range <- c(0,150000) #scale for plots A and B

md_true <- ggplot(cm_md_true[!cm_md_true$MegaDetector %in% c('vehicle','person') &
                               cm_md_true$True != 'person',], aes(x = True, y = MegaDetector)) +
  geom_tile(aes(fill = (Freq))) +
  geom_text(aes(label = Freq), size = 4) +
  scale_fill_gradient(limits = freq_range, low = 'lightblue', high = 'dodgerblue4') +
  # ggtitle('Threshold = 0.25') +
  theme_bw() + theme(
    #axis.text.x = element_text(angle = 45, hjust = 1)
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_blank())
md_true

yolo_true <- ggplot(cm_yolo_true[cm_yolo_true$NWR_model != 'person' &
                                   cm_yolo_true$True != 'person',], aes(x = True, y = NWR_model)) +
  geom_tile(aes(fill = (Freq))) +
  geom_text(aes(label = Freq), size = 4) +
  scale_fill_gradient(limits = freq_range, low = 'lightblue', high = 'dodgerblue4') +
  # ggtitle('Threshold = 0.25') +
  theme_bw() + theme(axis.text = element_text(size = 12),
                     axis.title = element_text(size = 12),
                     legend.title = element_blank())
yolo_true

md_yolo <- ggplot(cm_md_yolo[!cm_md_yolo$MegaDetector %in% c('vehicle','person') &
                               cm_md_yolo$NWR_model != 'person',], aes(x = NWR_model, y = MegaDetector)) +
  geom_tile(aes(fill = (Freq))) +
  geom_text(aes(label = Freq), size = 4) +
  # scale_fill_gradient(limits = c(0,15)) +
  scale_fill_gradient(low = 'darkseagreen1', high = 'darkseagreen4') +
  # ggtitle('Threshold = 0.25') +
  theme_bw() + theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_blank())
md_yolo

#combine and save
plots_25 <- (md_true + theme(legend.position = 'none') | yolo_true | md_yolo) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = 'bold', size = 16),
        plot.tag.position = c(0.05, 0.95))
plots_25

# ggsave('figures/confusion_matrices_thresh25.png', plots_25, dpi = 1000, width = 10, height = 3)


#Summary to report
sum(cm_md_true$Freq) #number of predictions (not images, technically)
sum(cm_yolo_true$Freq) #279,487 images

cm_md_yolo$prop <- (cm_md_yolo$Freq / sum(cm_md_yolo$Freq)) *100
cm_md_yolo #ok, all classes other than empty, person, animal comprised < 1 percent

    
## Calculate number of photos per threshold ------------------------------------
    
head(results_combined)
table(results_combined$md)
table(results_combined$yolo)

#What do NAs here mean?
table(results_combined$thresh, results_combined$md, useNA = 'a')   #no NAs for MD
table(results_combined$thresh, results_combined$yolo, useNA = 'a') #no NAs for YOLO
table(results_combined$thresh, results_combined$true, useNA = 'a') #NAs (that's OK, these weren't validated)

#Convert to wide and summarize
results_wide <- results_combined %>% pivot_longer(cols = c(md, true, yolo), names_to = 'model', values_to = 'class') 

#Add column for test set and then summarize
results_wide$validated <- preds_all$validated[match(results_wide$filename, preds_all$filename)]
results_sum <- results_wide %>% group_by(thresh, model, class, validated) %>% 
                    summarise(n_images = length(unique(filename))) #or n_images = n() ## one with validated grouping
results_sum_all <- results_wide %>% group_by(thresh, model, class) %>% 
                    summarise(n_images = length(unique(filename))) #or n_images = n() ## one with all together
results_sum_all$validated <- 'all'
results_sum <- bind_rows(results_sum_all, results_sum)

#note whether it was part of the test set
preds_all$validated <- ifelse(preds_all$filename %in% unique(true_df$filename), 'y', 'n')
  length(unique(preds_all[preds_all$validated == 'y',]$filename)) #278,282 (matches above)

  
## To summarize and report -----------------------------------------------------
cm_md_true
sum(cm_md_true[cm_md_true$MegaDetector == 'animal',]$Freq) #animals predicted by MD at 0.25

results_sum 
results_sum[results_sum$model == 'md' & results_sum$thresh == 0.25,] #animals predicted by MD at 0.25 (same on plot)
results_sum[results_sum$model == 'yolo' & results_sum$thresh == 0.25,] #animals predicted by YOLO at 0.25 (same on plot)

cm_md_true
#and how many in validation set?


## How many were missed at a few different thresholds?
#0.25
length(unique(results_combined[results_combined$thresh == 0.25 & 
                               results_combined$true == 'animal' & results_combined$md == 'empty',]$filename)) #MD missed 2491
length(unique(results_combined[results_combined$thresh == 0.25 & 
                               results_combined$true == 'animal' & results_combined$yolo == 'empty',]$filename)) #YOLO missed 1187

#0.35
length(unique(results_combined[results_combined$thresh == 0.35 & 
                               results_combined$true == 'animal' & results_combined$md == 'empty',]$filename)) #MD missed 3274
length(unique(results_combined[results_combined$thresh == 0.35 & 
                               results_combined$true == 'animal' & results_combined$yolo == 'empty',]$filename)) #YOLO missed 3454

#0.45
length(unique(results_combined[results_combined$thresh == 0.45 & 
                               results_combined$true == 'animal' & results_combined$md == 'empty',]$filename)) #MD missed 4252
length(unique(results_combined[results_combined$thresh == 0.45 & 
                               results_combined$true == 'animal' & results_combined$yolo == 'empty',]$filename)) #YOLO missed 6202


## Which species were missed in the test set?

#0.25
md_missed_25 <- sort(unique(results_combined[results_combined$thresh == 0.25 &  #get the filenames
                                             results_combined$true == 'animal' & results_combined$md == 'empty',]$filename))
yolo_missed_25 <- sort(unique(results_combined[results_combined$thresh == 0.25 &  #get the filenames
                                             results_combined$true == 'animal' & results_combined$yolo == 'empty',]$filename))

md_missed_25_preds <- true_df[true_df$filename %in% md_missed_25,]$species_cleaned 
sort(table(md_missed_25_preds)) #and find the species classes

yolo_missed_25_preds <- true_df[true_df$filename %in% yolo_missed_25,]$species_cleaned 
table(yolo_missed_25_preds) #and find the species classes


#0.45
md_missed_45 <- sort(unique(results_combined[results_combined$thresh == 0.45 &  #get the filenames
                                             results_combined$true == 'animal' & results_combined$md == 'empty',]$filename))
yolo_missed_45 <- sort(unique(results_combined[results_combined$thresh == 0.45 &  #get the filenames
                                               results_combined$true == 'animal' & results_combined$yolo == 'empty',]$filename))

md_missed_45_preds <- true_df[true_df$filename %in% md_missed_45,]$species_cleaned 
sort(table(md_missed_45_preds)) #and find the species classes

yolo_missed_45_preds <- true_df[true_df$filename %in% yolo_missed_45,]$species_cleaned 
table(yolo_missed_45_preds) #and find the species classes


## In proportion to their availability?    
head(true_df)
test_n_df <- data.frame(table(true_df$species_cleaned))

#Megadetector
# md_missed_df <- data.frame(table(md_missed_25_preds)); colnames(md_missed_df) <- c('class','md_missed')
md_missed_df <- data.frame(table(md_missed_45_preds)); colnames(md_missed_df) <- c('class','md_missed')

#YOLO
# yolo_missed_df <- data.frame(table(yolo_missed_25_preds)); colnames(yolo_missed_df) <- c('class','yolo_missed')
yolo_missed_df <- data.frame(table(yolo_missed_45_preds)); colnames(yolo_missed_df) <- c('class','yolo_missed')

#combine and add number in test dataset
missed_df <- merge(md_missed_df, yolo_missed_df, all = TRUE)
missed_df$test_n <- test_n_df$Freq[match(missed_df$class, test_n_df$Var1)]

#calculate proportions
missed_df$prop_missed_md <- missed_df$md_missed / missed_df$test_n  
missed_df$prop_missed_yolo <- missed_df$yolo_missed / missed_df$test_n  

  missed_df[order(missed_df$prop_missed_md),] #highest % missed by MD: bat, bushbaby, hyena, leopard, honey badger, porcupine
  missed_df[order(missed_df$prop_missed_yolo),] #highest % missed by YOLO: bat, bushbaby, other_bird, genet, mongoose, squirrel, leopard, jackal
  

## Calculate sensitivity/specificity -------------------------------------------    
    
#Read back in
results_combined <- fread('outputs/empty_results.csv')  
head(results_combined)

#Filter to test set only
results_combined$test_set <- ifelse(results_combined$filename %in% unique(true_df$filename), 'y', 'n')
table(results_combined$test_set, useNA = 'a')  
test_set_results <- results_combined[results_combined$test_set == 'y',]

#Calculate TP, TN, FP, FN
test_set_results$match_md_animal <- ifelse(grepl('animal', test_set_results$true), #does the image have an animal?
                                         ifelse(grepl('animal', test_set_results$md), #YES it has an animal. Did MD predict an animal?
                                                'TP', 'FN'), #TP if so, FN if not
                                         ifelse(grepl('animal', test_set_results$md), #NO it doesn't have an animal. Did MD predict an animal?
                                                'FP', 'TN')) #FP if so, TN if not

test_set_results$match_yolo_animal <- ifelse(grepl('animal', test_set_results$true), #does the image have an animal?
                                         ifelse(grepl('animal', test_set_results$yolo), #YES it has an animal. Did YOLO predict an animal?
                                                'TP', 'FN'), #TP if so, FN if not
                                         ifelse(grepl('animal', test_set_results$yolo), #NO it doesn't have an animal. Did YOLO predict an animal?
                                                'FP', 'TN')) #FP if so, TN if not
#review results
table(test_set_results$match_md_animal, useNA = 'a')
table(test_set_results$match_yolo_animal, useNA = 'a')

#inspect some examples
head(test_set_results[test_set_results$match_md_animal == 'TP',])
head(test_set_results[test_set_results$match_md_animal == 'FP',])
head(test_set_results[test_set_results$match_md_animal == 'FN',])
head(test_set_results[test_set_results$match_md_animal == 'TN',])
  
#summarise
length(unique(test_set_results$filename)) #images in test set
length(unique(test_set_results[grepl('animal', test_set_results$true)]$filename)) #truly had animals

#calculate metrics by threshold
matches_md <- test_set_results %>% group_by(thresh, match_md_animal) %>% summarise(n = n())
matches_yolo <- test_set_results %>% group_by(thresh, match_yolo_animal) %>% summarise(n = n())

matches_md_wide <- matches_md %>% group_by(thresh) %>% pivot_wider(names_from = match_md_animal, values_from = n)  
matches_yolo_wide <- matches_yolo %>% group_by(thresh) %>% pivot_wider(names_from = match_yolo_animal, values_from = n)  

sens_spec_md <- matches_md_wide %>% summarise(Sensitivity = (TP / (TP + FN)),
                                              Specificity = (TN / (TN + FP))) %>% mutate(Model = 'MegaDetector')
sens_spec_yolo <- matches_yolo_wide %>% summarise(Sensitivity = (TP / (TP + FN)),
                                                  Specificity = (TN / (TN + FP))) %>% mutate(Model = 'NWR_model')

#merge and convert to long
sens_spec <- bind_rows(sens_spec_md, sens_spec_yolo)
sens_spec_long <- sens_spec %>% pivot_longer(cols = c(Sensitivity, Specificity), names_to = 'metric', values_to = 'value')

#plot
sens_spec_plot <- ggplot(sens_spec_long, aes(x = thresh, y = value, color = Model)) +
  geom_line(lwd = 1.6, aes(lty = Model)) +
  # geom_point(aes(color = Model), size = 2) +
  xlim(c(0.25, 1.0)) + ylim(c(0,1)) +
  scale_color_manual(values = c('MegaDetector' = 'gray70', 'NWR_model' = 'black')) +
  xlab('Threshold') + ylab('Value') +
  facet_wrap(~metric) +
  theme_bw() + theme(axis.text = element_text(size = 14),
                     axis.title = element_text(size = 16),
                     # axis.title.y = element_blank(),
                     strip.text = element_text(size = 16),
                     legend.position = 'inside',
                     legend.background = element_rect(fill = 'transparent'),
                     legend.title = element_text(size = 16, face = 'bold'),
                     legend.justification = c(0.9, 0.3),
                     legend.text = element_text(size = 14))
sens_spec_plot    

#Combine with confusion matrices
figure_6 <- ((md_true + theme(legend.position = 'none') | yolo_true | md_yolo) / sens_spec_plot) + 
  plot_layout(heights = c(1,3)) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = 'bold', size = 16),
        plot.tag.position = c(0.05, 0.95))
figure_6

# ggsave('figures/confusion_matrices_thresh25_with_sens_spec_2.png', figure_6, dpi = 1000, width = 10, height = 8)
  
    