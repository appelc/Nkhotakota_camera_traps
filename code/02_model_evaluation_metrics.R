# Evaluate Malawi YOLO model 
## - Compute model performance metrics
## - Linear model for metrics ~ training data size
## - Create plots

library(data.table)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(rphylopic)
library(patchwork)

## Read in data ----------------------------------------------------------------
true_df <- fread('outputs/true_df.csv')
preds_df <- fread('outputs/preds_df.csv')

#create list of all species tags
(species = sort(unique(c(true_df$species_cleaned, preds_df$species_cleaned))))
  
#create temporary list for storing results
sp_matches_list <- list()
  
#find and store "true" tags and "predicted" tags for each species
true_sp_list <- lapply(species, function(sp) true_df[true_df$species_cleaned == sp,])
preds_sp_list <- lapply(species, function(sp) preds_df[preds_df$species_cleaned == sp,])


## Calculate true/pred matches per species -------------------------------------

#Loop
  # length(species)
  (sp = seq_along(species)[41]) #change here: 1-41
  (ss <- species[sp])
  start <- Sys.time()
  
  #set up
  true_sp <- true_sp_list[[sp]] #extract true tags for this species
  preds_sp <- preds_sp_list[[sp]] #extract predicted tags for this species

  #first find TP and FN for this species
  sp_results <- list()
  for (ii in unique(true_sp$filename)){ #for each file,
    if(nrow(preds_df[preds_df$filename == ii,]) == 1){ #does this image have only 1 predicted species?
      
      pred_sp_ii <- preds_df[preds_df$filename == ii,]$species_cleaned #find species prediction for this image
      pred_score_ii <- preds_df[preds_df$filename == ii,]$max_score #and prediction score(s)
      pred_count_ii <- ifelse(pred_sp_ii == ss, preds_df[preds_df$filename == ii,]$count, 0) #and predicted count (IF species matches)
      true_count_ii <- true_df[true_df$filename == ii,]$count #and true count
      match_ii <- ifelse(pred_sp_ii == ss, #if the predicted species matches the reference species,
                         'TP', #it's a true positive
                         'FN') #otherwise, it's a false negative (for this reference species)
      
      #store results
      sp_results[[length(sp_results) + 1]] <- data.table(ii, ss, ss, pred_sp_ii, pred_score_ii,  match_ii, true_count_ii, pred_count_ii)
      
    } else { #if the image has >1 predicted species,
      
      pred_sp_ii <- preds_df[preds_df$filename == ii,]$species_cleaned #find all species predictions for this image
      match_ii <- ifelse(any(pred_sp_ii == ss), #if (any of the) predicted species matches the reference species,
                         'TP', #it's a true positive
                         'FN') #otherwise, it's a false negative (for this reference species)
      
      #if the ref class is one of the predictions, get the corresponding score and count
      pred_score_ii <- ifelse(match_ii == 'TP', preds_df[preds_df$filename == ii & preds_df$species_cleaned == ss,]$max_score, NA)
      pred_count_ii <- ifelse(match_ii == 'TP', preds_df[preds_df$filename == ii & preds_df$species_cleaned == ss,]$count, NA) #could use #animals found here instead of NA but would have to add 'count' for each of the pred species
      true_count_ii <- true_df[true_df$filename == ii & true_df$species_cleaned == ss,]$count #and get the true count for this filename/species match
      pred_sp_name <- ifelse(match_ii == 'TP', ss, 'multiple') 
      
      #store results
      sp_results[[length(sp_results) + 1]] <- data.table(ii, ss, ss, pred_sp_name, pred_score_ii, match_ii, true_count_ii, pred_count_ii)
      
    } #end "if >1 sp" loop 
  } #end finding TP and FN
  
  #now find FP for this species
  preds_sp_extra <- preds_sp[!preds_sp$filename %in% unique(true_sp$filename),] #keep only photos that we haven't already considered
  for (ff in unique(preds_sp_extra$filename)){ #for each of these images,
    true_sp_ff <- true_df[true_df$filename == ff,]$species_cleaned #find the true species
    pred_score_ff <- preds_sp[preds_sp$filename == ff,]$max_score #and prediction score
    
    #store results
    sp_results[[length(sp_results) + 1]] <- data.table(ff, ss, true_sp_ff, ss, pred_score_ff, 'FP', NA, NA)
    
  } #end looping though additional "predicted" photos
  
  Sys.time() - start
  
  #store for this species
  sp_combined <- rbindlist(sp_results, use.names = FALSE)
  sp_matches_list[[ss]] <- sp_combined
  names(sp_matches_list)
  
  #combine all species into table
  sp_matches <- rbindlist(sp_matches_list, use.names = FALSE)
  table(sp_matches$ss)
  
  #at the end... rewrite column names:
  colnames(sp_matches) <- c('filename','class_ref','class_true','class_pred','score','match','count_true','count_pred')
  
  #save
  write.csv(sp_matches, 'outputs/image_matches.csv')
  saveRDS(sp_matches, 'outputs/image_matches.RDS')
  saveRDS(sp_matches_list, 'outputs/image_matches_list.RDS')

  dir.create('outputs/results/')
  sp_filename <- paste('outputs/results/', 'image_matches_list_', ss, '.RDS', sep = '')
  saveRDS(sp_results, sp_filename)
  
  #now increment 'sp' by 1 and re-run through here


## Now iterate through thresholds ----------------------------------------------

  #read back in if necessary
  sp_matches <- fread('outputs/image_matches.csv')
  
  #create threshold sequences
  thresh <- seq(0.25, 0.95, 0.05)
  
  #create empty objects to store results
  results_list <- list()
  metrics <- NULL
  
  #iterate through
  for (cc in unique(sp_matches$class_ref)){ #for each species as reference,
    sp_matches_cc <- sp_matches[sp_matches$class_ref == cc,] #find those rows
    
    list_tt <- list() #initialize list to store results
    for (tt in thresh){ #for each threshold,
      
      #do preds count as detections?
      sp_matches_cc$pred_result <- ifelse(sp_matches_cc$score >= tt, #for rows with score > threshold,
                                          cc,    #count those as detections of the species
                                          'empty') #otherwise, they don't count as detections
      
      #assign TP, FP, FN
      sp_matches_cc$result <- ifelse(sp_matches_cc$class_true == cc, #if true class matches the reference,
                                     ifelse(sp_matches_cc$pred_result == cc, 'TP', #it's either a TP (pred matches ref)
                                            'FN'), #or FN (pred does not match ref bc it's now empty)
                                     ifelse(sp_matches_cc$pred_result == cc, 'FP', #if true class is something else, was ref species prediction > threshold? if so, FP. 
                                            'TN')) #if not, FN.
      sp_matches_cc$thresh <- tt
      
      #does not work with "class_pred" = multiple! bc there are no score values for those.
      #could use max score... would have to add that into for loop above.
      #just keep track of how many we are excluding for each.
      
      #tally TP, FP, FN
      tp <- nrow(sp_matches_cc[sp_matches_cc$result %in% 'TP',])
      fp <- nrow(sp_matches_cc[sp_matches_cc$result %in% 'FP',])
      fn <- nrow(sp_matches_cc[sp_matches_cc$result %in% 'FN',])
      tn <- nrow(sp_matches_cc[sp_matches_cc$result %in% 'TN',]) #this isn't all of them, though. just ones that *were* FP above a different threshold.
      mult_preds <- nrow(sp_matches_cc[sp_matches_cc$class_pred %in% 'multiple',]) #should be the same as result = NA
      
      #calculate precision, recall, F1
      prec_cc_tt <- tp / (tp + fp)
      rec_cc_tt <- tp / (tp + fn)
      f1_cc_tt <- (2 * prec_cc_tt * rec_cc_tt) / (prec_cc_tt + rec_cc_tt)
      
      #store metrics
      metrics_cc_tt <- data.frame('species' = cc, 'threshold' = tt, 'precision' = prec_cc_tt, 'recall' = rec_cc_tt, 'F1' = f1_cc_tt,
                                  'TP' = tp, 'FP' = fp, 'FN' = fn, 'TN' = tn, 'mult_preds' = mult_preds)
      metrics <- rbind(metrics, metrics_cc_tt)
      
      #store dataframe for thresh
      list_tt[[paste0('thresh_', gsub('0.','',tt))]] <- sp_matches_cc
    }
    
    #store dataframe for sp
    results_list[[cc]] <- list_tt
  }
  
  names(results_list)
  names(results_list$aardvark)
  head(results_list$aardvark$thresh_25)
  head(metrics)
  
  #save
  write.csv(metrics, 'outputs/metrics.csv')  
  saveRDS(results_list, 'outputs/threshold_metrics.RDS')
    
  
## Calculate average and macro-averaged precision/recall/F1 -----------------------------
  
  #read in if necessary
  metrics <- fread('outputs/metrics.csv')

  #calculate averages per species
  avg_metrics <- metrics %>% group_by(species) %>% summarize(AP = mean(precision, na.rm = TRUE),
                                                             AR = mean(recall, na.rm = TRUE),
                                                             AF1 = mean(F1, na.rm = TRUE))
  #for Table 1
  avg_metrics
  # write.csv(avg_metrics, 'outputs/metrics_avg.csv')
  
  #list species to include for macro-averages (remove ones not in training/eval, with <10, etc)
  (sp_for_macro <- unique(metrics[!metrics$species %in% c('bat_sp.','domestic_cattle','goat',
                                                          'other_animal','other_bird','palm_civet',
                                                          'side-striped_jackal','small_mammal'),]$species))
  length(sp_for_macro_no_empty) #using 32 classes
  
  #save species list (w/o empty)
  # write.csv(sp_for_macro[sp_for_macro != 'empty'], 'outputs/species_for_metrics.csv')
  
    
## Plot precision/recall/F1 ----------------------------------------------------
  
  #read in if necessary
  metrics <- fread('outputs/metrics.csv')
  avg_metrics <- fread('outputs/metrics_avg.csv')
  sp_for_metrics <- fread('outputs/species_for_metrics.csv')
      
  #filter to only keep species included in macro calculations above (and also not 'empty')
  metrics_filtered <- metrics %>% filter(species %in% sp_for_metrics$V2)
    
  #change some class names
  metrics_filtered$species_cleaned <- ifelse(metrics_filtered$species == 'guinea_fowl', 'guineafowl', metrics_filtered$species)
  metrics_filtered$species_cleaned <- ifelse(metrics_filtered$species == 'bush_squirrel', 'squirrel', metrics_filtered$species_cleaned)
  metrics_filtered$species_cleaned <- ifelse(metrics_filtered$species == 'scrub_hare', 'savanna_hare', metrics_filtered$species_cleaned)
  metrics_filtered$species_cleaned <- gsub('_', ' ', metrics_filtered$species_cleaned)
  
  #list phylopic IDs
  phylopic_ids <- data.frame('species_cleaned' = sort(unique(metrics_filtered$species_cleaned)),
                             'id' = c('cfee2dca-3767-46b8-8d03-bd8f46e79e9e','72f2f854-f3cd-4666-887c-35d5c256ab0f',
                                      '65c4a9b3-dcde-4f0f-9a1f-8d71e74be9ec','7fb9bea8-e758-4986-afb2-95a2c3bf983d',
                                      # '540d58bc-d82f-4cfc-a63b-2c37d083edec','3d8acaf6-4355-491e-8e86-4a411b53b98b', #lesser kudu and sus scrofa here
                                      '540d58bc-d82f-4cfc-a63b-2c37d083edec','565c4a96-c80d-4ee3-86df-6f6c58ba2338', #lesser kudu and red river hog here
                                      '7f4984bb-65ba-4f77-a92d-96e2c5dc65f0','6f3ebbc6-be53-4216-b45b-946f7984669b',
                                      '7db171af-ac7a-4859-9c2b-66488a5a5c95','62398ac0-f0c3-48f8-8455-53512a05fbc4',
                                      '4f49e5d0-f41e-427c-a1ab-9ec51e1bd72b','a8018e40-5257-4ad5-a42e-6fbad115deab',
                                      'f7200232-d635-4d79-bcaa-a59867376d09','8952c22d-1d0b-4658-b8ff-348df8aff18f',
                                      '880baa26-af89-44ad-ae07-e92b84e3ff05','49b72776-1f99-442b-a084-65f402cdbd69',
                                      'e07d1491-1d85-4c47-9f7d-075ea57bf0c5','f25267a2-90b4-4ca7-af95-a425f3564bbe',
                                      '5e900306-3a5e-4193-8a94-74cc765f0aaf','a0cde6a8-4a47-4562-827f-90bd8d7bdd05',
                                      'cf85f532-c0c2-40ba-ae83-47846caf1514','bb357997-4d4d-4e73-b853-d95a7a47ec68',
                                      'a363eadc-1b1e-41fb-ba8c-b45cb405a850','a6c8cc22-c035-48e1-bd2a-98a29446b392', #actually both roan
                                      '8930c815-0a3e-4b77-a2da-e7273bccddd7','01895510-7c27-489d-b441-6f72b1f562d2',
                                      'c9aaafb0-bbab-43b7-b2a8-077ae0799b96','9303fc52-3da1-4d42-9e6d-665c21791df8', #diff squirrel sp
                                      '9ebe32a3-3870-4073-9992-998dafe98014','d594a0c4-2708-4cde-ba41-08fde8b1184f',
                                      'f93103f1-e2a0-4c73-b274-c7b51afe4db0','81caf94e-5cbe-4e5e-8101-545abea2bfc0'),
                             'size_scaling' = c(0.3,0.3,
                                                0.3,0.2,
                                                0.3,0.3,
                                                0.3,0.3,
                                                0.4,0.4,
                                                0.2,0.3,
                                                0.2,0.2,
                                                0.4,0.3,
                                                0.3,0.4,
                                                0.2,0.2,
                                                0.3,0.3,
                                                0.4,0.4,
                                                0.2,0.3,
                                                0.3,0.3,
                                                0.3,0.3,
                                                0.5,0.4))
  
  #merge phylopic IDs with data
  metrics_filtered <- metrics_filtered %>% left_join(phylopic_ids, by = 'species_cleaned')
  
  #plot (FIGURE 5)
  metrics_grid <- ggplot(metrics_filtered, aes(x = threshold, y = precision, color = 'Precision')) +
    geom_line(lwd = 1.2) +
    geom_line(aes(y = F1, color = 'F1'), lty = 'dashed', lwd = 1.2) +
    geom_line(aes(y = recall, color = 'Recall'), lwd = 1.2) +
    # geom_phylopic(aes(x = 0.6, y = 0.2, uuid = id), size = 0.3, fill = 'black', color = 'black') +
    geom_phylopic(aes(x = 0.6, y = 0.2, uuid = id, height = size_scaling), fill = 'black', color = NA) +
    scale_height_continuous(range = c(0.2,0.4)) +
    scale_color_manual(values = c('Precision' = '#E69F00', 'Recall' = '#009E73', 'F1' = '#56B4E9')) +
    ylim(c(0,1)) + xlim(c(0.25,1)) +
    xlab('Confidence score') +
    facet_wrap(~species_cleaned, nrow=8, scales = 'free') +
    theme_bw() + theme(legend.position = 'top',
                       legend.justification = 'center',
                       legend.margin = margin(5,5,5,5),
                       strip.background = element_blank(),
                       strip.placement = 'outside',
                       strip.text = element_text(size = 11, face = 'bold'), 
                       legend.text = element_text(size = 12),
                       legend.title = element_blank(),
                       axis.title = element_text(size = 12),
                       axis.title.y = element_blank(),
                       legend.background = element_rect(color = 'gray'))
  metrics_grid
  ggsave('figures/grid_metrics_5.png', metrics_grid, dpi = 1000, width = 8, height = 12)
  
  #Some summaries to report:
  head(metrics_filtered)

    #Recall
    sort(unique(metrics_filtered[metrics_filtered$recall > 0.8 & metrics_filtered$threshold >= 0.9,]$species_cleaned))
      #these sp had high recall even at high thresholds    
  
    #Precision
    sort(unique(metrics_filtered[metrics_filtered$precision > 0.8 & metrics_filtered$threshold >= 0.9,]$species_cleaned))
      #these sp had high precision at high thresholds 
    sort(unique(metrics_filtered[metrics_filtered$precision <= 0.8 & metrics_filtered$threshold > 0.9,]$species))
      #these sp did not
    
      
## Plot AR, AP by training size  -----------------------------------------------
      
  #read in training data species summary
  train_species <- fread('data/training_species_summary.csv')
  train_species <- train_species[train_species$split == 'train',]
  
  #filter for only the ones used in macro calculations (not strictly necessary here)
  avg_metrics_filtered <- avg_metrics %>% filter(species %in% sp_for_metrics$V2)
  
  #merge with training data size
  head(avg_metrics_filtered)
  avg_metrics_filtered$train_images <- train_species$number_images[match(avg_metrics_filtered$species, train_species$species)]
  avg_metrics_filtered %>% distinct(species, train_images)
  
  #convert to long for plotting
  avg_metrics_long_filtered <- avg_metrics_filtered %>% pivot_longer(cols = starts_with('A'), values_to = 'value')
  avg_metrics_long_filtered$metric <- factor(avg_metrics_long_filtered$name, levels = c('AP','AR','AF1'))
  
  #run linear models and save as dataframe
  ap_train <- lm(AP ~ train_images, avg_metrics_filtered)
  ar_train <- lm(AR ~ train_images, avg_metrics_filtered)
  f1_train <- lm(AF1 ~ train_images, avg_metrics_filtered)
  
  #now do 'log(train images)' as per Shahinfar, Tabak, papers
  ap_train_log <- lm(AP ~ log10(train_images), avg_metrics_filtered) 
  ar_train_log <- lm(AR ~ log10(train_images), avg_metrics_filtered)
  f1_train_log <- lm(AF1 ~ log10(train_images), avg_metrics_filtered)
  
  metrics_lm <- data.frame('metric' = factor(c('AP','AR','AF1'), levels = c('AP','AR','AF1')),
                           'slope' = c(ap_train$coefficients[2], ar_train$coefficients[2], f1_train$coefficients[2]),
                           'R2' = c(summary(ap_train)$r.squared, summary(ar_train)$r.squared, summary(f1_train)$r.squared))
  
  metrics_lm_log <- data.frame('metric' = factor(c('AP','AR','AF1'), levels = c('AP','AR','AF1')),
                               'slope' = c(ap_train_log$coefficients[2], ar_train_log$coefficients[2], f1_train_log$coefficients[2]),
                               'R2' = c(summary(ap_train_log)$r.squared, summary(ar_train_log)$r.squared, summary(f1_train_log)$r.squared))
  
  #expand metric names for facet labels
  avg_metrics_long_filtered$metric_name <- ifelse(avg_metrics_long_filtered$name == 'AP', 'Precision',
                                                ifelse(avg_metrics_long_filtered$name == 'AR', 'Recall',
                                                       ifelse(avg_metrics_long_filtered$name == 'AF1', 'F1',
                                                              avg_metrics_long_filtered$name)))
  avg_metrics_long_filtered$metric_name <- factor(avg_metrics_long_filtered$metric_name,
                                                  levels = c('Precision','Recall','F1'))
  
  metrics_lm$metric_name <- ifelse(metrics_lm$metric == 'AP', 'Precision',
                                   ifelse(metrics_lm$metric == 'AR', 'Recall',
                                          ifelse(metrics_lm$metric == 'AF1', 'F1',
                                                 metrics_lm$metric)))
  metrics_lm$metric_name <- factor(metrics_lm$metric_name, levels = c('Precision','Recall','F1'))
  
  metrics_lm_log$metric_name <- ifelse(metrics_lm_log$metric == 'AP', 'Precision',
                                   ifelse(metrics_lm_log$metric == 'AR', 'Recall',
                                          ifelse(metrics_lm_log$metric == 'AF1', 'F1',
                                                 metrics_lm_log$metric)))
  metrics_lm_log$metric_name <- factor(metrics_lm_log$metric_name, levels = c('Precision','Recall','F1'))
  
  #plot with regression line
  metrics_vs_training <- ggplot(avg_metrics_long_filtered, aes(x = train_images, y = value)) +
    geom_point(alpha = 0.6, color = 'dodgerblue4') +
    geom_smooth(stat = 'smooth', method = 'lm', color = 'dodgerblue4', fill = 'lightblue') +
    geom_text(data = metrics_lm, aes(label = paste('Slope =', round(slope,5))),
              x = 1000, y = 1.25, hjust = 0, vjust = 1, size = 4, color = 'black', fontface = 'italic') +
    geom_text(data = metrics_lm, aes(label = paste('R² =', round(R2,3))),
              x = 1000, y = 1.15, hjust = 0, vjust = 1, size = 4, color = 'black', fontface = 'italic') +
    geom_text_repel(data = avg_metrics_long_filtered[avg_metrics_long_filtered$species %in% c('elephant','baboon','zebra','leopard'),], 
              #aes(label = round(value,2)), 
              aes(label = species),
              # vjust = -0.5, hjust = 0, 
              size = 4) +
    ylim(c(0,1.26)) + 
    xlab('Number of training images') + 
    facet_grid(~metric_name) +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          axis.title.y = element_blank())
  metrics_vs_training
  
  # ggsave('figures/metrics_vs_training.png', metrics_vs_training, dpi = 1000, width = 8, height = 4)

  #plot with log(training) regression line
  metrics_vs_training_log <- ggplot(avg_metrics_long_filtered, aes(x = log10(train_images), y = value)) +
    geom_point(alpha = 0.6, color = 'dodgerblue4') +
    geom_smooth(stat = 'smooth', method = 'lm', color = 'dodgerblue4', fill = 'lightblue') +
    geom_text(data = metrics_lm_log, aes(label = paste('Slope =', round(slope,2))),
              x = 1.5, y = 1.25, hjust = 0, vjust = 1, size = 4, color = 'black', fontface = 'italic') +
    geom_text(data = metrics_lm_log, aes(label = paste('R² =', round(R2,2))),
              x = 1.5, y = 1.15, hjust = 0, vjust = 1, size = 4, color = 'black', fontface = 'italic') +
    geom_text_repel(data = avg_metrics_long_filtered[avg_metrics_long_filtered$species %in% c('elephant','baboon','zebra','leopard'),], 
                    #aes(label = round(value,2)), 
                    aes(label = species),
                    # vjust = -0.5, hjust = 0, 
                    size = 4) +
    # geom_hline(yintercept = 0.70, lty = 'dotted', color = 'coral3', lwd = 1) + #alpha = 0.6
    # geom_vline(xintercept = log10(1000), lty = 'dotted', color = 'coral3', lwd = 1) + #alpha = 0.6
    # ylim(c(0,1.26)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.26)) +
    # xlab('log(Number of training images)') + 
    xlab(expression(log[10]('Number of training images'))) +
    facet_grid(~metric_name) +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          axis.title.y = element_blank())
  metrics_vs_training_log
  
  # ggsave('figures/metrics_vs_training_log.png', metrics_vs_training_log, dpi = 1000, width = 8, height = 4)
  
  #plot without regression line
  metrics_vs_training_2 <- ggplot(avg_metrics_long_filtered, aes(x = train_images, y = value)) +
    geom_point(alpha = 0.6, color = 'dodgerblue4', size = 2) +
    annotate('text', x = 200, y = 1.0, label = 'a', size = 5, fontface = 'bold.italic', color = 'coral3',) +
    annotate('text', x = 7500, y = 1.0, label = 'b', size = 5, fontface = 'bold.italic', color = 'coral3') +
    annotate('text', x = 200, y = 0, label = 'c', size = 5, fontface = 'bold.italic', color = 'coral3') +
    annotate('text', x = 7500, y = 0, label = 'd', size = 5, fontface = 'bold.italic', color = 'coral3') +
    geom_text_repel(data = avg_metrics_long_filtered[avg_metrics_long_filtered$species %in% 
                                                       c('elephant','baboon','zebra','leopard'),], 
                    aes(label = species), size = 4.5) +
    geom_hline(yintercept = 0.70, lty = 'dotted', color = 'coral3', lwd = 1) + #alpha = 0.6
    geom_vline(xintercept = 1000, lty = 'dotted', color = 'coral3', lwd = 1) + #alpha = 0.6
    ylim(c(0,1)) + 
    xlab('Number of training images') + 
    facet_grid(~metric_name) +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          axis.title.y = element_blank())
  metrics_vs_training_2
  
  # ggsave('figures/metrics_vs_training_3.png', metrics_vs_training_2, dpi = 1000, width = 8, height = 4)
  # ggsave('figures/metrics_vs_training_3.svg', metrics_vs_training_2, dpi = 1000, width = 8, height = 4)
  
  
  #Combine panels and save
  metrics_panel <- (metrics_vs_training_2 / metrics_vs_training_log) +
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag = element_text(face = 'bold', size = 16))
  metrics_panel
  
  # ggsave('figures/metrics_vs_training_panel.png', metrics_panel, dpi = 1000, width = 8, height = 8)
  
  
  #To report: values for species > 1000
  head(avg_metrics_filtered)
  
  avg_metrics_filtered %>% filter(train_images > 1000) %>% summarise(minAP = min(AP),
                                                                     minAR = min(AR),
                                                                     minAF1 = min(AF1),
                                                                     count = n())
  unique(avg_metrics_filtered[avg_metrics_filtered$train_images > 1000,]$species)
  
  
  #Linear models to report in Appendix
  summary(ap_train)
  summary(ar_train)
  summary(f1_train)
  
  summary(ap_train_log)
  summary(ar_train_log)
  summary(f1_train_log) 
  

## Get phylopic attributions ---------------------------------------------------
  
  'id' = c('cfee2dca-3767-46b8-8d03-bd8f46e79e9e','72f2f854-f3cd-4666-887c-35d5c256ab0f',
           '65c4a9b3-dcde-4f0f-9a1f-8d71e74be9ec','7fb9bea8-e758-4986-afb2-95a2c3bf983d',
           # '540d58bc-d82f-4cfc-a63b-2c37d083edec','3d8acaf6-4355-491e-8e86-4a411b53b98b', #lesser kudu (for bushbuck) and sus scrofa (bushpig)
           '540d58bc-d82f-4cfc-a63b-2c37d083edec','565c4a96-c80d-4ee3-86df-6f6c58ba2338', #lesser kudu (for bushbuck) and red river hog (bushpig)
           '7f4984bb-65ba-4f77-a92d-96e2c5dc65f0','6f3ebbc6-be53-4216-b45b-946f7984669b',
           '7db171af-ac7a-4859-9c2b-66488a5a5c95','62398ac0-f0c3-48f8-8455-53512a05fbc4',
           '4f49e5d0-f41e-427c-a1ab-9ec51e1bd72b','a8018e40-5257-4ad5-a42e-6fbad115deab',
           'f7200232-d635-4d79-bcaa-a59867376d09','8952c22d-1d0b-4658-b8ff-348df8aff18f',
           '880baa26-af89-44ad-ae07-e92b84e3ff05','49b72776-1f99-442b-a084-65f402cdbd69',
           'e07d1491-1d85-4c47-9f7d-075ea57bf0c5','f25267a2-90b4-4ca7-af95-a425f3564bbe',
           '5e900306-3a5e-4193-8a94-74cc765f0aaf','a0cde6a8-4a47-4562-827f-90bd8d7bdd05',
           'cf85f532-c0c2-40ba-ae83-47846caf1514','bb357997-4d4d-4e73-b853-d95a7a47ec68',
           'a363eadc-1b1e-41fb-ba8c-b45cb405a850','a6c8cc22-c035-48e1-bd2a-98a29446b392', #actually both roan
           '8930c815-0a3e-4b77-a2da-e7273bccddd7','01895510-7c27-489d-b441-6f72b1f562d2',
           'c9aaafb0-bbab-43b7-b2a8-077ae0799b96','9303fc52-3da1-4d42-9e6d-665c21791df8', #diff squirrel sp
           '9ebe32a3-3870-4073-9992-998dafe98014','d594a0c4-2708-4cde-ba41-08fde8b1184f',
           'f93103f1-e2a0-4c73-b274-c7b51afe4db0','81caf94e-5cbe-4e5e-8101-545abea2bfc0')
  
  get_attribution(id, text = TRUE, permalink = TRUE)
  
  