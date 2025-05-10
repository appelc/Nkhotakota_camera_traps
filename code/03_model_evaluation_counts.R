# Evaluate Malawi YOLO model 
## - Compare true vs. predicted counts of animals 
## - Linear models for counts ~ class and true ~ predicted counts
## - Create plots

library(data.table)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(rphylopic)
library(effects)
library(patchwork)
library(ggeffects)


## Read in data ----------------------------------------------------------------
sp_matches <- fread('outputs/image_matches.csv')

#Read in species list and filter
species_keep <- fread('outputs/species_for_metrics.csv', header = FALSE)
sp_matches <- sp_matches %>% filter(class_ref %in% species_keep$V2)

#Keep true positives only
table(sp_matches$match, useNA = 'a')
sp_matches_TP <- sp_matches[sp_matches$match == 'TP',]

#Keep images with 1 species only for this comparison
sp_per_img <- sp_matches_TP %>% group_by(filename) %>% summarise(n_species = length(unique(class_ref)))
img_mult_sp <- unique(sp_per_img[sp_per_img$n_species > 1,]$filename)

sp_matches_TP <- sp_matches_TP[!sp_matches_TP$filename %in% img_mult_sp,]

#For now also remove any image that has >1 row
sp_matches_TP <- sp_matches_TP[!(duplicated(sp_matches_TP$filename, fromLast = TRUE) | duplicated(sp_matches_TP$filename, fromLast = FALSE))]


## Model 1 ---------------------------------------------------------------------
### Test which classes have nonzero difference, and how much it is

#Calculate difference between true-pred
sp_matches_TP$diff <- sp_matches_TP$count_true - sp_matches_TP$count_pred
hist(sp_matches_TP$diff)

#Calculate proportional difference between pred/true
sp_matches_TP$pred_true <- sp_matches_TP$count_pred / sp_matches_TP$count_true
hist(sp_matches_TP$pred_true)

#Run both models
mod1a <- lm(diff ~ -1 + class_true, data = sp_matches_TP) #additive
mod1b <- lm(pred_true ~ -1 + class_true, data = sp_matches_TP) #proportional
summary(mod1a)
summary(mod1b)
  
#Examine residuals
hist(resid(mod1a),xlab='residuals',main='')
hist(resid(mod1b),xlab='residuals',main='')
    
 
## Plot coefficient estimates (Model 1) ----------------------------------------

#store model results -- version 1a
mod1a_sum <- summary(mod1a)
coef1a <- data.frame(mod1a_sum$coefficients)
colnames(coef1a) <- c('est','se','t_value','p_value')
# write.csv(coef1a, 'outputs/model_results_count_additive.csv')

#store model results -- version 1b to look at proportional changes
mod1b_sum <- summary(mod1b)
coef1b <- data.frame(mod1b_sum$coefficients)
colnames(coef1b) <- c('est','se','t_value','p_value')
# write.csv(coef1b, 'outputs/model_results_count_proportional.csv')

#extract class name and significance (based on p-value 0.05)
coef1a$class <- gsub('class_true', '', rownames(coef1a))
coef1b$class <- gsub('class_true', '', rownames(coef1b))

#convert SE to CI
coef1a$lci <- coef1a$est - (1.96 * coef1a$se)
coef1a$uci <- coef1a$est + (1.96 * coef1a$se)

coef1b$lci <- coef1b$est - (1.96 * coef1b$se)
coef1b$uci <- coef1b$est + (1.96 * coef1b$se)

#remove human (any others?)
coef1a <- coef1a[coef1a$class != 'human',]
coef1b <- coef1b[coef1b$class != 'human',]

#edit class names
coef1a$class_mod <- ifelse(coef1a$class == 'guinea_fowl', 'guineafowl', coef1a$class)
coef1a$class_mod <- ifelse(coef1a$class == 'bush_squirrel', 'squirrel', coef1a$class_mod)
coef1a$class_mod <- ifelse(coef1a$class == 'scrub_hare', 'savanna_hare', coef1a$class_mod)
coef1a$class_mod <- gsub('_', ' ', coef1a$class_mod)

coef1b$class_mod <- ifelse(coef1b$class == 'guinea_fowl', 'guineafowl', coef1b$class)
coef1b$class_mod <- ifelse(coef1b$class == 'bush_squirrel', 'squirrel', coef1b$class_mod)
coef1b$class_mod <- ifelse(coef1b$class == 'scrub_hare', 'savanna_hare', coef1b$class_mod)
coef1b$class_mod <- gsub('_', ' ', coef1b$class_mod)

#summarize group sizes
group_size <- sp_matches_TP %>% group_by(class_true) %>% summarise(group_avg = mean(count_true),
                                                                   group_median = median(count_true),
                                                                   group_max = max(count_true))
group_size$class_mod <- ifelse(group_size$class_true == 'guinea_fowl', 'guineafowl', group_size$class_true)
group_size$class_mod <- ifelse(group_size$class_true == 'bush_squirrel', 'squirrel', group_size$class_mod)
group_size$class_mod <- ifelse(group_size$class_true == 'scrub_hare', 'savanna_hare', group_size$class_mod)
group_size$class_mod <- gsub('_', ' ', group_size$class_mod)
group_size <- group_size[group_size$class_true != 'human',]

#add to coefficient dataframe
coef1a$max_group_size <- group_size$group_max[match(coef1a$class, group_size$class_true)]
coef1a$mean_group_size <- group_size$group_avg[match(coef1a$class, group_size$class_true)]
table(coef1a$max_group_size)
table(round(coef1a$mean_group_size)) #round or floor?

coef1a$max_over_1 <- ifelse(coef1a$max_group_size > 1, 'y', 'n')
coef1a$mean_over_1 <- ifelse(coef1a$mean_group_size > 1, 'Groups > 1', 'Groups = 1') #these end up being the same

coef1b$max_group_size <- group_size$group_max[match(coef1b$class, group_size$class_true)]
coef1b$mean_group_size <- group_size$group_avg[match(coef1b$class, group_size$class_true)]
table(coef1b$max_group_size)
table(round(coef1b$mean_group_size)) #round or floor?

coef1b$max_over_1 <- ifelse(coef1b$max_group_size > 1, 'y', 'n')
coef1b$mean_over_1 <- ifelse(coef1b$mean_group_size > 1, 'Groups > 1', 'Groups = 1') #these end up being the same

#add to group size dataframe to match facets
group_size$mean_over_1 <- ifelse(group_size$group_avg > 1, 'Groups > 1', 'Groups = 1') 

#set breaks for legends
breaks_vector <- c(1,10,19)

#merge additive and proportional for sorting
coef1a$est_mod1b <- coef1b$est[match(coef1a$class_mod, coef1b$class_mod)]
coef1b$est_mod1a <- coef1a$est[match(coef1b$class_mod, coef1a$class_mod)]

## plot (additive)
mod1a_plot <- ggplot(coef1a, aes(x = reorder(class_mod, est_mod1b), y = est, 
                              size = max_group_size, color = max_group_size)) + #change mean or max here
  geom_pointrange(aes(ymin = lci, ymax = uci)) +
  geom_point() +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), limits = c(-0.28,1.1)) +
  scale_x_discrete(expand = expansion(add = c(1, 1))) +
  scale_size_continuous(range = c(0.4,1.5), breaks = breaks_vector) +
  scale_color_viridis_c(option = 'D', breaks = breaks_vector) +
  guides(color = guide_legend('Group size (max)'), size = guide_legend('Group size (max)')) +
  coord_flip() +
  ggtitle('True - Predicted') +
  geom_hline(yintercept = 0, lty = 'dashed') +
  ylab('Additive change ± 95% CI') +
  theme_bw() + theme(axis.title.y = element_blank(),
                     axis.text = element_text(size = 12),
                     legend.position = 'inside',
                     legend.background = element_rect(fill = 'transparent'),
                     legend.title = element_text(size = 12, face = 'bold'),
                     legend.justification = c(0.9, 0.9),
                     legend.text = element_text(size = 12))
mod1a_plot
# ggsave('figures/count_model_est_diff_mean.png', mod1a_plot, dpi = 1000, width = 5, height = 6)
# ggsave('figures/count_model_est_diff_max.png', mod1a_plot, dpi = 1000, width = 5, height = 6)

  
## plot (proportional) 
mod1b_plot_prop <- ggplot(coef1b, aes(x = reorder(class_mod, est), y = est, 
                                     size = max_group_size, color = max_group_size)) + #change mean or max here
  geom_pointrange(aes(ymin = lci, ymax = uci)) +
  geom_point() +
  # scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), limits = c(,1.1)) +
  scale_x_discrete(expand = expansion(add = c(1.1, 1))) +
  scale_size_continuous(range = c(0.4,1.5), breaks = breaks_vector) +
  scale_color_viridis_c(option = 'D', breaks = breaks_vector) +
  guides(color = guide_legend('Group size (max)'), size = guide_legend('Group size (max)')) +
  # guides(color = guide_colorbar(title = 'Group size (max)', breaks = breaks_vector),
  #        size = guide_legend(title = 'Group size (max)', breaks = breaks_vector)) +
  coord_flip() +
  ggtitle('Predicted/True') +
  geom_hline(yintercept = 1, lty = 'dashed') +
  ylab('Proportional change ± 95% CI') +
  theme_bw() + theme(axis.title.y = element_blank(),
                     axis.text = element_text(size = 12),
                     legend.position = 'none',
                     # legend.position = 'inside',
                     legend.background = element_rect(fill = 'transparent'),
                     legend.justification = c(0.05, 1),
                     legend.title = element_text(size = 12, face = 'bold'),
                     legend.text = element_text(size = 12))
mod1b_plot_prop
# ggsave('figures/count_model_est_prop_mean.png', mod1b_plot_prop, dpi = 1000, width = 5, height = 6)
# ggsave('figures/count_model_est_prop_max.png', mod1b_plot_prop, dpi = 1000, width = 5, height = 6)

  
## plot (proportional): 1 - est
mod1b_plot_prop_minus <- ggplot(coef1b, aes(x = reorder(class_mod, est), y = 1-est, 
                                     size = mean_group_size, color = mean_group_size)) + #change mean or max here
  geom_pointrange(aes(ymin = 1-lci, ymax = 1-uci)) +
  geom_point() +
  # scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), limits = c(,1.1)) +
  scale_x_discrete(expand = expansion(add = c(1.1, 1))) +
  scale_size_continuous(range = c(0.4,1.5)) + #previously 0.5, 2
  coord_flip() +
  ggtitle('1 - (Predicted/True)') +
  geom_hline(yintercept = 0, lty = 'dashed') +
  ylab('1 - Proportional change in count ± 95% CI') +
  theme_bw() + theme(axis.title.y = element_blank(),
                     axis.text = element_text(size = 12),
                     legend.position = 'none',
                     legend.justification = c(0.9, 0.9))
mod1b_plot_prop_minus
# ggsave('figures/count_model_est_prop-1_mean.png', mod1b_plot_prop_minus, dpi = 1000, width = 5, height = 6)
# ggsave('figures/count_model_est_prop-1_max.png', mod1b_plot_prop_minus, dpi = 1000, width = 5, height = 6)
  
    
## plot individual species histograms for gut check ----------------------------
par(mfrow = c(2,2))
for (sp in unique(sp_matches_TP$class_true)){
  hist(sp_matches_TP[sp_matches_TP$class_true == sp,]$diff, main=sp, xlab='diff')
}


## Model 2 ---------------------------------------------------------------------
### How much higher is the true count for each increase in predicted count?

mod2 <- lm(count_true ~ -1 + count_pred:class_true, data = sp_matches_TP)
summary(mod2)
hist(resid(mod2), xlab = 'residuals', main='')
  
#store model results
mod2_sum <- summary(mod2)
coef2 <- data.frame(mod2_sum$coefficients)
# write.csv(coef2, 'outputs/model_results_count_interaction.csv')


## Marginal plots (Model 2) ----------------------------------------------------

#generate new data for predictions (only go to max group size for each class)
newdata <- group_size %>% group_by(class_true) %>% 
                          summarise(count_pred = list(1:max(group_max))) %>% #or group_max+3
                          unnest(cols = c(count_pred))

#generate predictions (lm)    
mod2_pred <- predict.lm(mod2, newdata = newdata, se.fit = TRUE)

#create dataframe and convert SE to CI
preds_df <- newdata %>% mutate(fit = mod2_pred$fit,
                               lowerSE = fit - mod2_pred$se.fit,
                               upperSE = fit + mod2_pred$se.fit,
                               lowerCI = fit - (1.96*mod2_pred$se.fit),
                               upperCI = fit + (1.96*mod2_pred$se.fit))

#remove human and change class names
preds_df <- preds_df[preds_df$class_true != 'human',]
sp_matches_TP <- sp_matches_TP[sp_matches_TP$class_true != 'human',]

preds_df$class_mod <- ifelse(preds_df$class_true == 'ground_hornbill', 'ground hornbill', preds_df$class_true)
preds_df$class_mod <- ifelse(preds_df$class_true == 'bush_squirrel', 'squirrel', preds_df$class_mod)
preds_df$class_mod <- ifelse(preds_df$class_true == 'scrub_hare', 'savanna hare', preds_df$class_mod)
preds_df$class_mod <- ifelse(preds_df$class_true == 'guinea_fowl', 'guineafowl', preds_df$class_mod)
preds_df$class_mod <- gsub('_', ' ', preds_df$class_mod)

sp_matches_TP$class_mod <- ifelse(sp_matches_TP$class_true == 'ground_hornbill', 'ground hornbill', sp_matches_TP$class_true)
sp_matches_TP$class_mod <- ifelse(sp_matches_TP$class_true == 'bush_squirrel', 'bush squirrel', sp_matches_TP$class_mod)
sp_matches_TP$class_mod <- ifelse(sp_matches_TP$class_true == 'scrub_hare', 'savanna hare', sp_matches_TP$class_mod)
sp_matches_TP$class_mod <- ifelse(sp_matches_TP$class_true == 'guinea_fowl', 'guineafowl', sp_matches_TP$class_mod)
sp_matches_TP$class_mod <- gsub('_', ' ', sp_matches_TP$class_mod)

## plot
mod2_plot <- ggplot(preds_df, 
                    aes(x = count_pred, y = fit, color = class_mod, fill = class_mod)) +
  geom_abline(slope = 1, intercept = 0, lty = 'dashed', lwd = 0.4, color = 'gray30') +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), alpha = 0.4, color = NA) +
  geom_line() +
  geom_rug(data = sp_matches_TP, aes(y = count_true), inherit.aes = FALSE, length = unit(0.05, 'npc')) +
  geom_rug(data = sp_matches_TP, aes(x = count_pred), inherit.aes = FALSE, length = unit(0.05, 'npc')) +
  xlab('Predicted count') + ylab('True count') +
  facet_wrap(~class_mod, nrow = 5) + #, scales = 'free'
  theme_bw() + theme(legend.position = 'none')
mod2_plot

  
## now facet by group size -- doesn't make sense to predict some out to 20
  
#by med, mean, max?
preds_df$max_group_size <- group_size$group_max[match(preds_df$class_true, group_size$class_true)]
table(preds_df$max_group_size)

#also add group size to original dataframe for plotting rug values
sp_matches_TP$max_group_size <- group_size$group_max[match(sp_matches_TP$class_true, group_size$class_true)]

#read in phylopic IDs
phylopic_ids <- fread('outputs/phylopic_ids.csv'); phylopic_ids <- phylopic_ids[,-'V1']

#add coordinates for plotting (each has its own axis, helps to plot below first without)
phylopic_ids$x[phylopic_ids$species_cleaned == 'bushbuck'] <- 2.2
phylopic_ids$y[phylopic_ids$species_cleaned == 'bushbuck'] <- 0.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'bushbuck'] <- 1
phylopic_ids$x[phylopic_ids$species_cleaned == 'small antelope'] <- 2.2
phylopic_ids$y[phylopic_ids$species_cleaned == 'small antelope'] <- 0.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'small antelope'] <- 1
phylopic_ids$x[phylopic_ids$species_cleaned == 'reedbuck'] <- 2.2
phylopic_ids$y[phylopic_ids$species_cleaned == 'reedbuck'] <- 0.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'reedbuck'] <- 1.1
phylopic_ids$x[phylopic_ids$species_cleaned == 'porcupine'] <- 2.5
phylopic_ids$y[phylopic_ids$species_cleaned == 'porcupine'] <- 0.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'porcupine'] <- 1
phylopic_ids$x[phylopic_ids$species_cleaned == 'ground hornbill'] <- 2.5
phylopic_ids$y[phylopic_ids$species_cleaned == 'ground hornbill'] <- 0.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'ground hornbill'] <- 1
phylopic_ids$x[phylopic_ids$species_cleaned == 'impala'] <- 2.2
phylopic_ids$y[phylopic_ids$species_cleaned == 'impala'] <- 0.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'impala'] <- 1.2
phylopic_ids$x[phylopic_ids$species_cleaned == 'bushpig'] <- 4.5
phylopic_ids$y[phylopic_ids$species_cleaned == 'bushpig'] <- 1
phylopic_ids$height[phylopic_ids$species_cleaned == 'bushpig'] <- 1.2
phylopic_ids$x[phylopic_ids$species_cleaned == 'kudu'] <- 5
phylopic_ids$y[phylopic_ids$species_cleaned == 'kudu'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'kudu'] <- 3
phylopic_ids$x[phylopic_ids$species_cleaned == 'elephant'] <- 4.5
phylopic_ids$y[phylopic_ids$species_cleaned == 'elephant'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'elephant'] <- 2
phylopic_ids$x[phylopic_ids$species_cleaned == 'zebra'] <- 4.5
phylopic_ids$y[phylopic_ids$species_cleaned == 'zebra'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'zebra'] <- 1.9
phylopic_ids$x[phylopic_ids$species_cleaned == 'waterbuck'] <- 5.2
phylopic_ids$y[phylopic_ids$species_cleaned == 'waterbuck'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'waterbuck'] <- 3.2
phylopic_ids$x[phylopic_ids$species_cleaned == 'eland'] <- 4.5
phylopic_ids$y[phylopic_ids$species_cleaned == 'eland'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'eland'] <- 3.2
phylopic_ids$x[phylopic_ids$species_cleaned == 'vervet monkey'] <- 6
phylopic_ids$y[phylopic_ids$species_cleaned == 'vervet monkey'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'vervet monkey'] <- 3
phylopic_ids$x[phylopic_ids$species_cleaned == 'warthog'] <- 6.5
phylopic_ids$y[phylopic_ids$species_cleaned == 'warthog'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'warthog'] <- 3
phylopic_ids$x[phylopic_ids$species_cleaned == 'buffalo'] <- 6
phylopic_ids$y[phylopic_ids$species_cleaned == 'buffalo'] <- 1.5
phylopic_ids$height[phylopic_ids$species_cleaned == 'buffalo'] <- 7
phylopic_ids$x[phylopic_ids$species_cleaned == 'baboon'] <- 9
phylopic_ids$y[phylopic_ids$species_cleaned == 'baboon'] <- 2
phylopic_ids$height[phylopic_ids$species_cleaned == 'baboon'] <- 4
phylopic_ids$x[phylopic_ids$species_cleaned == 'guineafowl'] <- 8
phylopic_ids$y[phylopic_ids$species_cleaned == 'guineafowl'] <- 2
phylopic_ids$height[phylopic_ids$species_cleaned == 'guineafowl'] <- 4
phylopic_ids$x[phylopic_ids$species_cleaned == 'sable'] <- 14
phylopic_ids$y[phylopic_ids$species_cleaned == 'sable'] <- 5
phylopic_ids$height[phylopic_ids$species_cleaned == 'sable'] <-100

#merge phylopic IDs with data
preds_df_plot <- preds_df %>% left_join(phylopic_ids, by = c('class_mod' = 'species_cleaned'))

#sort facets by estimates to match prev figure
preds_df_plot <- preds_df_plot %>% mutate(class_mod_sort = reorder(class_mod, fit)) #no, reorder by group size, below
sp_matches_TP$class_mod_sort <- factor(sp_matches_TP$class_mod, levels = levels(preds_df_plot$class_mod_sort)) #to match


## plot only for ones with observed group size > 2
mod2_plot_a <- ggplot(preds_df_plot[preds_df_plot$max_group_size > 2,], 
                      aes(x = count_pred, y = fit, color = max_group_size, fill = max_group_size)) + #or use class_mod for color/fill
  geom_abline(slope = 1, intercept = 0, lty = 'dashed', lwd = 0.4, color = 'gray40') +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), alpha = 0.3, color = NA) +
  geom_line(lwd = 1.1) + 
  geom_phylopic(aes(x = x, y = y, uuid = id, height = height), fill = 'black', color = NA) +
  scale_height_continuous(range = c(1,10)) +
  # coord_cartesian(xlim = c(0,20), ylim = c(0,20)) +
  coord_cartesian(clip = 'off') +
  geom_rug(data = sp_matches_TP[sp_matches_TP$max_group_size > 2,],
           aes(y = count_true, x = 0, color = max_group_size), inherit.aes = FALSE, length = unit(0.04, 'npc'),
           sides = 'l', position = position_jitter(height = 0.3)) +
  geom_rug(data = sp_matches_TP[sp_matches_TP$max_group_size > 2,],
           aes(x = count_pred, y = 0, color = max_group_size), inherit.aes = FALSE, length = unit(0.07, 'npc'),
           sides = 'b', position = position_jitter(width = 0.3)) +
  scale_color_viridis_c(option = 'D') +
  scale_fill_viridis_c(option = 'D') +
  xlab('Predicted count') + ylab('True count (± 95% CI)') +
  # facet_wrap(~class_mod_sort, nrow = 3, scales = 'free') + #nrow = 3 or 6
  facet_wrap(~reorder(class_mod, max_group_size), nrow = 3, scales = 'free') + #nrow = 3 or 6
  theme_bw() + theme(legend.position = 'none',
                     strip.background = element_blank(),
                     strip.placement = 'outside',
                     strip.text = element_text(size = 10), 
                     axis.title = element_text(size = 12))
mod2_plot_a
#ggsave('figures/count_model_predictions_over2.png', mod2_plot_a, dpi = 1000, width = 5, height = 6)

## Don't use this one though; use Poisson, below


## Model 2b --------------------------------------------------------------------
### How much higher is the true count for each increase in predicted count? (GLM with Poisson)

#only use species with max group size > 2
(sp_gr2 <- unique(group_size[group_size$group_max > 2,]$class_true))
sp_matches_TP_gr2 <- sp_matches_TP[sp_matches_TP$class_true %in% sp_gr2,]

#convert classes to factor
sp_matches_TP_gr2$class_true <- as.factor(sp_matches_TP_gr2$class_true)

#Poisson model
#use quasi-poisson to relax mean=variance assumption (for p-values only)
mod2p <- glm(count_true ~ log(count_pred)*class_true, data = sp_matches_TP_gr2, #use 'log(count_pred)'
             family = 'quasipoisson') 
summary(mod2p)

#Test predict and plot using ggpredict (will do manually below to have more control)
preds2p_ggp <- ggpredict(mod2p, terms = c('count_pred [0:19 by=1]','class_true'))

ggplot(preds2p_ggp, aes(x = x, y = predicted, color = group)) +
  geom_abline(slope = 1, intercept = 0, lty = 'dashed', lwd = 0.4, color = 'gray30') +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.4, color = NA) +
  facet_wrap(~group) +
  ggtitle('ggpredict method') +
  # scale_color_viridis_d(option = "D") +
  # ylim(c(0,20)) +
  theme_bw()

#store model results
mod2p_sum <- summary(mod2p)
coef2p <- data.frame(mod2p_sum$coefficients)
# write.csv(coef2p, 'Malawi_evaluating/model_results_count_interaction_poisson.csv')
    

## Marginal plots (Model 2b) ---------------------------------------------------

#generate new data for predictions (only go to max group size for each class)
group_size_gr2 <- group_size[group_size$group_max > 2,] #only keep classes with max group size > 2
newdata_gr2 <- group_size_gr2 %>% group_by(class_true) %>% 
                                  summarise(count_pred = list(1:max(group_max))) %>% #or group_max+3
                                  unnest(cols = c(count_pred))

#predict and calculate CI
preds2p <- predict(mod2p, newdata = newdata_gr2, type = 'response', se.fit = TRUE) 
preds_df_2p <- newdata_gr2 %>% mutate(fit = preds2p$fit,
                               lowerSE = fit - preds2p$se.fit,
                               upperSE = fit + preds2p$se.fit,
                               lowerCI = fit - (1.96*preds2p$se.fit),
                               upperCI = fit + (1.96*preds2p$se.fit))
  
#change class names
preds_df_2p$class_mod <- ifelse(preds_df_2p$class_true == 'ground_hornbill', 'ground hornbill', preds_df_2p$class_true)
preds_df_2p$class_mod <- ifelse(preds_df_2p$class_true == 'bush_squirrel', 'squirrel', preds_df_2p$class_mod)
preds_df_2p$class_mod <- ifelse(preds_df_2p$class_true == 'scrub_hare', 'savanna hare', preds_df_2p$class_mod)
preds_df_2p$class_mod <- ifelse(preds_df_2p$class_true == 'guinea_fowl', 'guineafowl', preds_df_2p$class_mod)
preds_df_2p$class_mod <- gsub('_', ' ', preds_df_2p$class_mod)
  #already changed them in 'sp_matches_TP' above
  
#now add max group size so I can change the axes by class
preds_df_2p$max_group_size <- group_size$group_max[match(preds_df_2p$class_true, group_size$class_true)]
table(preds_df_2p$max_group_size)
  
#I read in phylopic IDs above -- but change some coords here if I need to 
phylopic_ids[phylopic_ids$species_cleaned %in% c('bushbuck','ground hornbill','impala',
                                                 'porcupine','reedbuck','small antelope')]$y <- 0.75
phylopic_ids[phylopic_ids$species_cleaned %in% c('bushpig','eland','elephant','kudu','zebra',
                                                 'waterbuck')]$y <- 2
phylopic_ids[phylopic_ids$species_cleaned %in% c('buffalo','vervet monkey','warthog',
                                                 'guineafowl','baboon')]$y <- 3.0
phylopic_ids[phylopic_ids$species_cleaned %in% c('sable')]$y <- 10 #from 5
phylopic_ids[phylopic_ids$species_cleaned %in% c('sable')]$x <- 16 #from 14
phylopic_ids[phylopic_ids$species_cleaned %in% c('small antelope')]$x <- 2.5 #from 2.2

#and icon sizes, if necessary (first play with range limits in scale_height_continuous though)
phylopic_ids[phylopic_ids$species_cleaned == 'impala']$height <- 1.05 #from 1.2
phylopic_ids[phylopic_ids$species_cleaned == 'eland']$height <- 2.6 #from 3.2
phylopic_ids[phylopic_ids$species_cleaned == 'warthog']$height <- 4.0 #from 3.0
phylopic_ids[phylopic_ids$species_cleaned == 'guineafowl']$height <- 2.5 #from 4.0

#and merge them with data
preds_df_plot_2p <- preds_df_2p %>% left_join(phylopic_ids, by = c('class_mod' = 'species_cleaned'))

#create a dummy dataset to set y axis limits for each row
preds_df_plot_2p$dummy_y <- ifelse(preds_df_plot_2p$class_true %in% c('bushbuck','ground_hornbill','impala',
                                                                      'porcupine','reedbuck','small_antelope'), 4, NA)
preds_df_plot_2p$dummy_y <- ifelse(preds_df_plot_2p$class_true %in% c('bushpig','eland','elephant',
                                                                      'kudu','zebra','waterbuck'), 9, preds_df_plot_2p$dummy_y)
preds_df_plot_2p$dummy_y <- ifelse(preds_df_plot_2p$class_true %in% c('buffalo','vervet_monkey','warthog',
                                                                      'guinea_fowl','baboon'), 17, preds_df_plot_2p$dummy_y)
preds_df_plot_2p$dummy_y <- ifelse(preds_df_plot_2p$class_true %in% c('sable'), 46, preds_df_plot_2p$dummy_y)
            

## plot true ~ predicted per class (GLM with Poisson)
mod2p_plot_a <- ggplot(preds_df_plot_2p[preds_df_plot_2p$max_group_size > 2,], 
                      aes(x = count_pred, y = fit, color = max_group_size, fill = max_group_size)) + #or use class_mod for color/fill
  geom_point(aes(x = count_pred, y = dummy_y), size = 0, color = NA) + #for setting axis limits only
  geom_abline(slope = 1, intercept = 0, lty = 'dashed', lwd = 0.4, color = 'gray40') +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), alpha = 0.3, color = NA) +
  geom_line(lwd = 1.1) + 
  geom_phylopic(aes(x = x, y = y, uuid = id, height = height), fill = 'black', color = NA) +
  scale_height_continuous(range = c(1,20)) + #for size of phylopic 
  # coord_cartesian(clip = 'off') +
  # geom_rug(data = sp_matches_TP[sp_matches_TP$max_group_size > 2,],
  #          aes(y = count_pred, x = 0, color = max_group_size), inherit.aes = FALSE, length = unit(0.04, 'npc'),
  #          sides = 'l', position = position_jitter(height = 0.3)) +
  geom_rug(data = sp_matches_TP[sp_matches_TP$max_group_size > 2,],
           aes(x = count_true, y = 0, color = max_group_size), inherit.aes = FALSE, length = unit(0.07, 'npc'),
           sides = 'b', position = position_jitter(width = 0.3)) +
  scale_color_viridis_c(option = 'D') +
  scale_fill_viridis_c(option = 'D') +
  scale_y_continuous(limits = c(0, NA),
                     breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) + #ensures y-axes >0
  scale_x_continuous(limits = c(0, NA),
                     breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))) + #ensures y-axes >0
  xlab('Predicted count') + ylab('True count (± 95% CI)') +
  # facet_wrap(~class_mod_sort, nrow = 3, scales = 'free') + #nrow = 3 or 6
  facet_wrap(~reorder(class_mod, max_group_size), nrow = 3, scales = 'free') + #nrow = 3 or 6
  theme_bw() + theme(legend.position = 'none',
                     strip.background = element_blank(),
                     strip.placement = 'outside',
                     strip.text = element_text(size = 10), 
                     axis.title = element_text(size = 12))
mod2p_plot_a
  
#combine with other plots
fig6p <- ((mod1b_plot_prop + 
             theme(plot.title = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y.left = element_blank(),
                   axis.ticks.y.right = element_line(linewidth = 2))) | (mod1a_plot + 
                                                                           theme(plot.title = element_blank(),
                                                                                 axis.text.y = element_text(hjust = 0.5)))) / 
  mod2p_plot_a +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = 'bold', size = 16)) #plot.tag.position = c(0.05, 0.95)
fig6p
# ggsave('figures/count_model_panel3_f_poisson2.png', fig6p, dpi = 1000, width = 9, height = 10)


##To report in appendix:
summary(mod2p)
  

