
## Evaluate Malawi YOLOv5 model for EcolIndicators paper

## PART 4
##    - Species richness

library(data.table)
library(tidyverse)
library(ggplot2)
library(scales)

setwd('/Users/caraappel/Documents/_RESEARCH/YOLO/')


## Read in data -------------------------------    
true_df <- fread('Malawi_evaluating/true_df_073024.csv'); true_df <- true_df[,-'V1']
preds_df <- fread('Malawi_evaluating/preds_df_073024.csv'); preds_df <- preds_df[,-'V1']

#Read in species list and filter
species_keep <- fread('Malawi_evaluating/species_for_metrics.csv', header = FALSE)

true_df <- true_df %>% filter(species %in% species_keep$V2)
preds_df <- preds_df %>% filter(species_cleaned %in% species_keep$V2)


### Species richness (similar to Whytock et al.) -------------------------------    

head(true_df) #(true is really "observed")
head(preds_df)  

(species <- species_keep$V2) #from above
thresh <- seq(0.25, 1, by = 0.1)

richness <- NULL #create empty object for storing results
sp_lists <- list()

for (cc in unique(preds_df$site)) { #for each site that had predictions,
  true_cc <- true_df[true_df$site == cc,] #find the verified species detections
  pred_cc <- preds_df[preds_df$site == cc,] #find the model predicted species detections
  
  sp_lists_tt <- list()
  for (tt in thresh){ #and for each threshold,
    #find predicted detections under this threshold
    pred_cc_tt <- pred_cc[pred_cc$max_score >= tt,] 
    
    #calculate richness
    sp_rich_true <- length(unique(true_cc$species)) #true sp richness (threshold isn't relevant here)
    sp_rich_pred <- length(unique(pred_cc_tt$class)) #predicted sp richness at this threshold

    #store
    rich_cc <- data.frame('site' = cc, 'thresh' = tt, 'rich_obs' = sp_rich_true, 'rich_pred' = sp_rich_pred) #wide format
    sp_cc <- list('sp_true' = unique(true_cc$species) , 'sp_pred' = unique(pred_cc_tt$class))
    
    #save
    richness <- bind_rows(richness, rich_cc)
    sp_lists_tt[[paste('thresh', gsub('0.','',tt), sep = '_')]] <- sp_cc
    
  } #end threshold loop
  
  sp_lists[[cc]] <- sp_lists_tt
  
} #end site loop

head(richness)
head(sp_lists$`C08-A`$thresh_95)



## Regression model ------------------------------------------------------------

model_preds <- NULL #for storing
model_params <- NULL

for (tt in thresh){ #for each threshold,
  
  richness_thresh <- richness[richness$thresh == tt,]
  
  mod_tt <- lm(rich_pred ~ rich_obs, data = richness_thresh) #run linear model
  coef_tt <- coef(lm(rich_pred ~ rich_obs, data = richness_thresh))[2]  #store slope
  int_tt <- coef(lm(rich_pred ~ rich_obs, data = richness_thresh))[1] #store intercept
  r2_tt <- summary(mod_tt)$r.squared #store R2
  
  #generate observed richness values for plotting
  obs_rich_values <- data.frame('rich_obs' = seq(min(richness_thresh$rich_obs), max(richness_thresh$rich_obs), length.out = nrow(richness_thresh)))
  
  #predict y-values (predicted richness) for each of those values
  pred_rich_predictions <- predict(mod_tt, newdata = obs_rich_values)
  
  #store
  pred_df_tt <- data.frame('thresh' = rep(tt,nrow(richness_thresh)), 'obs_valus' = obs_rich_values, 'pred_values' = pred_rich_predictions)
  params_df_tt <- data.frame('thresh' = tt, 'intercept' = int_tt, 'slope' = coef_tt, 'r2' = r2_tt)
  model_preds <- bind_rows(model_preds, pred_df_tt)
  model_params <- bind_rows(model_params, params_df_tt)
}

head(model_preds)
head(model_params)


## Plot using ggplot --------------------------------------------------------
  head(model_preds)
  head(richness)
  thresh
  
  # blue = rgb(red = 0, green = 153, blue = 153, maxColorValue = 255)
  
  #plot
  sp_rich_plot <- ggplot(data = richness, aes(x = rich_obs, y = rich_pred)) +
    geom_point(color = 'steelblue4', alpha = 0.5) +
    # geom_line(data = model_preds, aes(x = rich_obs, y = pred_values)) +
    geom_abline(data = model_params, aes(slope = slope, intercept = intercept), color = 'steelblue4') +
    geom_abline(slope = 1, intercept = 0, lty = 'dashed') +
    geom_text(data = model_params, aes(label = paste('Slope = ', sprintf('%.2f', slope), sep = ' ')),
              x = 15, y = 8, hjust = 0, size = 3.5) + #(2,32) or (17,8)
    geom_text(data = model_params, aes(label = paste('R² = ', sprintf('%.2f', r2), sep = ' ')),
              x = 15, y = 4, hjust = 0, size = 3.5) + #(2,28) or (17,4)
    xlim(c(0,35)) + ylim(c(0,35)) +
    xlab('Species richness (observed)') + ylab('Species richness (predicted)') +
    facet_wrap(~factor(thresh), nrow = 2) +
    theme_bw() + theme(axis.title = element_text(size = 14),
                       axis.text = element_text(size = 10),
                       strip.text = element_text(size = 12))
  sp_rich_plot
  ggsave('figures/species_richness_ggplot.png', sp_rich_plot, dpi = 1000, width = 7, height = 4)
  
  
  
## Summaries -------------------------------------------------------------------

  head(richness)
  
  #range of values (#species)?
  summary(richness$rich_obs) #observed: mean 10, range 0 - 24
  summary(richness$rich_pred) #predicted: mean 15, range 0 - 33
  
  #how many sites were used in these calculations?
  length(unique(richness$site)) #158
  
  #what was the biggest gap between obs/est? look at just thresh=0.95 for now
  rich_95 <- richness[richness$thresh == '0.95',]
  rich_95$gap <- rich_95$rich_pred - rich_95$rich_obs
    table(rich_95$gap) 
    
    #which sites? (158 total)
    sort(unique(richness[richness$gap > 4,]$site))
    sort(unique(richness[richness$gap == 19,]$site))
    
  #assess bias more formally (same as 'gap' above)
  richness$bias <- richness$rich_pred - richness$rich_obs
    
    #summarize by threshold (avg among all sites)
    (rich_sum <- richness %>% group_by(thresh) %>% summarise(bias_mean = mean(bias),
                                                bias_sd = sd(bias),
                                                bias_min = min(bias),
                                                bias_max = max(bias),
                                                n = n()))
  
    #plot?
    ggplot(rich_sum, aes(x = thresh, y = bias_mean)) +
      geom_point() +
      # geom_line() +
      geom_pointrange(aes(ymin = bias_mean - bias_sd, ymax = bias_mean + bias_sd)) +
      theme_bw()
    
    
  #were certain species often missed?
  
  #(didn't finish this...)
  for (ss in names(sp_lists)){
    site <- sp_lists[[ss]]
    
    for (tt in names(site)){
      site_tt <- site[[tt]]
      
      unique_true <- site_tt$sp_true[!site_tt$sp_true %in% site_tt$sp_pred]
      unique_pred <- site_tt$sp_pred[!site_tt$sp_pred %in% site_tt$sp_true]
      all_sp <- unique(c(site_tt$sp_true, site_tt$sp_pred))
      
      true_df <- ifelse(length(unique_true) == 0, data.frame(NA), 
                        data.frame('sp' = paste(unique_true), 'unique' = 'true', thresh = tt, 'site' = ss))
      pred_df <- ifelse(length(unique_pred) == 0, data.frame(0), 
                        data.frame('sp' = unique_pred, 'unique' = 'pred', thresh = tt, 'site' = ss))
    }
  }
    
  #
  test <- rbindlist(sp_lists)
  test1 <- lapply(sp_lists, FUN = function(x) rbindlist(x, use.names = F))
    head(test)
  
  
    
### Plot using code from Whytock: -----------------------------------------------
    
    #create color palette
    blue = rgb(red = 0, green = 153, blue = 153, alpha = 125, maxColorValue = 255)
    
    #set up plot to save
    # pdf("../figures/speciesRichness_thresholds.pdf", width = 11, height = 8.5)
    png('figures/species_richness.png', width = 7, height = 4, units = 'in', res = 1000)  # Open PNG device
    
    #and thresholds
    thresh <- seq(0.25, 1, by = 0.1)
    
    #set up and plot
    par(mfrow = c(2,4), mar = c(2,2,2,2), oma = c(4,4,1,1))
    for (tt in thresh){ #for each threshold,
      
      #get data
      rich_tt <- richness[richness$thresh >= tt,] 
      
      #start plot
      plot(
        rich_tt$rich_pred ~ rich_tt$rich_obs,
        ylim = c(0, 35),
        xlim = c(0, 35),
        xlab = "",
        ylab = "",
        col = blue,
        pch = 16,
        cex = 1.2
      )
      
      #run model
      mod_tt <- lm(rich_pred ~ rich_obs, data = rich_tt) #run linear model
      coef_tt <- coef(lm(rich_pred ~ rich_obs, data = rich_tt))[2]  #store slope
      r2_tt <- summary(mod_tt)$r.squared #store R2
      
      #generate observed richness values for plotting
      obs_rich_values <- data.frame('rich_obs' = seq(min(rich_tt$rich_obs), max(rich_tt$rich_obs), length.out = nrow(rich_tt)))
      
      #predict y-values (predicted richness) for each of those values
      pred_rich_predictions <- predict(mod_tt, newdata = obs_rich_values)
      
      #plot
      lines(pred_rich_predictions ~ obs_rich_values$rich_obs)
      
      text(x = 32, paste0("Threshold = ", tt), adj = -0.01)
      text(x = 28, paste0("Slope = ", sprintf("%.2f", round(coef_tt, digits = 2))), adj = -0.01)
      text(x = 24, paste0("R² = ", sprintf("%.2f", round(r2_tt, digits = 2))), adj = -0.01)
      
      x <- 1:35
      y <- 1:35
      abline(lm(x~y), col = "darkblue", lty = 3)
      
    }
    mtext("Species richness (observed)",side=1,line=1,outer=TRUE,cex=1.2)
    mtext("Species richness (predicted)",side=2,line=1,outer=TRUE,cex=1.2,las=0)
    
    dev.off() 
    