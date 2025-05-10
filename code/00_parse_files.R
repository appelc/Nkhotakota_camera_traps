# Parse and combine predictions files and review summary files from Njobvu-AI

library(tidyverse)


## Model predictions -----------------------------------------------------------

#directory for predictions files
pred_dir <- 'data/predictions/'
pred_files <- list.files(pred_dir, pattern = 'OUT*', full.names = TRUE)

#find max columns in any file
(maxcol = max(unlist(sapply(pred_files, count.fields, sep = ',')))) #find max columns we need

#read in and combine
pred_list <- lapply(pred_files, read.table, sep = ',', header = FALSE, 
                    skip = 1, fill = TRUE, quote = '', col.names = c(1:maxcol))
preds <- as.data.frame(do.call(rbind, pred_list), stringsAsFactors = FALSE)

#delete unnecessary rows (keep only if 'filename' or 'class' is in the row)
preds$keep <- ifelse(grepl('filename', preds$X1), 'y', 
                     ifelse(grepl('class', preds$X1), 'y', 'n'))
preds <- preds[preds$keep %in% 'y',]

#is each row an image or a class prediction?
preds$type <- ifelse(grepl('filename', preds$X1), 'image', 'class')
table(preds$type, useNA = 'always')
imgs <- which(preds[,'type'] %in% 'image') #get indices of 'image' rows

#for each image, count how many 'class' rows occur after it until the next 'image' row
pred_counts <- data.frame(x = seq_along(-imgs), n_boxes = diff(c(imgs, 0L)) - 1L)
pred_counts$index <- imgs #add index
pred_counts$image <- preds[pred_counts$index,]$X1 #use index to add image name

#may need to fix one here (the last image)
table(pred_counts$n_boxes, useNA = 'always') 
tail(pred_counts); tail(preds)
pred_counts[nrow(pred_counts),]$n_boxes <- 0 #manually change here (how many boxes does the last image have?)

#match back up with preds
preds$n_boxes <- pred_counts$n_boxes[match(preds$X1, pred_counts$image)]  

#get just the images and their number of classes
image_names <- preds[preds$type %in% 'image',] 
image_names$name <- sapply(strsplit(as.character(sapply(strsplit(as.character(image_names$X1), '\\/'), '[', 9)), '\\.'), '[', 1)

#does this match up with number of files above?
nrow(preds[preds$type == 'image',])
length(unique(image_names$name)) 

#if not, check duplicates:
dups <- image_names[duplicated(image_names$name),]
unique(dups$name) 

#get just the classes
classes <- preds[preds$type %in% 'class',]
classes$name <- sapply(strsplit(as.character(classes$X2), '\\:'), '[', 2)
classes$score <- sapply(strsplit(as.character(sapply(strsplit(as.character(classes$X7), '\\:'), '[', 2)), '\\}'), '[', 1)
table(classes$name, useNA = 'always')

#now match classes & scores with images
image_preds <- data.frame('image' = rep(image_names$name, image_names$n_boxes),
                          'class' = classes$name, 'score' = classes$score)
#if you get an error that lengths don't match, go back up and check that last image

#how many image/box predictions? these two should match:
nrow(image_preds)
length(rep(image_names$name, image_names$n_boxes))

#how many total images?
length(unique(image_preds$image))  #we're missing some that had n_boxes = 0 (therefore 0 times in rep above)

#add empty images back in (model predicted these were empty)
empty_images <- data.frame('image' = image_names[image_names$n_boxes == 0,]$name, 
                           'class' = 'empty', 
                           'score' = NA)
image_preds <- rbind(image_preds, empty_images)
length(unique(image_preds$image)) #should have them all now

#error checking
table(image_preds$class, useNA = 'always')

#remove "" from class names
image_preds$class <- gsub('"', '', image_preds$class)

#simplify to 1 row per image*class, with scores concatenated
preds_final <- image_preds %>% group_by(image, class) %>%
                               summarise(count = length(class),
                                         scores = paste(score, collapse = ','))
length(unique(preds_final$image))

#add 'filename' column
preds_final$filename <- paste(preds_final$image, '.JPG', sep = '')

#parse info from filenames **OPTIONAL** -- we use the format 'SITE-CAMERA-CHECKDATE__DATE__TIME(BURST)'
#extract site
preds_final$site <- paste(sapply(strsplit(as.character(preds_final$image), '\\-'), '[', 1),
                          sapply(strsplit(as.character(preds_final$image), '\\-'), '[', 2), sep = '-')
table(preds_final$site, useNA = 'a')

#remove any if necessary
preds_final <- preds_final[!preds_final$site %in% c('D1-NA','NA-NA'),]

#format datetime
preds_final$check <- sapply(strsplit(as.character(preds_final$image), '\\__'), '[', 1)
preds_final$date <- sapply(strsplit(as.character(preds_final$image), '\\__'), '[', 2)
preds_final$time <- sapply(strsplit(as.character(sapply(strsplit(as.character(preds_final$image), '\\__'), '[', 3)), '\\('), '[', 1)
preds_final$datetime <- paste(preds_final$date, preds_final$time, sep = ' ')
preds_final$datetime <- as.POSIXct(strptime(preds_final$datetime, '%Y-%m-%d %H-%M-%S'), tz = 'Africa/Blantyre')

#check datetimes
min(preds_final[!is.na(preds_final$datetime),]$datetime)
max(preds_final[!is.na(preds_final$datetime),]$datetime)

#remove 4 images w/ incorrect 2017 date (just setup photos)
View(preds_final[preds_final$datetime < '2018-01-01',])
preds_final <- preds_final[!preds_final$datetime < '2018-01-01',] 

#also remove one 2020 photo from L21-B-2023-10-19 (this was a setup photo)
preds_final <- preds_final[preds_final$image != 'L21-B-2023-10-19__2020-01-01__00-00-15(1)',]

#remove sites from Chipata Mountain (not part of monitoring program)
preds_final <- preds_final[!preds_final$site %in% c('CHI-01','CHI-02'),]

#(end OPTIONAL)

#save
write.csv(preds_final, 'outputs/preds_cleaned.csv')


## Summaries of reviewed projects ----------------------------------------------

#directory for review summary files
review_dir <- 'data/review_summaries/'
review_files <- list.files(review_dir, pattern = '*Summary.txt', full.names = TRUE)

#create list to store output
review_entries <- list()

#iterate through each file
for (rr in review_files){
  rr_lines <- readLines(rr)
  
  #iterate through each line in this file
  for (ll in rr_lines){
    parts <- strsplit(ll, '\t')[[1]]
    filename <- parts[1]
    reviewed <- parts[length(parts)] #1: still has 'needs review' flag; 0: has been reviewed
    
    #if the line has species tags:
    if(length(parts) > 2){
      for (pp in 2:(length(parts) - 1)) {
        split_tag <- strsplit(parts[pp], ': ')[[1]]
        if (length(split_tag) == 2) {
          count <- split_tag[1]
          species <- split_tag[2]
        } else {
          count <- NA
          species <- NA
        }
        review_entries[[length(review_entries) + 1]] <- c(filename, count, species, reviewed)
      }
    } 
    
    #if the line has no species tags:
    else {
      review_entries[[length(review_entries) + 1]] <- c(filename, NA, 'empty', reviewed)
    }
  }
}

#convert to dataframe
review_df <- as.data.frame(do.call(rbind, review_entries), stringsAsFactors = FALSE)
colnames(review_df) <- c('filename', 'count', 'species', 'reviewed')

#parse info from filenames **OPTIONAL** -- we use the format 'SITE-CAMERA-CHECKDATE__DATE__TIME(BURST)'
#extract and pad hexagon names, then format other columns
review_df$hex <- sapply(strsplit(review_df$filename, '\\-'), '[', 1)
review_df$hex <- sprintf('%s%02d', substr(review_df$hex, 1, 1),
                    as.numeric(substr(review_df$hex, 2, nchar(review_df$hex))))
review_df$site <- paste(review_df$hex,
                   sapply(strsplit(review_df$filename, '\\-'), '[', 2), sep = '-')
review_df$check_date <- paste(sapply(strsplit(review_df$filename, '\\-'), '[', 3),
                         sapply(strsplit(review_df$filename, '\\-'), '[', 4),
                         sapply(strsplit(sapply(strsplit(review_df$filename, '\\-'), '[', 5), '\\__'), '[', 1), sep = '-')
review_df$site_check <- paste(review_df$site, review_df$check_date, sep = '-')
review_df$date <- sapply(strsplit(review_df$filename, '\\__'), '[', 2)
review_df$time <- sapply(strsplit(sapply(strsplit(review_df$filename, '\\('), '[', 1), '\\__'), '[', 3)
review_df$datetime <- paste(review_df$date, review_df$time, sep = ' ')
review_df$datetime <- as.POSIXct(strptime(review_df$datetime, '%Y-%m-%d %H-%M-%S'), tz = 'Africa/Blantyre')
#(end OPTIONAL)

#which ones have been reviewed? filter by the 'reviewed' column
table(review_df$reviewed, useNA = 'a')
tags_reviewed <- review_df[tags$reviewed == 0,] #0 means reviewed (1 means it has the "Needs Review" flag)

#view ones not reviewed by class
table(review_df$species, review_df$reviewed)

#to report:
length(unique(review_df[review_df$reviewed == '0',]$filename)) #number images reviewed
length(unique(review_df[review_df$reviewed == '0' & review_df$species == 'empty',]$filename)) #number empty 
length(unique(review_df[review_df$reviewed == '0' & review_df$species != 'empty',]$filename)) #number with animals

#save
write.csv(review_df, 'outputs/summaries_parsed.csv')
write.csv(tags_reviewed, 'outputs/reviewed_images.csv')


## Create 'true' and 'predicted' dataframes for comparison ---------------------

#read back in if necessary
preds_final <- fread('outputs/preds_cleaned.csv')
tags_reviewed <-  fread('outputs/reviewed_images.csv')

#keep only preds for ones that have been reviewed
preds_df <- preds_final[preds_final$filename %in% unique(tags_reviewed$filename),]

#and keep only tags for ones that have predictions (some wrong datetimes here)
true_df <- tags_reviewed[tags_reviewed$filename %in% unique(preds_final$filename),]

#clean up class names: first, calculate max score for each image/species combination
preds_df$max_score <- sapply(strsplit(preds_df$scores, ','), FUN = function(x) max(as.numeric(x), na.rm = TRUE))

#consolidate some species and define correct matches
sort(unique(true_df$species))
sort(unique(preds_df$class))

#consolidate in 'true_df'
true_df$species_cleaned <- ifelse(true_df$species == 'dog', 'domestic_dog', true_df$species)
true_df$species_cleaned <- ifelse(true_df$species == 'jackal', 'side-striped_jackal', true_df$species_cleaned)
true_df$species_cleaned <- ifelse(true_df$species == 'vervet', 'vervet_monkey', true_df$species_cleaned)
true_df$species_cleaned <- ifelse(true_df$species %in% c('duiker','red_duiker','grysbok','klipspringer'), 
                                  'small_antelope', true_df$species_cleaned)

#consolidate in 'preds_df'
preds_df$species_cleaned <- ifelse(preds_df$class == 'dog', 'domestic_dog', preds_df$class)
preds_df$species_cleaned <- ifelse(preds_df$class == 'jackal', 'side-striped_jackal', preds_df$species_cleaned) 
preds_df$species_cleaned <- ifelse(preds_df$class == 'vervet', 'vervet_monkey', preds_df$species_cleaned) 

#review again
unique(true_df$species_cleaned)[!unique(true_df$species_cleaned) %in% unique(preds_df$species_cleaned)]
#in "true" but not predicted: other_bird, other_animal, side-striped jackal, palm_civet, bat_sp, small_mammal

unique(preds_df$species_cleaned)[!unique(preds_df$species_cleaned) %in% unique(true_df$species_cleaned)]
#in "preds" but not evaluation set: domestic_cattle, goat

#save
write.csv(true_df, 'outputs/true_df.csv')
write.csv(preds_df, 'outputs/preds_df.csv')

