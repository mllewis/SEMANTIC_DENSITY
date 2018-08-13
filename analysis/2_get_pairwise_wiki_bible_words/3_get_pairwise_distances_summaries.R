# Get pairwise distance summaries from europarl
library(tidyverse)
library(tidyboot)

#### PARAMETERS ####
PREFIX_PATH <- "file_prefixes.csv"
DISTS_PATH <- "/Volumes/wilbur_the_great/density_models/bible_dists/" # path to csv for writing distances
OUTPUT_PATH <- "bible_distance_summaries.csv"

###

get_dist_summaries <- function(target_lang, dist_path, out_file){
  
  print(target_lang)
  
  dists <- read_csv(paste0(dist_path,"_", target_lang, ".csv"))
  
  dist_summaries <- dists %>%
        tidyboot_mean(column = cos_dist, nboot = 10) %>%
        mutate(median = median(dists$cos_dist),
               sd = sd(dists$cos_dist),
               cv = sd/mean,
               wiki_lang_code = target_lang) 
  
  write_csv(dist_summaries, out_file, append = TRUE)
  
}


### DO THE THING  ####
target_langs  <- read_csv(PREFIX_PATH) %>%
  filter(!is.na(bible_wiki_lang_code))  %>%
  filter(bible_wiki_lang_code != "ig") %>%
  pull(bible_wiki_lang_code)

# loops over languages and saves pairiwse word distances to a single csv
walk(target_langs, 
     get_dist_summaries, 
     DISTS_PATH, 
     OUTPUT_PATH)
