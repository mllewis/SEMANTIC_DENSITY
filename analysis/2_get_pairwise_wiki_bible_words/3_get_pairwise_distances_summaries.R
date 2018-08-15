# Get pairwise distance summaries from europarl
library(tidyverse)
library(tidyboot)

#### PARAMETERS ####
PREFIX_PATH <- "file_prefixes.csv"
DISTS_PATH <- "/Volumes/wilbur_the_great/density_models/bible_dists/5000_words" # path to csv for writing distances
OUTPUT_PATH <- "bible_distance_5000_summaries.csv"

###

get_dist_summaries <- function(target_lang, dist_path, out_file){
  
  print(target_lang)
  
  dists <- read_csv(paste0(dist_path,"_", target_lang, ".csv")) %>%
    select(-language) %>%
    gather()
  
  dist_summaries <- data.frame(n_words = length(dists$value),
                               mean = mean(dists$value),
                               median = median(dists$value),
                               sd = sd(dists$value),
                               wiki_lang_code = target_lang)  %>%
                          mutate(cv = mean/sd)
  
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
